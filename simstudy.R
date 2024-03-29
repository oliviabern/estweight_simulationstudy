# Simulation study to assess the bias and variance estimates
# Olivia Bernstein
# 3/23/21

#### Load packages ####
library("tidyverse")
library("survey")
library("snowfall")
library("rlecuyer")
library("randomForest")
library("entbal") # devtools::install_github("bvegetabile/entbal")
library("CBPS")
library("splitstackshape")
library("estweight") # devtools::install_github("oliviabern/estweight")

#### Load data ####

# Working directory (EDIT TO INCLUDE THE CORRECT PATH)
path.work = "path_to_where_nhanes_rep_is_stored"

# load data
load(paste0(path.work,"/nhanes_rep.RData"))


#### Simulations parameters ####

# functions
expit = function(x){exp(x)/(1+exp(x))}

# sample sizes 
n.r = 500
n.b = 500

# simulation size
n.sim = 1000
n.boot = 200

# Calculate probability of being oversampled and true HT weight
sampprob = expit(.5*(nhanes_rep$female*.3 + (nhanes_rep$edu=="highschool")*.5 + (nhanes_rep$edu=="lessthan12")*.2 + (nhanes_rep$edu=="somecollege")*.8 + 
                       (nhanes_rep$race_eth=="Hispanic")*1.7 + (nhanes_rep$race_eth=="NHAsian")*.9 + (nhanes_rep$race_eth=="NHAsian")*(nhanes_rep$race_eth=="somecollege")*2 + 
                       (nhanes_rep$race_eth=="NHBlack")*.1 + (nhanes_rep$race_eth=="NHBlack")*nhanes_rep$exercise*1.5 + 
                       -.002*nhanes_rep$age^2 + 8))
nhanes_rep$sampprob = sampprob
nhanes_rep$HTweight_true = (1/sampprob)/sum(1/sampprob)

# Calculation E(Y) where Y is the response
nhanes_rep$Ey = expit(1 + log(2)*(nhanes_rep$race_eth=="Hispanic") + -log(3)*(nhanes_rep$race_eth=="NHAsian") + log(1.5)*(nhanes_rep$race_eth=="NHBlack") + 
                        -log(2)*sampprob + log(2)*(nhanes_rep$race_eth=="Hispanic")*sampprob + 
                        log(4)*(nhanes_rep$race_eth=="NHAsian")*sampprob + -log(3)*(nhanes_rep$race_eth=="NHBlack")*sampprob)

# Remove subjects with race/ethnicity of "Other"
nh = subset(nhanes_rep,race_eth != "Other")

n.nh = nrow(nh)


sim = function(simcount){
  b.samp.id = sample(1:n.nh, n.b, prob = nh$sampprob)
  r.samp.id = sample(1:n.nh, n.r)
  
  b.samp = nh[b.samp.id,]
  r.samp = nh[r.samp.id,]
  
  # simulate response
  b.samp$y = rbinom(n.b,1,b.samp$Ey)
  r.samp$y = rbinom(n.r,1,r.samp$Ey)
  
  b.samp$biased = 1; r.samp$biased = 0
  
  # estimate HT weight with logistic regression
  Xcomb = data.frame(ID = 1:(n.b + n.r),
                     rbind(b.samp, r.samp))
  
  # Dataframe to pass to function
  Xfit = Xcomb[,c("ID","age","female","edu","race_eth","highBP","diabetes","kidney","liver","CHD",
                  "cancer", "exercise","sleep","majordep","prescription","biased")]
  # convert strings and indicators to factors
  convert2factor = c("female","edu","race_eth","highBP","diabetes","kidney","liver","CHD","cancer",
                     "exercise","majordep","prescription")
  for(i in 1:length(convert2factor)){
    Xfit[,convert2factor[i]] = as.factor(Xfit[,convert2factor[i]])
  }
  
  # relevel race_eth variable
  Xfit$race_eth = factor(Xfit$race_eth, levels = c("NHWhite","Hispanic","NHAsian","NHBlack"))
  
  # get response
  response = Xcomb[Xcomb$biased==1,c("ID","y")]
  
  # formulas for final scientific model
  form.outcome = as.formula(y ~ race_eth)
  
  ##### Estimate weights and fit weighted outcome model ####
  
  ## logistic regression to estimate weights
  est.log = convGLM(data = Xfit, outcome_formula = form.outcome, response = response,
                 weight_model = "logistic", outcome_family = "quasibinomial")
  
  ## random forest to estimate weights
  est.rf = convGLM(data = Xfit, outcome_formula = form.outcome, response = response,
                         weight_model = "randomForest", outcome_family = "quasibinomial")
  
  ## entbal to estimate weights
  est.eb = convGLM(data = Xfit, outcome_formula = form.outcome, response = response,
                   weight_model = "entbal", outcome_family = "quasibinomial")
  
  ## CBPS to estimate weights (balancing on 2 moments)
  est.cbps = convGLM(data = Xfit, outcome_formula = form.outcome, response = response,
                     weight_model = "CBPS", outcome_family = "quasibinomial")
  
  ## Fit other outcome models of interest
  Xcomb$race_eth = factor(Xcomb$race_eth, levels = c("NHWhite","Hispanic","NHAsian","NHBlack"))
  
  # fit marginal model in representative sample
  fit.rep = glm(form.outcome, family = binomial, data = Xcomb, 
                subset = biased == 0)
  beta.rep = fit.rep$coefficients
  
  # fit marginal model in biased sample (unweighted)
  fit.bias.unwt = glm(form.outcome, family = binomial, data = Xcomb,
                      subset = biased == 1)
  beta.bias.unwt = fit.bias.unwt$coefficients
  
  fit.bias.truewt = svyglm(form.outcome,
                           design = svydesign(ids = ~0, weights = Xcomb$HTweight_true, data = Xcomb),
                           family = quasibinomial, subset = biased == 1)
  beta.bias.truewt = fit.bias.truewt$coefficients
  
  
  #### Get bootstrapped estimate of standard error
  beta.bias.log.boot <- beta.bias.rf.boot <- beta.bias.eb.boot <- beta.bias.cbps.boot <- matrix(NA, nrow = n.boot, ncol = 4)
  
  
  for(b in 1:n.boot){
    # stratify bootstrap sample by race/ethnicity
    group = Xcomb$race_eth
    size = as.vector(table(group))
    names(size) = names(table(group))

    samp = stratified(data.frame(rowid = 1:(n.r+n.b), group = group),
                           group = "group",
                           size = size,replace = TRUE)
    boot.index = samp$rowid 
    
    # Use commented code to obtain a simple random sample for bootstrap
    # boot.index = sample(1:(n.r+n.b), (n.r+n.b), replace = TRUE) 
    
    Xcomb.boot = Xcomb[boot.index,]
    Xfit.boot = Xcomb.boot[,c("ID","age","female","edu","race_eth","highBP","diabetes","kidney","liver","CHD",
                    "cancer","exercise","sleep","majordep","prescription","biased")]
    # convert strings and indicators to factors
    for(i in 1:length(convert2factor)){
      Xfit.boot[,convert2factor[i]] = as.factor(Xfit.boot[,convert2factor[i]])
    }
    
    # relevel race_eth variable
    Xfit.boot$race_eth = factor(Xfit.boot$race_eth, levels = c("NHWhite","Hispanic","NHAsian","NHBlack"))
    
    # get response
    response.boot= Xcomb.boot[Xcomb.boot$biased==1,c("ID","y")]
    
    ## logistic regression to estimate weights
    est.log.boot = convGLM(data = Xfit.boot, outcome_formula = form.outcome, response = response.boot,
                      weight_model = "logistic", outcome_family = "quasibinomial")
    
    ## random forest to estimate weights
    est.rf.boot = convGLM(data = Xfit.boot, outcome_formula = form.outcome, response = response.boot,
                     weight_model = "randomForest", outcome_family = "quasibinomial")
    
    ## entbal to estimate weights
    est.eb.boot = convGLM(data = Xfit.boot, outcome_formula = form.outcome, response = response.boot,
                     weight_model = "entbal", outcome_family = "quasibinomial")
    
    ## CBPS to estimate weights (balancing on 2 moments)
    est.cbps.boot = convGLM(data = Xfit.boot, outcome_formula = form.outcome, response = response.boot,
                       weight_model = "CBPS", outcome_family = "quasibinomial")
    
    ## save coefficients
    beta.bias.log.boot[b,] = est.log.boot[,"coef"]
    beta.bias.rf.boot[b,] = est.rf.boot[,"coef"]
    beta.bias.eb.boot[b,] = est.eb.boot[,"coef"]
    beta.bias.cbps.boot[b,] = est.cbps.boot[,"coef"]
    
  }
  
  out = list(est.log = est.log,
             est.rf = est.rf,
             est.eb = est.eb,
             est.cbps = est.cbps,
             beta.rep = beta.rep,
             beta.bias.unwt = beta.bias.unwt,
             beta.bias.truewt = beta.bias.truewt,
             beta.boot.log = beta.bias.log.boot,
             beta.boot.rf = beta.bias.rf.boot,
             beta.boot.eb = beta.bias.eb.boot,
             beta.boot.cbps = beta.bias.cbps.boot)
  
  out
}


sfInit(parallel = TRUE,cpus = 40) # edit number of CPUS based on availability
sfExport("sim")
sfExport("expit")
sfExport("n.r")
sfExport("n.b")
sfExport("n.nh")
sfExport("n.boot")
sfExport("nh")
sfLibrary(tidyverse)
sfLibrary(survey)
sfLibrary(randomForest)
sfLibrary(entbal)
sfLibrary(CBPS)
sfLibrary(estweight)
sfLibrary(splitstackshape)
sfClusterSetupRNG(type="RNGstream", seed = 2012688935)

start = Sys.time()

simresults = sfLapply(1:n.sim,sim)

end = Sys.time()
end-start

save(simresults, file = paste0(path.work,"/simresults.RData"))


sfStop()

#### Create Results Tables ####
library(kableExtra)

#### Mean coefficient estimates (percent bias relative to estimate in SRS)

mean.coef.comb = function(results, colname){
  apply(do.call("rbind",lapply(results,function(x){x[[colname]][,"coef"]})),2,mean)
}

mean.coef = function(results, colname){
  apply(do.call("rbind",lapply(results,function(x){x[[colname]]})),2,mean)
}


pt.out = round(cbind(mean.coef(simresults,"beta.rep"),
               mean.coef(simresults,"beta.bias.unwt"),
               mean.coef(simresults,"beta.bias.truewt"),
               mean.coef.comb(simresults,"est.log"),
               mean.coef.comb(simresults,"est.cbps"),
               mean.coef.comb(simresults,"est.eb"),
               mean.coef.comb(simresults,"est.rf")),3)

colnames(pt.out) = c("NHNone","C2CNone","True", "Logistic","CBPS","EB", "RF")

# Add percent bias
pbias = function(tab, colname){
  (tab[,colname] - tab[,"NHNone"])/tab[,"NHNone"]
}

pbias.out = cbind(rep("",4),
                  round(do.call("cbind",lapply(colnames(pt.out)[-1],function(x){pbias(pt.out,x)})),2))

# combine estimates and % bias
pt.out1 = cbind(pt.out[,1],
                matrix(paste0(pt.out[,-1]," (",pbias.out[,-1],")"), nrow = 4))



rownames(pt.out1) = c("Intercept", "Hispanic", "NH Asian", "NH Black")

colnames(pt.out1) = c("No weights","No weights","True weights", "Logistic","CBPS","EB", "RF")

# format the table for Latex
k = kable(pt.out1, format = 'latex', booktabs = TRUE,
          caption = NULL)%>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

add_header_above(k, c(" "=1,"SRS" = 1,
                      "Biased Sample" = 6))

#### Standard Error Estimates

# Get empirical SE and mean analytic SE
SE.est = function(results, colname){
  # empirical SE
  empirical = apply(do.call("rbind",lapply(results,function(x){x[[colname]][,"coef"]})),2,sd)
  # mean analytic SE
  analytic = apply(do.call("rbind",lapply(results,function(x){x[[colname]][,"analyticSE"]})),2,mean)
  cbind(empirical, analytic)
}

# Get mean BS estimate of SE (remove any unestimable coefficients: magnitude over 100)
bs.est = function(results, colname){
  apply(do.call("rbind",
                lapply(results,function(x){
                  bs = x[[colname]]
                  repbig = ifelse(abs(bs)>1000,NA,bs)
                  apply(repbig,2,function(z){
                    sd(z,na.rm = TRUE)
                  })})),2,mean)}

# SE in representative sample
SE.rep = apply(do.call("rbind",lapply(simresults,function(x){x[["beta.rep"]]})),2,sd)
SE.bias.unweight = apply(do.call("rbind",lapply(simresults,function(x){x[["beta.bias.unwt"]]})),2,sd)
SE.bias.truweight = apply(do.call("rbind",lapply(simresults,function(x){x[["beta.bias.truewt"]]})),2,sd)

se.out = round(cbind(SE.rep,SE.bias.unweight,SE.bias.truweight,
                SE.est(simresults, "est.log"),
                bs.est(simresults, "beta.boot.log"),
                SE.est(simresults, "est.cbps"),
                bs.est(simresults, "beta.boot.cbps"),
                SE.est(simresults, "est.eb"),
                bs.est(simresults, "beta.boot.eb"),
                SE.est(simresults, "est.rf"),
                bs.est(simresults, "beta.boot.rf")),2)

colnames(se.out) = c(rep("Emp.",3),rep(c("Emp.","Analytic","BS"),4))
rownames(se.out) = c("Intercept", "Hispanic", "NH Asian", "NH Black")

k = kable(se.out, format = 'latex', booktabs = TRUE,
          caption = NULL)%>%
  kable_styling(latex_options = c("hold_position", "scale_down"))

k1 = add_header_above(k, c(" "=1,"No weights","No weights", "True weights","Logistic" = 3,
                      "CBPS" = 3,
                      "Entropy Balancing" = 3,
                      "Random Forest" = 3))

add_header_above(k1, c(" "=1,"SRS","Biased Sample" = 14))
