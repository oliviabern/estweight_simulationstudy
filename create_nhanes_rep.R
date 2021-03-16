# Code to create NHANES-REP
# Olivia Bernstein
# 12/16/20

# This code takes data from NHANES 2013-2016
# Subsets down to variables also collected in the C2C dataset
# and aggregates any variables to match granularity in C2C
#
# This code can be adapted to select different variables and 
# adjust the aggregation

#### Select directories (EDIT THESE TO INCLUDE THE CORRECT PATHS) ####

# Path where raw 2015-2016 NHANES data is stored
path.nhanes.1516 = "path_to_where_nhanes_1516_is_stored"
# Path where raw 2013-2014 NHANES data is stored 
path.nhanes.1314 = "path_to_where_nhanes_1314_is_stored"

# Working directory 
path.work = "path_to_where_nhanes_rep_is_stored"

#### Load packages ####
library("tidyverse")
library("SASxport")

#### Convert XPT files to CSV ####

## 2015-2016 files

# demographics file
demo = read.xport(paste0(path.nhanes.1516,"/DEMO_I.XPT"))
write.csv(demo, file = paste0(path.nhanes.1516,"/DEMO.csv"))

# Blood pressure file
bp = read.xport(paste0(path.nhanes.1516,"/BPQ_I.XPT"))
write.csv(bp, file = paste0(path.nhanes.1516,"/BPQ.csv"))

# Diabetes file
diab = read.xport(paste0(path.nhanes.1516,"/DIQ_I.XPT"))
write.csv(diab, file = paste0(path.nhanes.1516,"/DIQ.csv"))

# Kidney file
kiq = read.xport(paste0(path.nhanes.1516,"/KIQ_U_I.XPT"))
write.csv(kiq, file = paste0(path.nhanes.1516,"/KIQ.csv"))

# Medical conditions file
med = read.xport(paste0(path.nhanes.1516,"/MCQ_I.XPT"))
write.csv(med, file = paste0(path.nhanes.1516,"/MCQ.csv"))

# Physical activity file
pa = read.xport(paste0(path.nhanes.1516,"/PAQ_I.XPT"))
write.csv(pa, file = paste0(path.nhanes.1516,"/PAQ.csv"))

# Perscription drugs file
rx = read.xport(paste0(path.nhanes.1516,"/RXQ_RX_I.XPT"))
write.csv(rx, file = paste0(path.nhanes.1516,"/RXQ.csv"))

# Sleep disorders file
slp = read.xport(paste0(path.nhanes.1516,"/SLQ_I.XPT"))
write.csv(slp, file = paste0(path.nhanes.1516,"/SLQ.csv"))

# Depression file
dpq = read.xport(paste0(path.nhanes.1516,"/DPQ_I.XPT"))
write.csv(dpq, file = paste0(path.nhanes.1516,"/DPQ.csv"))

## 2013-2014 files

# demographics file
demo = read.xport(paste0(path.nhanes.1314,"/DEMO_H.XPT"))
write.csv(demo, file = paste0(path.nhanes.1314,"/DEMO.csv"))

# Blood pressure file
bp = read.xport(paste0(path.nhanes.1314,"/BPQ_H.XPT"))
write.csv(bp, file = paste0(path.nhanes.1314,"/BPQ.csv"))

# Diabetes file
diab = read.xport(paste0(path.nhanes.1314,"/DIQ_H.XPT"))
write.csv(diab, file = paste0(path.nhanes.1314,"/DIQ.csv"))

# Kidney file
kiq = read.xport(paste0(path.nhanes.1314,"/KIQ_U_H.XPT"))
write.csv(kiq, file = paste0(path.nhanes.1314,"/KIQ.csv"))

# Medical conditions file
med = read.xport(paste0(path.nhanes.1314,"/MCQ_H.XPT"))
write.csv(med, file = paste0(path.nhanes.1314,"/MCQ.csv"))

# Physical activity file
pa = read.xport(paste0(path.nhanes.1314,"/PAQ_H.XPT"))
write.csv(pa, file = paste0(path.nhanes.1314,"/PAQ.csv"))

# Perscription drugs file
rx = read.xport(paste0(path.nhanes.1314,"/RXQ_RX_H.XPT"))
write.csv(rx, file = paste0(path.nhanes.1314,"/RXQ.csv"))

# Sleep disorders file
slp = read.xport(paste0(path.nhanes.1314,"/SLQ_H.XPT"))
write.csv(slp, file = paste0(path.nhanes.1314,"/SLQ.csv"))

# Depression file
dpq = read.xport(paste0(path.nhanes.1314,"/DPQ_H.XPT"))
write.csv(dpq, file = paste0(path.nhanes.1314,"/DPQ.csv"))

#### Select variables from 2015-2016 ####
## Can download data from the website
# need to convert to CSV
# https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2015

## Load 2015-2016 data
demo.1516 <- read.csv(paste0(path.nhanes.1516,"/DEMO.csv"), row.names=1)
bpq.1516 <- read.csv(paste0(path.nhanes.1516,"/BPQ.csv"), row.names=1)
diq.1516 <- read.csv(paste0(path.nhanes.1516,"/DIQ.csv"), row.names=1)
kiq.1516 <- read.csv(paste0(path.nhanes.1516,"/KIQ.csv"), row.names=1)
mcq.1516 <- read.csv(paste0(path.nhanes.1516,"/MCQ.csv"), row.names=1)
paq.1516 <- read.csv(paste0(path.nhanes.1516,"/PAQ.csv"), row.names=1)
rxq.1516 <- read.csv(paste0(path.nhanes.1516,"/RXQ.csv"), row.names=1)
slq.1516 <- read.csv(paste0(path.nhanes.1516,"/SLQ.csv"), row.names=1)
dpq.1516 <- read.csv(paste0(path.nhanes.1516,"/DPQ.csv"), row.names=1)

## Select demographic variables
demo1.1516 = demo.1516 %>% dplyr::select(SEQN, WTINT2YR, WTMEC2YR, DMDEDUC3, DMDEDUC2, 
                               RIDAGEYR, RIAGENDR, RIDRETH3) %>% 
  dplyr::rename(HT_wt = WTMEC2YR, # This uses the weight for measurements collected at the Mobile Examination Center
                age = RIDAGEYR) %>% 
  mutate(female = ifelse(RIAGENDR == 2, 1, 0),
         edu_youth = ifelse(DMDEDUC3 %in% c(13,14),"highschool",
                            ifelse(DMDEDUC3 == 15, "somecollege",
                                   ifelse(DMDEDUC3 %in% c(0:12,55,66),"lessthan12",NA))),
         edu_adult = ifelse(DMDEDUC2 == 3, "highschool", 
                            ifelse(DMDEDUC2 == 4, "somecollege",
                                   ifelse(DMDEDUC2 ==5, "collegegrad", 
                                          ifelse(DMDEDUC2 %in% 1:2, "lessthan12",NA)))),
         edu = ifelse(!is.na(edu_youth),edu_youth,edu_adult), # no one has an entry for both the youth and adult variables
         race_eth = recode(as.character(RIDRETH3), '1' = "Hispanic", '2' = "Hispanic", '3' = "NHWhite", '4' = "NHBlack",
                           '6' = "NHAsian", '7' = "Other")) %>% 
  dplyr::select(SEQN, HT_wt, age, female, edu, race_eth)

## Select blood presure variables
bpq1.1516 = bpq.1516 %>% dplyr::select(SEQN, BPQ020, BPQ030) %>% 
  mutate(highBP = ifelse(BPQ020 == 1, 1, 
                         ifelse(BPQ020 == 2, 0, NA))) %>% 
  dplyr::select(SEQN, highBP)

## Select diabetes variables
diq1.1516 = diq.1516 %>% dplyr::select(SEQN, DIQ010, DIQ160) %>% 
  mutate(diabetes = as.numeric(recode(DIQ010, '1'= '1', '2' = '0', .default = NA_character_))) %>% 
  dplyr::select(SEQN, diabetes)

## Select kidney variables
kiq1.1516 = kiq.1516 %>% dplyr::select(SEQN, KIQ022) %>% 
  mutate(kidney = as.numeric(recode(KIQ022, '1' = '1', '2' = '0', .default = NA_character_))) %>% 
  dplyr::select(SEQN, kidney)

## Select medical history questions
mcq1.1516 = mcq.1516 %>% dplyr::select(SEQN, MCQ160L,	MCQ160B, MCQ160C, MCQ160E, MCQ220) %>% 
  mutate(liver = as.numeric(recode(MCQ160L, '1' = '1','2' = '0', .default = NA_character_)), 
         congheartfailure = as.numeric(recode(MCQ160B, '1' = '1','2' = '0', .default = NA_character_)),
         coronaryheartdisease= as.numeric(recode(MCQ160C, '1' = '1','2' = '0', .default = NA_character_)),
         CHD = as.numeric(congheartfailure | coronaryheartdisease),
         cancer = as.numeric(recode(MCQ220, '1' = '1','2' = '0', .default = NA_character_)))  %>% 
  dplyr::select(SEQN, liver, CHD, cancer)

## Select sleep hours
slq1.1516 = slq.1516 %>% dplyr::select(SEQN, SLD012) %>% 
  dplyr::rename(sleep = SLD012) %>% 
  mutate(sleep = ifelse(sleep==99,NA,sleep))

## Select physical activity info
# exercise is an indicator whether subjects report participating in
# vigorous recreational activity or moderate rec. activity
# If subjects have a 1 for vigorous or moderate recreational activity: exercise == 1
# If subject has 2 NA's OR a 0 & an NA in vigorous or moderate: exercise == NA
# If subjects has a 0 for both vigorous or moderate: exercise == 0
paq1.1516 = paq.1516 %>% dplyr::select(SEQN,PAQ650, PAQ665) %>% 
  mutate(vigrec = as.numeric(recode(PAQ650, '1' = '1','2' = '0', .default = NA_character_)), # vigorous recreational activity
         modrec = as.numeric(recode(PAQ665, '1' = '1','2' = '0', .default = NA_character_)), # moderate recreational activity
         exercise = ifelse(vigrec == 1 | modrec == 1,1,0)) %>% 
  dplyr::select(SEQN, exercise)

## Select depression info
# Total score is considered missing missing if more than 1 item is missing
dpq1.1516 = dpq.1516
dpq1.1516$nNA = rowSums(is.na(dpq1.1516[,-1]))
dpq1.1516$totalscore = rowSums(dpq1.1516[,2:11],na.rm = TRUE)
dpq2.1516 = dpq1.1516 %>%
  mutate(twoplusNA = nNA > 1,
         totalscore = ifelse(twoplusNA,NA,totalscore),
         majordep = (totalscore >= 10)) %>% 
  dplyr::select(SEQN, majordep)

## Select perscription drug info
# Each subject has an indicator of prescription drug use and a list of which drugs they reported taking
# We want to create an indicator of using prescription drugs that does not include subjects who only report OTC use

# Get reported prescription drug use indicator (presc_reported)
# RXDUSE: Taken prescription drugs, past month
# RXDUSE == 9: don't know, RXDUSE == 7: refused to answer
# These responses will become NA's
rxq1.1516 = rxq.1516 %>% dplyr::select(SEQN, RXDUSE) %>% distinct() %>% 
  dplyr::rename(presc_reported = RXDUSE) %>% 
  mutate(presc_reported = as.numeric(recode(presc_reported, '1' = '1','2' = '0', .default = NA_character_)))

# Identify which subjects only report OTC drugs and replace presc_reported with a 0
# List of commonly reported OTC drugs
toremove = c("IBUPROFEN", "FLUTICASONE NASAL", "CETIRIZINE", "POTASSIUM CHLORIDE", 
             "ESOMEPRAZOLE", "NAPROXEN", "ASPIRIN", "TRIAMCINOLONE NASAL", "LORATADINE; PSEUDOEPHEDRINE")

# Ientify which subjects report prescription drugs after removing OTC drugs
presc.1516 = subset(rxq.1516,RXDUSE==1, select = c("SEQN","RXDDRUG"))
presc.1516$KEEP = ifelse(presc.1516$RXDDRUG %in% toremove, 0, 1)
presctomerge.1516 = presc.1516 %>% dplyr::select(SEQN, KEEP) %>% # get one row per subject
  group_by(SEQN) %>% 
  mutate(presc_final = max(KEEP)) %>%
  ungroup() %>% dplyr::select(SEQN,presc_final) %>% 
  distinct() 

# merge prescription drug indicator with list of subjects who reported drug history
rxq2.1516 = merge(rxq1.1516, presctomerge.1516, by = "SEQN",all = TRUE)

# replace presc_reported with 0 for subjects who reported 1 but only listed OTC drugs
# else use presc_reported as final response
rxq3.1516 = rxq2.1516 %>% 
  mutate(presc_noNA = replace_na(presc_final, " "),
         presc_comb = ifelse(presc_noNA == 0, 0, presc_reported)) %>% 
  dplyr::select(SEQN, presc_comb) %>% 
  dplyr::rename(prescription = presc_comb)



## create combined NHANES dataset
nh1.1516 = merge(demo1.1516, bpq1.1516, by = "SEQN", all = TRUE)
nh2.1516 = merge(nh1.1516, diq1.1516, by = "SEQN", all = TRUE)  
nh3.1516 = merge(nh2.1516, kiq1.1516, by = "SEQN", all = TRUE)
nh4.1516 = merge(nh3.1516, mcq1.1516, by = "SEQN", all = TRUE)
nh5.1516 = merge(nh4.1516, paq1.1516, by = "SEQN", all = TRUE)
nh6.1516 = merge(nh5.1516, slq1.1516, by = "SEQN", all = TRUE)
nh7.1516 = merge(nh6.1516, dpq2.1516, by = "SEQN", all = TRUE)
nh8.1516 = merge(nh7.1516, rxq3.1516, by = "SEQN", all = TRUE)

# subset to subjects older than 50 to match eligibility requirements of C2C
nh9.1516 = nh8.1516 %>% subset(age >= 50) 
# get complete cases
nh.cc.1516 = nh9.1516[complete.cases(nh9.1516),]


#### Select variables from 2013-2014 ####
## Can download data from the website
# need to convert to CSV
# https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2013

## Load 2013-2014 data
demo.1314 <- read.csv(paste0(path.nhanes.1314,"/DEMO.csv"), row.names=1)
bpq.1314 <- read.csv(paste0(path.nhanes.1314,"/BPQ.csv"), row.names=1)
diq.1314 <- read.csv(paste0(path.nhanes.1314,"/DIQ.csv"), row.names=1)
kiq.1314 <- read.csv(paste0(path.nhanes.1314,"/KIQ.csv"), row.names=1)
mcq.1314 <- read.csv(paste0(path.nhanes.1314,"/MCQ.csv"), row.names=1)
paq.1314 <- read.csv(paste0(path.nhanes.1314,"/PAQ.csv"), row.names=1)
rxq.1314 <- read.csv(paste0(path.nhanes.1314,"/RXQ.csv"), row.names=1)
slq.1314 <- read.csv(paste0(path.nhanes.1314,"/SLQ.csv"), row.names=1)
dpq.1314 <- read.csv(paste0(path.nhanes.1314,"/DPQ.csv"), row.names=1)

## Select demographic variables
demo1.1314 = demo.1314 %>% dplyr::select(SEQN, WTINT2YR, WTMEC2YR, DMDEDUC3, DMDEDUC2, 
                                         RIDAGEYR, RIAGENDR, RIDRETH3) %>% 
  dplyr::rename(HT_wt = WTMEC2YR, # This uses the weight for measurements collected at the Mobile Examination Center
                age = RIDAGEYR) %>% 
  mutate(female = ifelse(RIAGENDR == 2, 1, 0),
         edu_youth = ifelse(DMDEDUC3 %in% c(13,14),"highschool",
                            ifelse(DMDEDUC3 == 15, "some college",
                                   ifelse(DMDEDUC3 %in% c(0:12,55,66),"less than 12",NA))),
         edu_adult = ifelse(DMDEDUC2 == 3, "highschool", 
                            ifelse(DMDEDUC2 == 4, "somecollege",
                                   ifelse(DMDEDUC2 ==5, "collegegrad", 
                                          ifelse(DMDEDUC2 %in% 1:2, "lessthan12",NA)))),
         edu = ifelse(!is.na(edu_youth),edu_youth,edu_adult), # no one has an entry for both the youth and adult variables
         race_eth = recode(as.character(RIDRETH3), '1' = "Hispanic", '2' = "Hispanic", '3' = "NHWhite", '4' = "NHBlack",
                           '6' = "NHAsian", '7' = "Other")) %>% 
  dplyr::select(SEQN, HT_wt, age, female, edu, race_eth)

## Select blood presure variables
bpq1.1314 = bpq.1314 %>% dplyr::select(SEQN, BPQ020, BPQ030) %>% 
  mutate(highBP = ifelse(BPQ020 == 1, 1, 
                         ifelse(BPQ020 == 2, 0, NA))) %>% 
  dplyr::select(SEQN, highBP)

## Select diabetes variables
diq1.1314 = diq.1314 %>% dplyr::select(SEQN, DIQ010, DIQ160) %>% 
  mutate(diabetes = as.numeric(recode(DIQ010, '1'= '1', '2' = '0', .default = NA_character_))) %>% 
  dplyr::select(SEQN, diabetes)

## Select kidney variables
kiq1.1314 = kiq.1314 %>% dplyr::select(SEQN, KIQ022) %>% 
  mutate(kidney = as.numeric(recode(KIQ022, '1' = '1', '2' = '0', .default = NA_character_))) %>% 
  dplyr::select(SEQN, kidney)

## Select medical history questions
mcq1.1314 = mcq.1314 %>% dplyr::select(SEQN, MCQ160L,	MCQ160B, MCQ160C, MCQ160E, MCQ220) %>% 
  mutate(liver = as.numeric(recode(MCQ160L, '1' = '1','2' = '0', .default = NA_character_)), 
         congheartfailure = as.numeric(recode(MCQ160B, '1' = '1','2' = '0', .default = NA_character_)),
         coronaryheartdisease= as.numeric(recode(MCQ160C, '1' = '1','2' = '0', .default = NA_character_)),
         CHD = as.numeric(congheartfailure | coronaryheartdisease),
         cancer = as.numeric(recode(MCQ220, '1' = '1','2' = '0', .default = NA_character_)))  %>% 
  dplyr::select(SEQN, liver, CHD, cancer)

## Select sleep hours (different variable name from 2015-2016)
slq1.1314 = slq.1314 %>% dplyr::select(SEQN, SLD010H) %>% 
  dplyr::rename(sleep = SLD010H) %>% 
  mutate(sleep = ifelse(sleep==99,NA,sleep))

## Select physical activity info
# exercise is an indicator whether subjects report participating in
# vigorous recreational activity or moderate rec. activity
# If subjects have a 1 for vigorous or moderate recreational activity: exercise == 1
# If subject has 2 NA's OR a 0 & an NA in vigorous or moderate: exercise == NA
# If subjects has a 0 for both vigorous or moderate: exercise == 0
paq1.1314 = paq.1314 %>% dplyr::select(SEQN,PAQ650, PAQ665) %>% 
  mutate(vigrec = as.numeric(recode(PAQ650, '1' = '1','2' = '0', .default = NA_character_)), # vigorous recreational activity
         modrec = as.numeric(recode(PAQ665, '1' = '1','2' = '0', .default = NA_character_)), # moderate recreational activity
         exercise = ifelse(vigrec == 1 | modrec == 1,1,0)) %>% 
  dplyr::select(SEQN, exercise)

## Select depression info
# Total score is considered missing missing if more than 1 item is missing
dpq1.1314 = dpq.1314
dpq1.1314$nNA = rowSums(is.na(dpq1.1314[,-1]))
dpq1.1314$totalscore = rowSums(dpq1.1314[,2:11],na.rm = TRUE)
dpq2.1314 = dpq1.1314 %>%
  mutate(twoplusNA = nNA > 1,
         totalscore = ifelse(twoplusNA,NA,totalscore),
         majordep = (totalscore >= 10)) %>% 
  dplyr::select(SEQN, majordep)

## Select perscription drug info
# Each subject has an indicator of prescription drug use and a list of which drugs they reported taking
# We want to create an indicator of using prescription drugs that does not include subjects who only report OTC use

# Get reported prescription drug use indicator (presc_reported)
# RXDUSE: Taken prescription drugs, past month
# RXDUSE == 9: don't know, RXDUSE == 7: refused to answer
# These responses will become NA's
rxq1.1314 = rxq.1314 %>% dplyr::select(SEQN, RXDUSE) %>% distinct() %>% 
  dplyr::rename(presc_reported = RXDUSE) %>% 
  mutate(presc_reported = as.numeric(recode(presc_reported, '1' = '1','2' = '0', .default = NA_character_)))

# Ientify which subjects report prescription drugs after removing OTC drugs
presc.1314 = subset(rxq.1314,RXDUSE==1, select = c("SEQN","RXDDRUG"))
presc.1314$KEEP = ifelse(presc.1314$RXDDRUG %in% toremove, 0, 1)
presctomerge.1314 = presc.1314 %>% dplyr::select(SEQN, KEEP) %>% # get one row per subject
  group_by(SEQN) %>% 
  mutate(presc_final = max(KEEP)) %>%
  ungroup() %>% dplyr::select(SEQN,presc_final) %>% 
  distinct() 

# merge prescription drug indicator with list of subjects who reported drug history
rxq2.1314 = merge(rxq1.1314, presctomerge.1314, by = "SEQN",all = TRUE)

# replace presc_reported with 0 for subjects who reported 1 but only listed OTC drugs
# else use presc_reported as final response
rxq3.1314 = rxq2.1314 %>% 
  mutate(presc_noNA = replace_na(presc_final, " "),
         presc_comb = ifelse(presc_noNA == 0, 0, presc_reported)) %>% 
  dplyr::select(SEQN, presc_comb) %>% 
  dplyr::rename(prescription = presc_comb)



## create combined NHANES dataset
nh1.1314 = merge(demo1.1314, bpq1.1314, by = "SEQN", all = TRUE)
nh2.1314 = merge(nh1.1314, diq1.1314, by = "SEQN", all = TRUE)  
nh3.1314 = merge(nh2.1314, kiq1.1314, by = "SEQN", all = TRUE)
nh4.1314 = merge(nh3.1314, mcq1.1314, by = "SEQN", all = TRUE)
nh5.1314 = merge(nh4.1314, paq1.1314, by = "SEQN", all = TRUE)
nh6.1314 = merge(nh5.1314, slq1.1314, by = "SEQN", all = TRUE)
nh7.1314 = merge(nh6.1314, dpq2.1314, by = "SEQN", all = TRUE)
nh8.1314 = merge(nh7.1314, rxq3.1314, by = "SEQN", all = TRUE)

# subset to subjects older than 50 to match eligibility requirements of C2C
nh9.1314 = nh8.1314 %>% subset(age >= 50) 
# get complete cases
nh.cc.1314 = nh9.1314[complete.cases(nh9.1314),]

#### Create combined NHANES-REP dataset for 2013-2016 ####

# save complete case dataset
nh.cc = rbind(nh.cc.1314, nh.cc.1516) 
write.csv(nh.cc, paste0(path.work,"/nhanes.csv"))

## Replicate subjects by frequency weight
# NHANES instructs users to divide each sampling weight by 2 when combining 2 cycles of the survey
# This is not needed because I am going to divide by the smallest weight

# divide by the smallest sampling weight and round up
nh.cc$HT_wt_norm = ceiling(nh.cc$HT_wt/min(nh.cc$HT_wt))

# duplicate rows
rep.indx = rep(1:nrow(nh.cc), times = nh.cc$HT_wt_norm)
n.copies = unlist(apply(matrix(nh.cc$HT_wt_norm, nrow = 1),2,seq))
nhanes_rep = nh.cc[rep.indx,] %>% 
  mutate(SEQN = paste0(SEQN,"_",n.copies)) %>% 
  dplyr::select(-HT_wt, -HT_wt_norm)

save(nhanes_rep, file = paste0(path.work,"/nhanes_rep.RData"))
write.csv(nhanes_rep, file = paste0(path.work,"/nhanes_rep.csv"),
          row.names = FALSE)



