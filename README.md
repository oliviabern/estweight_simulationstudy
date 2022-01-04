## Code for the simulation study presented in "Adjustment for Biased Sampling Using NHANES Derived Propensity Weights"

The file create_nhanes_rep.R contains code for cleaning NHANES data and producing the NHANES-REP dataset. NHANES data can be downloaded from https://wwwn.cdc.gov/nchs/nhanes/Default.aspx

The file simstudy.R contains code to reproduce the simulation study and analyze the results. The code uses the snowfall package to run the simulations in parallel. Users can update the number of CPUs depending on how many are available.
