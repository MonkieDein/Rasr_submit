# Rasr_submit
This repo is to store the clean rasr code for supplementary submission

## Requirement
Set the directory to where the folder is located. For 
- Prelim.R
- main.R
- Evaluator.R
- RiskOfEval.R
1. Line 3: setwd("~/<directory path>/Rasr_submit")

## Prelim.R
Installation of benchmark data is requred
1. Run the code and auto-install the [benchmark data](http://data.rmdp.xyz/domains/)
2. Read [description](http://data.rmdp.xyz/domains/README.md) of each dataset.

## Main.R
Generate policy for **benchmark algorithms** that is not available in CRAAM
1. rasr_erm
2. naive_erm
3. epis_erm
4. chow_cvar

## Evaluator.R
Generate sample instances and deploy policies.

## RiskOfEval.R
Calculate the Risk of Return of the evaluated sample instances.




