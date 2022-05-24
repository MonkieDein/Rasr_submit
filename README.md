# Rasr_submit
This repo is to store the clean rasr code for supplementary submission

## 0. Requirement
Set the directory to where the folder is located. For 
- Prelim.R
- main.R
- Evaluator.R
- RiskOfEval.R
1. Line 3: setwd("~/\<directory path\>/Rasr_submit")

## 1. Prelim.R
Installation of benchmark data is required
- Run the code and auto-install the [benchmark data](http://data.rmdp.xyz/domains/)
- Read [description](http://data.rmdp.xyz/domains/README.md) of each dataset.

## 2. Main.R
Generate policy for **benchmark algorithms** that is not available in CRAAM
- rasr_erm
- naive_erm
- epis_erm
- chow_cvar

## 3. Evaluator.R
Generate sample instances and deploy policies.

## RiskOfEval.R
Calculate the Risk of Return of the evaluated sample instances.




