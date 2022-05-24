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

For reproducibility purposes we set an arbitrary seed (1), sample only 10,000 instances, and uses time horizon 500 for the experiment. In the main body of the paper, we use a random 100,000 instances and time horizon 1,000. Regardless, the trend and information of the solution are similar.

## RiskOfEval.R
Calculate the Risk of Return of the evaluated sample instances.




