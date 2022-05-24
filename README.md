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
Generate sample instances with Monte Carlo methods and deploy policies.

- In the main body of the paper, we sample 100,000 instances and time horizon 1,000.
- In the appendix, to reduce time consumption and for reproducibility purposes. We set an arbitrary seed (1), sample only 10,000 instances, and uses only time horizon 500 for the experiment.  
- Note that: The distribution of the evaluation are similar both in the paper and in the appendix.

## RiskOfEval.R
Calculate the Risk of Return of the evaluated sample instances.




