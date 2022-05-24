# Rasr_submit
Rasr code for supplementary submission. All the results are run on Ubuntu 20.04, **8-core** Intel(R) Core(TM) i7-9700 CPU @ 3.00GHz, 32GB RAM. Note that, some library like **foreach** could require additional work for other OS like (Window 10), it requires manually adding the function to the memory on the slave processors.

1. In Code:
- Basic_Utils.R : Utility function that is used in the experiment.
- RASR_code.R : Contains ERM_MDP of all kind of risk aversion.
- PflugCVaR.R : An optimized implementation of Chow's risk-averse DYNAMIC CVaR.
---
- Prelim.R : Install datasets.
- main.R : Generate Policies beyond CRAAM benchmark algorithms.
- Evaluator.R : Evaluate generated policies.
- RiskOfEval.R : Calculate Risk of return given evaluated return distribution for each policy.

## 0. Requirement
Set the directory to where the folder is located. For 
- Prelim.R
- main.R
- Evaluator.R
- RiskOfEval.R
1. Line 3: setwd("~/\<directory path\>/Rasr_submit")

2. Make sure your R has all the libraries that is required. Use **install.packages("\<package_name\>")** to install missing packages.

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




