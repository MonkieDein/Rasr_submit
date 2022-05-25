# Rasr_submit
Rasr code for supplementary submission. All the results are run on Ubuntu 20.04, **8-core** Intel(R) Core(TM) i7-9700 CPU @ 3.00GHz, 32GB RAM. Note that, some library like **foreach** could require additional work for other OS like (Window 10), it requires manually adding the function to the memory on the slave processors.

### In Code:
- Basic_Utils.R : Utility function that is used in the experiment.
- RASR_code.R : Contains ERM_MDP of all kind of risk aversion.
- PflugCVaR.R : An optimized implementation of Chow's risk-averse DYNAMIC CVaR.
---
- Prelim.R : Install datasets.
- main.R : Generate Policies beyond CRAAM benchmark algorithms.
- Evaluator.R : Evaluate generated policies.
- RiskOfEval.R : Calculate Risk of return given evaluated return distribution for each policy.
---
##### In PlotNTab:
- Barplot.R : Generate Barplot given RiskOfReward
- Table.R : Generate Computation time table and RiskOfReward table
- Histogram.R : Generate Histogram shows distribution of riskAverse algorithm vs NominalMDP

### In Eval/train/<domain>:
CRAAMPolicies.RData : The policies of algorithm available in CRAAM. [row = risk levels, col = states]
- Derman
- RSVF
- RSVI
- BCR

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

Time (secs) to compute [Ubuntu 20.04, **8-core** Intel(R) Core(TM) i7-9700 CPU @ 3.00GHz, 32GB RAM]
| Methods    | Riverswim  | Population   | Inventory   |
| :--------- | ---------: | -----------: | ----------: |
| RASR       | <2         | 24           | <7          |
| Naive      | 27         | 175          | 186         |
| Erik       | 1117       | 110306       | 9977        |
| Chow       | 69         | 861          | 572         |

## 3. Evaluator.R
Generate sample instances with Monte Carlo methods and deploy policies.

- In the main body of the paper, we sample 100,000 instances and time horizon 1,000.
- In the appendix, to reduce time consumption and for reproducibility purposes. We set an arbitrary seed (1), sample only 10,000 instances, and uses only time horizon 500 for the experiment.  
- Note that: The distribution of the evaluation are similar both in the paper and in the appendix.

Time (secs) to compute [Ubuntu 20.04, **8-core** Intel(R) Core(TM) i7-9700 CPU @ 3.00GHz, 32GB RAM]
| Domain     | Riverswim  | Population   | Inventory   |
| :--------- | ---------: | -----------: | ----------: |
| Time       | 2555       | 11021        | 2935        |


## 4. RiskOfEval.R
Calculate the Risk of Return of the evaluated sample instances.

## 5. Generate Plots and Tables
- Barplot.R : Generate Barplot given RiskOfReward
- Table.R : Generate Computation time table and RiskOfReward table
- Histogram.R : Generate Histogram shows distribution of riskAverse algorithm vs NominalMDP
