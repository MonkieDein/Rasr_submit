# Set working directory as the main folder of the code
# Change to your respective directory location
setwd("~/Documents/GitHub/Rasr_submit")
source("Code/Basic_Utils.R")

TestFold = wdir(paste0(wdir("Eval/"),"test/"))

domains = c("inventory","riverswim") # ,"population"

if (!file.exists(paste0(TestFold,domain,"/evaluation.RData"))){
  stop("Evaluation file does not exist please run Evaluator.R")
} else{
  load(file = paste0(TestFold,domain,"/evaluation.RData"))
  
  L = sapply(0:4000,function(l) exp(20)*0.99^l)
  AlgRisk = lapply(strsplit(names(evaluation),"-"),function(vec) vec)
  risks = as.numeric(unique(sapply(AlgRisk,function(vec) vec[2])))
  algorithms = unique(sapply(AlgRisk,function(vec) vec[1]))
  
  # Risk of Reward [[risk]] [[algorithm]] [[measure]]
  RiskofRewards = list()
  for (risk in risks){
    RiskofRewards[[paste0(risk)]] = list()
  }
  
  for (i in 1:length(AlgRisk)){
    risk = AlgRisk[[i]][2]
    alg = AlgRisk[[i]][1]
    X = evaluation[[paste(AlgRisk[[i]],collapse="-")]]
    
    RiskofRewards[[ risk ]][[ alg ]] = list()
    RiskofRewards[[ risk ]][[ alg ]][["VaR"]] = quantile(X,1-as.numeric(risk))
    RiskofRewards[[ risk ]][[ alg ]][["CVaR"]] = CVAR(X,beta = as.numeric(risk))
    RiskofRewards[[ risk ]][[ alg ]][["EVaR"]] = EVAR(X,levels = L,risk = as.numeric(risk))
  }

}
