# Set working directory as the main folder of the code
# Change to your respective directory location
setwd("~/Documents/GitHub/Rasr_submit")
source("Code/RASR_code.R")
source("Code/PflugCVaR.R")
library(zeallot)
library(gsubfn) 
library(foreach) # For parallel programming purposes
library(doParallel)
library(ggplot2)

domains = c("riverswim","inventory") # ,"population" file too big cannot push to git.
algorithms = c("RASR","Naive","Erik","Derman","RSVF","BCR","RSVI","Chow")
risks = c(0.99,0.95,0.9)
# Folders
TrainFold = wdir(paste0(wdir("Eval/"),"train/"))
TestFold = wdir(paste0(wdir("Eval/"),"test/"))

domain = "riverswim"


for (domain in domains){
  if (file.exists(paste0(TestFold,domain,"/evaluation.RData"))){
    load(file = paste0(TestFold,domain,"/evaluation.RData"))
  } else{
    evaluation = list()
  }
  
  cat("Evaluating",domain,"evaluator")

  loadFold = wdir(paste0(TrainFold,domain,"/"))
  # Get Solved policy
  if (file.exists(paste0(loadFold,"Train.RData"))){
    cat(paste0("Use Cache Evaluations ",domain,"\n"))
    load(file = paste0(loadFold,"Train.RData"))
    load(file = paste0(loadFold,"CRAAMPolicies.RData"))
  } else{
    stop("file did not exist, please run main before evaluating")
  }


  # Generate instances if does not exist
  saveFold = wdir(paste0(wdir(paste0(TestFold,domain,"/")),"InstanceGenerator/"))
  n = ifelse(domain == "riverswim",500,10000)
  t = 500
    if (!file.exists(paste0(saveFold,"/instance_",n,".csv"))){
    generate_sample(MDP,n,t,saveFold)
  }

  # Initialize required parameter  
  L = ERM_param$L # This is ERM levels
  # intial distribution 
  inits = which(MDP$S_0$probability > 0)
  if (sum(abs(MDP$S_0$probability[inits] - rep(1/length(inits),length(inits)))) > 1e-8){
    stop("Not uniform initial S at EvalExactERMPi, please implement non stationary S0")
  }
  
  # risk = 0.9
  for (risk in risks){
  theta = 1-risk
  
  
  # Algorithm preprocess for policy
  for (alg in algorithms){
    cat("Evaluating",alg,domain,"evaluator")
    
    if (!is.null(evaluation[[paste0(alg,"-",risk)]])){
      next
    }
    # Chow algorithm require to evaluate with history dependent evaluator
    if (alg == "Chow"){
      registerDoParallel(cores=detectCores())
      # If history dependent we need to solve it with a specific evaluator
      TDR_D3U <- foreach (i= 1:n,.combine = 'c') %:% foreach (s0 = inits,.combine = 'c') %dopar% {
        # s0_aug = Extra$Aug$S_map[[paste(s0,which.min(abs(theta - PflugCVaROut$L)),sep="-")]]
        EvalHistPi2(i,Extra$Aug$S_map[[paste(s0,which.min(abs(theta - PflugCVaROut$L)),sep="-")]],
                    PflugCVaROut$Pi,MDP,saveFold, Extra$Aug$S, Extra$Aug$S_map,PflugCVaROut$T_cnt,Time=t)
      }
      registerDoParallel(cores=1)
      stopImplicitCluster()
    } else{
      if (alg == "RASR"){
        EVaR = max(RasrErmOut$Scores + log(1-risk)/L)
        pi = RasrErmOut[["Pi"]][which.max(RasrErmOut$Scores + log(1-risk)/L):ERM_param$lTl,]
      } else if (alg == "Naive"){
        EVaR = max(NaiveErmOut$Scores + log(1-risk)/L)
        pi = matrix(NaiveErmOut[["Pi"]][which.max(NaiveErmOut$Scores + log(1-risk)/L),],nrow=1)
      } else if (alg == "Erik"){
        EVaR = max(EpisErmOut$Scores + log(1-risk)/L)
        pi = matrix(EpisErmOut[["Pi"]][which.max(EpisErmOut$Scores + log(1-risk)/L),],nrow=1)
      } else {
        pi = matrix(Policies[[alg]][which(abs(Policies$risk_levels-risk)<1e-10),],nrow=1)
      }
      registerDoParallel(cores=detectCores())
      # If history dependent we need to solve it with a specific evaluator
      TDR_D3U <- foreach (i= 1:n,.combine = 'c') %:% foreach (s0 = inits,.combine = 'c') %dopar% {
        EvalMarkovPi(i,s0,pi,MDP,saveFold,Time=t)
      }
      registerDoParallel(cores=1)
      stopImplicitCluster()
    }
    evaluation[[paste0(alg,"-",risk)]] = TDR_D3U
  }
}
  save(evaluation,file = paste0(TestFold,domain,"/evaluation.RData"))
}






