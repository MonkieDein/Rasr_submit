# Set working directory as the main folder of the code
# Change to your respective directory location
setwd("~/Documents/GitHub/Rasr_submit")

source("Code/RASR_code.R")
source("Code/PflugCVaR.R")

# Domain put to test
domains = c("riverswim","inventory") #  ,"population" : file too big cannot push to git.
# Folders
TrainFold = wdir(paste0(wdir("Eval/"),"train/"))

# domain = "inventory"
for (domain in domains){
  cat("domain",domain)
  saveFold = wdir(paste0(TrainFold,domain,"/"))

  if (file.exists(paste0(saveFold,"Train.RData"))){
    cat(paste0("Use Cache Trained Output \n",domain))
    next
  }
  MDP = df2mdp(paste0("Domains/",domain),"/training.csv")
  
  startPrep <- Sys.time()
  ERM_param = prep_ERM(MDP,alpha_0 = exp(10),epsilon = exp(-15)) 
  endPrep <- Sys.time()
  
  startRasrErm <- Sys.time()
  RasrErmOut = RasrErm(MDP, ERM_param$rAlp, ERM_param$L) 
  endRasrErm <- Sys.time()

  startNaiveErm <- Sys.time()
  NaiveErmOut = NaiveErm(MDP , ERM_param$rAlp, ERM_param$L)
  endNaiveErm <- Sys.time()

  startEpisErm <- Sys.time()
  EpisErmOut = EpisErm(MDP, ERM_param$L)
  endEpisErm <- Sys.time()

  startPflugCVaR <- Sys.time()
  PflugCVaROut = PflugCVaR(MDP,lLl=101)
  endPflugCVaR <- Sys.time()
  
  Timedf = data.frame(Task = c("ERMprep","RasrErm","NaiveErm","EpisErm","PflugCVaR"),
                     T_start = c(startPrep,startRasrErm,startNaiveErm,startEpisErm,startPflugCVaR),
                     T_end = c(endPrep,endRasrErm,endNaiveErm,endEpisErm,endPflugCVaR))

  Timedf$T_elapsed = Timedf$T_end - Timedf$T_start 
  
  PflugCVaROut$lLl = dim(PflugCVaROut$Pi)[1]
  PflugCVaROut$L = (0:(PflugCVaROut$lLl-1))/(PflugCVaROut$lLl-1)
  Extra = list()
  Extra[["Aug"]] = P_Augment_CVaR( MDP, PflugCVaROut$T_cnt , PflugCVaROut$Pi ,lLl = PflugCVaROut$lLl ,Pbar_only = TRUE )


  write.csv(Timedf,paste0(saveFold,"time.csv"))
  rm(list = ls.str(mode = 'numeric'))
  rm(list = ls.str(mode = 'function'))
  
  
  save.image(file = paste0(saveFold,"Train.RData"))
  rm(list = ls.str(mode = 'list'))
  source("Code/RASR_code.R")
  source("Code/BenchmarkCode/PflugCVaR.R")
}
