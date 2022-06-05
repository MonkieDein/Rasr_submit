source("Code/Basic_Utils.R")
library(gsubfn) 
library(foreach) # For parallel programming purposes
library(doParallel)
library(ggplot2)

prep_ERM = function(MDP,alpha_0 = exp(10),epsilon = exp(-15) ,Rtemp = NULL){
  # Generate approximation horizon (T) and required levels (L)
  lTl = ceiling(log(alpha_0/epsilon)/(1-MDP$gamma))
  L = c(sapply(1:(lTl - 1), function(t) alpha_0*MDP$gamma^(t-1) ),0) 

  if (is.null(Rtemp)){
    Rtemp = array(unlist(MDP$R), dim=c(MDP$lSl,MDP$lAl,MDP$lSl,MDP$lOl))
  }
  registerDoParallel(cores=detectCores())
  # Save the final score in combined score
  rAlp <- foreach (t = 1:lTl) %dopar% {
    array( sapply(1:MDP$lSl,function(s2) sapply(1:MDP$lAl,function(a) sapply(1:MDP$lSl,function(s)
      ERM(Rtemp[s,a,s2,],alpha = L[t])
    ))) , dim=c(MDP$lSl,MDP$lAl,MDP$lSl))
  }
  registerDoParallel(cores=1)
  stopImplicitCluster()

  return(list(lTl = lTl, L = L, rAlp = rAlp))
}

RasrErm = function(MDP, rAlp, L, lTl=length(L) ,Pi = NULL,v_nominal = NULL){
  V = matrix( 0 , nrow = lTl , ncol = MDP$lSl )
  solvePi = is.null(Pi)
  # Solve for Pi if not given otherwise only solve for V.
  if (solvePi){ Pi = V*0 } 
  # don't need to solve nominal if passed in
  n_iter = ifelse(is.null(v_nominal),1000,1)
  if (!is.null(v_nominal)){V[lTl,] = v_nominal}
  for (i in 1:n_iter){ # Here we assume the nominal MDP converge in less than 1000 steps 
    if (solvePi){ Pi[lTl,] = sapply(1:MDP$lSl, function(s)  which.max(sapply(1:MDP$lAl, function(a)  MDP$Pbar[s,a,] %*% (MDP$Rbar[s,a,] + MDP$gamma * V[lTl,])  ) )) }
    V[lTl,] = sapply(1:MDP$lSl, function(s) MDP$Pbar[s,Pi[lTl,s],] %*% (MDP$Rbar[s,Pi[lTl,s],] + MDP$gamma * V[lTl,]) )  
  }
  for (t in (lTl - 1):1){
    if (solvePi){ Pi[t, ] = sapply(1:MDP$lSl, function(s)  which.max(sapply(1:MDP$lAl, function(a)  ERM( c(rAlp[[t]][s,a,] + MDP$gamma * V[t+1,]),alpha = L[t],prob=MDP$Pbar[s,a,])   ))) }
    V[t, ] = sapply(1:MDP$lSl, function(s) ERM( c(rAlp[[t]][s,Pi[t,s],] + MDP$gamma * V[t+1,]),alpha = L[t],prob=MDP$Pbar[s,Pi[t,s],])   )
  }
  # Evaluate the ERM for each risk level.
  Scores = sapply(1:lTl,function (t) ERM(V[t,],alpha = L[t],prob = MDP$S_0$probability) ) 
  return(list(Pi = Pi, V = V, Scores = Scores))
}

# Since NaiveErm and EpisErm uses do-par for parallel programming, ErmMat2List convert the matrix output to list type
ErmMat2List = function(stats,levels,S,lSl){
  Scores = stats[,2*lSl+1]
  V = stats[,(1:lSl)+lSl]
  rownames(V) = levels
  colnames(V) = S
  
  Pi = stats[,(1:lSl)]
  rownames(Pi) = levels
  colnames(Pi) = S
  return(list(Pi = Pi, V = V,Scores = Scores))
}


# Solve the NaiveErm for single level.
NaiveErm1L = function(alpha,V_cur,R_cur,Pbar,Pi_cur,lSl,lAl,discount,S_0 ){
  solvePi = is.null(Pi_cur)
  for (i in 1:1000){
    if (solvePi) { Pi_cur = sapply(1:lSl, function(s)  which.max(sapply(1:lAl, function(a) ERM( c(R_cur[s,a,] + discount * V_cur),alpha = alpha,prob=Pbar[s,a,]) ))) }
    V_cur = sapply(1:lSl, function(s) ERM( c(R_cur[s,Pi_cur[s],] + discount * V_cur),alpha = alpha,prob=Pbar[s,Pi_cur[s],]) )
  }
  Scores_cur = ERM(V_cur,alpha = alpha,prob=S_0$probability) 
  return(c(Pi_cur,V_cur,Scores_cur))
}
# NaiveErm is RasrErm without changing risk-level for bellman update.
NaiveErm = function(MDP , rAlp, levels, Pi = NULL){
  registerDoParallel(cores=detectCores())
  # Save the final score in combined score
  statistics <- foreach (t = 1:length(levels),.combine = 'rbind') %dopar% {
    NaiveErm1L(levels[t] , rep(0,MDP$lSl) , rAlp[[t]] , MDP$Pbar , Pi[t,] , MDP$lSl , MDP$lAl , MDP$gamma , MDP$S_0)
  }
  registerDoParallel(cores=1)
  stopImplicitCluster()
  # statistics <- lapply(1:length(levels),function(t) NaiveErm1L(levels[t] , rep(0,MDP$lSl) , rAlp[[t]] , MDP$Pbar , Pi[t,] , MDP$lSl , MDP$lAl , MDP$gamma , MDP$S_0)) 
  # statistics = Reduce(rbind,statistics)
  return(ErmMat2List(statistics , levels , MDP$S , MDP$lSl))
}


# Solve the EpisErm for single level.
EpisErm1L = function(alpha,outcomes,V_cur,Rew,P,Pi_cur,lSl,lAl,discount,S_0){
  solvePi = is.null(Pi_cur)
  for (i in 1:1000){
    if (solvePi){ Pi_cur = sapply(1:lSl, function(s)  which.max(sapply(1:lAl, function(a) ERM( sapply(outcomes+1, function(o) P[[o]][s,a,] %*% (Rew[[o]][s,a,]+ discount * V_cur)), alpha = alpha)  ))) }
    V_cur = sapply(1:lSl, function(s) ERM( sapply(outcomes+1, function(o) P[[o]][s,Pi_cur[s],] %*% (Rew[[o]][s,Pi_cur[s],]+ discount * V_cur)), alpha = alpha) )
  }
  Scores_cur = ERM(V_cur,alpha = alpha,prob=S_0$probability) 
  return(c(Pi_cur,V_cur,Scores_cur))
}
# EpisErm is the NaiveErm which consider epistemic uncertainty only.  
EpisErm = function(MDP, levels, Pi = NULL){
  registerDoParallel(cores=detectCores())
  # Save the final score in combined score
  statistics <- foreach (t = 1:length(levels),.combine = 'rbind') %dopar% {
    EpisErm1L(levels[t] , MDP$O , rep(0,MDP$lSl) , MDP$R , MDP$P , Pi[t,] , MDP$lSl , MDP$lAl , MDP$gamma , MDP$S_0)
  }
  registerDoParallel(cores=1)
  stopImplicitCluster()
  # statistics = lapply(1:length(levels), function(t) EpisErm1L(levels[t] , MDP$O , rep(0,MDP$lSl) , MDP$R , MDP$P , Pi[t,] , MDP$lSl , MDP$lAl , MDP$gamma , MDP$S_0))
  # statistics = Reduce(rbind,statistics)
  # 
  return(ErmMat2List(statistics , levels , MDP$S , MDP$lSl))
}

