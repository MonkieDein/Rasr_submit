source("Code/Basic_Utils.R")
library(gsubfn) 
library(foreach) # For parallel programming purposes
library(doParallel)
library(ggplot2)

CondCVaR = function(V,knots,a,Pbar,Rbar,gamma,lSl,lLl = nrow(V)){
  Vnew = V*0
  TransitionCnt = array(0,dim=c(lLl,lSl,lSl))
  for (s in 1:lSl){
    df = list()
    for (s2 in 1:lSl){
      df[[s2]] = data.frame(L = knots, dL = c(0,rep(1/(lLl-1),lLl-1)) ) 
      df[[s2]]$V = Rbar[s,a,s2] + gamma * V[,s2]
      df[[s2]]$LV = Pbar[s,a,s2]*df[[s2]]$L * df[[s2]]$V
      df[[s2]]$Ps2dL = Pbar[s,a,s2]*df[[s2]]$dL
      df[[s2]]$dLV = c(0,df[[s2]]$LV[-1] - df[[s2]]$LV[-lLl])/df[[s2]]$Ps2dL
      df[[s2]]$State = s2
    }
    df = Reduce(rbind,df)
    df = df[df$Ps2dL > 0,]
    df = df[order(df$dLV),]
    
    Vsum = 0
    Psum = 0
    cur = 1
    # Use to know which Augmented State to Transition to.
    LevelCnt = rep(1,lSl)
    # Solve for the Robust Value function
    TransitionCnt[1,s,] = LevelCnt
    Vnew[1,s] = min(df$V[df$Ps2dL>0])
    for (l in 2:lLl){
      k = knots[l]
      while (k > (Psum + df$Ps2dL[cur]+1e-10) ){
        Psum = (Psum + df$Ps2dL[cur])
        Vsum = Vsum + df$dLV[cur]*df$Ps2dL[cur]
        LevelCnt[df$State[cur]] = LevelCnt[df$State[cur]] + 1
        cur = cur + 1
      }
      TransitionCnt[l,s,] = LevelCnt
      TransitionCnt[l,s,df$State[cur]] = TransitionCnt[l,s,df$State[cur]] + (k-Psum)/df$Ps2dL[cur]
      Vnew[l,s] = (Vsum + (k-Psum)*df$dLV[cur])/k # This is Piecewise Linear
    }
  }
  return(list(Vnew = Vnew,TCounter = TransitionCnt))
}

# dec take in a float and return its decimal value
dec = function(f){
  return(f-floor(f))
}

# This algorithm use discretization for Pflug CVaR
PflugCVaR = function(MDP, lLl = 101, knots = NULL){
  # Making Consistent dimension, if only number of knots is given then they are distributed uniformly
  # If knots is given, then knots will overwrite the number of level (lLl) with its length.
  if (is.null(knots)){
    knots = (0:(lLl-1))/(lLl-1)
  } else {
    if (length(knots) != lLl){
      message("Levels is inconsistent with number of knots. Knots will be used instead.")
    }
    lLl = length(knots)
  }
  
  Vnew = matrix(0,nrow = lLl, ncol = MDP$lSl, byrow = TRUE)
  V = Vnew + 1 # initialize arbitrary V that is not 0 to start the while loop
  n_iter = 0
  while (max(abs(V-Vnew)) > 1e-10){
    V = Vnew
    registerDoParallel(cores=detectCores())
    # Solve Conditional CVaR for each action
    CVaR_out <- foreach (a = 1:MDP$lAl) %dopar% {
      CondCVaR(V,knots,a,MDP$Pbar,MDP$Rbar,MDP$gamma,MDP$lSl,lLl)
    }
    registerDoParallel(cores=1)
    stopImplicitCluster()
    V_list = lapply(1:MDP$lAl,function(a) CVaR_out[[a]][["Vnew"]])
    Vnew = Reduce(pmax,V_list)
    n_iter = n_iter + 1
    # Check for bug
    if (sum((Vnew[-1,] - Vnew[-lLl,])< -1e-10) ){ # Non Monotone
      warning("The CVaR is non-monotone need inspection")
    }
    if (n_iter > 1000){ # Non Convergence
      warning("It takes way too long to converge")
      break
    }
  }
  # Retrieve Policy
  Pi_list = lapply(1:MDP$lAl,function(a) a*(V_list[[a]]==Vnew) )
  Pi = Reduce(pmax,Pi_list)
  # Retrieve Transition
  T_list = lapply(1:MDP$lAl,function(a) CVaR_out[[a]][["TCounter"]])
  T_cnt = array(0,dim=c(lLl,MDP$lSl,MDP$lSl))
  for (l in 1:lLl){
    for (s in 1:MDP$lSl){
      T_cnt[l,s,] = T_list[[Pi[l,s]]][l,s,]
    }
  }
  return(list(V=V,Pi=Pi,T_cnt = T_cnt))
}

P_Augment_CVaR = function(MDP, T_cnt , Pi ,lLl = dim(T_cnt)[1] ,Pbar_only = TRUE){
  S_Aug = paste(sapply(1:MDP$lSl,function(s) rep(paste(s),lLl)),1:lLl,sep = "-")
  S_map = list() # Map augmented states to its index
  for (s in 1:length(S_Aug)){ S_map[[paste0(S_Aug[s])]] = s }
  if (!Pbar_only){
    P_aug = lapply(MDP$P, function(P) P_Aug_CVaR_single(MDP$lSl, lLl, S_Aug, S_map, P, T_cnt , Pi) )
  } else {
    P_aug = list()
  }
  Pbar_aug = P_Aug_CVaR_single(MDP$lSl, lLl, S_Aug, S_map, MDP$Pbar, T_cnt , Pi) 

  return(list(P=P_aug,Pbar=Pbar_aug, S = S_Aug, S_map = S_map))
}

P_Aug_CVaR_single = function(lSl, lLl, S_Aug, S_map, P, T_cnt , Pi){
  P_Aug = array(0,dim = c(lSl*lLl,lSl*lLl),dimnames = list(row = S_Aug,col = S_Aug))
  for (l in 1:lLl){
    for (s in 1:lSl){
      a = Pi[l,s]
      for (s2 in 1:lSl){
        P_Aug[S_map[[paste(s,l,sep="-")]],S_map[[paste(s2,floor(T_cnt[l,s,s2]),sep="-")]]] = P[s,a,s2] * (1 - dec(T_cnt[l,s,s2]))
        P_Aug[S_map[[paste(s,l,sep="-")]],S_map[[paste(s2,floor(T_cnt[l,s,s2]) + 1,sep="-")]]] = P[s,a,s2] * (dec(T_cnt[l,s,s2]))
      }
    }
  }
  if (sum(abs(rowSums(P_Aug)-1)>1e-10)){
    warning("There exist augmented transition that does not add to one.")
  }
  return(P_Aug)
}
