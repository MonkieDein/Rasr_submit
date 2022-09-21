
# Please complete this function : use pflug cvar as reference
CondVaR = function(V,knots,a,Pbar,Rbar,gamma,lSl,lLl = nrow(V)){
  Vnew = V*0
  TransitionCnt = array(0,dim=c(lLl,lSl,lSl))
  for (s in 1:lSl){
    df = list()
    for (s2 in 1:lSl){
      # need sorted value and weight 
      df[[s2]] = data.frame(L = knots ) 
      df[[s2]]$dL = c(0,df[[s2]]$L[-1] - df[[s2]]$L[-lLl])
      df[[s2]]$V = Rbar[s,a,s2] + gamma * V[,s2]
      df[[s2]]$Ps2dL = Pbar[s,a,s2]*df[[s2]]$dL
      df[[s2]]$State = s2
    }
    df = Reduce(rbind,df)
    df = df[df$Ps2dL > 0,]
    df = df[order(df$V),]
    
    Psum = 0
    cur = 1
    LevelCnt = rep(1,lSl)
    
    # Solve for the Robust Value function
    TransitionCnt[1,s,] = LevelCnt
    Vnew[1,s] = min(df$V[df$Ps2dL>0])
    for (l in 2:lLl){
      k = knots[l]
      while (k > (Psum + df$Ps2dL[cur]+1e-10) ){
        Psum = (Psum + df$Ps2dL[cur])
        LevelCnt[df$State[cur]] = LevelCnt[df$State[cur]] + 1
        cur = cur + 1
      }
      TransitionCnt[l,s,] = LevelCnt
      # TransitionCnt[l,s,df$State[cur]] = TransitionCnt[l,s,df$State[cur]] + (k-Psum)/df$Ps2dL[cur]
    }
  }
  return(list(Vnew = Vnew,TCounter = TransitionCnt))
}

# This algorithm use discretization for Pflug CVaR
solveVaR = function(MDP, lLl = 101, knots = NULL){
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
    VaR_out <- foreach (a = 1:MDP$lAl) %dopar% {
      CondVaR(V,knots,a,MDP$Pbar,MDP$Rbar,MDP$gamma,MDP$lSl,lLl)
    }
    registerDoParallel(cores=1)
    stopImplicitCluster()
    V_list = lapply(1:MDP$lAl,function(a) VaR_out[[a]][["Vnew"]])
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
  T_list = lapply(1:MDP$lAl,function(a) VaR_out[[a]][["TCounter"]])
  T_cnt = array(0,dim=c(lLl,MDP$lSl,MDP$lSl))
  for (l in 1:lLl){
    for (s in 1:MDP$lSl){
      T_cnt[l,s,] = T_list[[Pi[l,s]]][l,s,]
    }
  }
  return(list(V=V,Pi=Pi,T_cnt = T_cnt))
}