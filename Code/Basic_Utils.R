library(gsubfn)  # need 0.7-0 or later
library(stringr)
# TURN single outcome mdp.data.frame into it's respective reward,transition (S,A,S') matrix.
prep_MDP = function(mdp.df,MDP){
  P <- array(0, c(MDP$lSl,MDP$lAl,MDP$lSl)) # transition prob dim
  R = array(0, c(MDP$lSl,MDP$lAl,MDP$lSl)) # reward dim 
  dimnames(P) = list(MDP$S,MDP$A,MDP$S)
  
  n_rows = nrow(mdp.df)
  # Assign transition prob and reward matrix w.r.t(S,A,S')
  for (r in 1:n_rows){
    R[MDP$S == mdp.df$idstatefrom[r],MDP$A == mdp.df$idaction[r],MDP$S == mdp.df$idstateto[r]] = mdp.df$reward[r]
    P[MDP$S == mdp.df$idstatefrom[r],MDP$A == mdp.df$idaction[r],MDP$S == mdp.df$idstateto[r]] = mdp.df$probability[r]
  }
  return(list(P=P,R=R))
}

df2mdp = function(folder_name,file){
  mdp.df = read.csv( paste0(folder_name,file)  ,header = TRUE)
  MDP = list()
  # Parse in Basic parameter States, Actions, Outcomes space and their length.
  MDP$O = sort(unique(mdp.df$idoutcome)) 
  MDP$S = sort(unique(unique(c(mdp.df$idstatefrom,mdp.df$idstateto)))) 
  MDP$A = sort(unique(mdp.df$idaction))
  MDP$lOl = length(MDP$O)
  MDP$lSl = length(MDP$S) 
  MDP$lAl = length(MDP$A) 
  # Extract each outcomes Reward and Transition Matrix.
  temp = lapply(MDP$O, function(o) prep_MDP(mdp.df[mdp.df$idoutcome==o,],MDP))
  MDP$P = lapply(MDP$O+1, function(o) temp[[o]]$P)
  MDP$R = lapply(MDP$O+1, function(o) temp[[o]]$R)
  # Solve or Parse in other MDP parameters.
  MDP$Pbar = Reduce("+",MDP$P)/MDP$lOl
  MDP$Rbar = Reduce("+",MDP$R)/MDP$lOl
  MDP$gamma = read.csv(paste0(folder_name,"/parameters.csv"),header = TRUE)$value
  MDP$S_0 = read.csv(paste0(folder_name,"/initial.csv"),header = TRUE)
  return(MDP)
}


# Expectation function which calculate the mean for vector-reward X and probability of occurrence.
E = function(X, prob = NULL){
  if (!is.null(prob)){
    if (length(prob) != length(X)){
      stop("Mismatch Dimensions of prob and value")
    }
    return(sum(X*prob))
  }
  return(mean(X))
}

# Entropic Risk Measure
# Given a set of values, the function evaluate the appropriate entropic risk value
# Alpha refer to the risk aversion parameter where 
# ERM(alpha = lim -> 0) is the expectation and ERM(alpha = lim -> infty) is worst case
# Entropic Risk Measure with Log-Sum-Exp Trick with Prob
ERM = function(X, alpha=0.9, prob = NULL){
  if (alpha == 0){
    return(E(X,prob))
  }
  Y = -alpha*X
  C = max(Y)+1
  if (!is.null(prob)){
    if (length(prob) != length(X)){
      stop("Mismatch Dimensions of prob and value")
    }
    # The hat is avoid underflow, we need to shift the value to max of non-zero prob
    Yhat = Y[prob!=0]
    Chat = max(Yhat)
    probhat = prob[prob!=0]
    return(-(Chat+log(sum(exp(Yhat-Chat)*probhat)))/alpha)
  }
  return(-(C+log(mean(exp(Y-C))))/alpha)
}

EVAR = function(X,levels,risk=0.95, prob=NULL){
  return(max(sapply(levels, function(z) ERM(X,alpha = z,prob = prob) + log(1-risk)/z )))
}


# Which.Erm2Evar takes a vector of Scores for Erm (Eta), their respective alphas (L) and a confident level (beta)
# that is comparable to VaR and CVaR. Erm2Evar return the optimal index of the optimal alpha in L.
which.Erm2Evar = function(L, Eta, beta = 0.9){
  if (length(Eta) != length(L)){
    warning("Risk and return dimension does not match.")
  }
  return( which.max( Eta + log(1-beta)/L ) )
}


# Theta is used as significant level, theta = 0 equivalent to minimum, theta = 1 equivalent to average
CVAR = function(X,thetas = 0.05,prob = NULL){
  n = length(X)
  if (is.null(prob)){
    prob = rep(1/n,n)
  } else if (length(prob) != length(X)){
    stop("Mismatch Dimensions of prob and value")
  } else if (abs(sum(prob) - 1) > 1e-8){
    stop("Distribution probability does not sum to one (1)")
  }
  
  # sort value and its probability
  ord = order(X)
  X = X[ord]
  prob = prob[ord]

  # force thetas to be a vector
  thetas = c(thetas)
  if (max(thetas)>1 || min(thetas)<0){
    stop("Undefined confident level. (thetas) should be an array between 0 and 1.")
  }
  lLl = length(thetas)
  v = thetas*0
  names(v) = thetas
  
  # Initialize parameter for loop
  Psum = 0 
  Vsum = 0
  index = 1
  
  for (l in 1:lLl){
    k = thetas[l]
    if (k == 0){
      v[l] = min(X[prob>0])
    } else {
      while (k > (Psum + prob[index] +1e-10) ){
        Psum = (Psum + prob[index])
        Vsum = Vsum + prob[index]*X[index]
        index = index + 1
      }
      v[l] = (Vsum + (k-Psum)*X[index])/k  # This is Piecewise Linear
    }
  }
  return(v)
}

# Total Discounted Return (Cost)
# TDR take in vector of returns V[t=0,t=1,...] and discount factor 
# Calculated the total discounted return
TDR = function(V, discount=0.9){
  D = sapply(1:length(V),function(t) discount^(t-1))
  return(sum(D * V))
}

wdir = function(directory_name){
  if (!dir.exists(directory_name)){
    cat("Directory",directory_name,"Not Exist. Creating Directory...\n")
    dir.create(directory_name)
  }
  return(directory_name)
}

# Generate Model and sampling instances for every time step. 
# This also generate initial state distribution but we did not use
# this initial state distribution since we have either single initial state 
# or uniformly distributed initial state.
generate_sample = function(MDP,n,t,folder_name){
  set.seed(1)
  O = 1:MDP$lOl
  for (i in 1:n){
    write.csv(data.frame(R_outcome = sample(O,t,replace=TRUE), 
                         T_outcome = sample(O,t,replace=TRUE),
                         S_ = runif(t)),paste0(folder_name,"/instance_",i,".csv"))
  }
  write.csv(sample(1:MDP$lSl,prob = MDP$S_0$probability,n,replace = TRUE),
            paste0(folder_name,"/S0.csv"))
}

# Draw Next State S'
drawS_ = function(weights,choice){
  choiceIndex = 1
  for (w in weights){
    choice = choice - w
    if (choice <= 1e-10){
      return(choiceIndex)
    }
    choiceIndex = choiceIndex + 1
  }
  return(choiceIndex)
}

VAR = function(X,theta = 0.05,prob = NULL){
  # sort value and its probability
  ord = order(X)
  X = X[ord]
  prob = prob[ord]
  return(X[drawS_(prob,theta)])
}

# For Stationary Policy please pass in Pi as matrix(Pi,nrow = 1)
EvalMarkovPi = function(i,s0,Pi,MDP,folder_name,Time = NULL){
  cur_df = read.csv(paste0(folder_name,"/instance_",i,".csv"))
  if (is.null(Time)){Time = nrow(cur_df)} # Sample Time Horizon
  lLl = nrow(Pi)      # Risk Levels Depth
  TDR = 0
  gamma_T = 1
  cur_s = s0
  for (t in 1:Time){
    lvl = min(t,lLl)
    s_next = drawS_(MDP$P[[cur_df$T_outcome[t]]][cur_s,Pi[ lvl, cur_s],],choice = cur_df$S_[t])
    TDR = TDR + gamma_T*MDP$R[[cur_df$R_outcome[t]]][cur_s,Pi[lvl , cur_s],s_next]
    gamma_T = MDP$gamma*gamma_T
    cur_s = s_next
  }
  TDR = TDR/(1-gamma_T)
  return(TDR)
}

# This EvalHistPi does not require whole Augmenting Transition Model to be passed in
# In every time step it solve for the transition independently
EvalHistPi = function(i,s0,Pi,MDP,folder_name, S_Aug,S_map,T_cnt,Time = NULL){
  cur_df = read.csv(paste0(folder_name,"/instance_",i,".csv"))
  if (is.null(Time)){Time = nrow(cur_df)} # Sample Time Horizon
  lLl = nrow(Pi)      # Risk Levels Depth
  TDR = 0
  gamma_T = 1
  # Cur_s and s_next is a reference to the augmented States
  cur_s = s0 
  # s_ori,s_new and l_ori,l_new refer to the initial state and level correspond the the augment state
  for (t in 1:Time){
    c(s_ori,l_ori) %<-% as.numeric(unlist(str_split(S_Aug[cur_s],"-")))
    P_Aug = rep(0,length(S_Aug))
    for (s2 in 1:MDP$lSl){
      P_Aug[S_map[[paste(s2,floor(T_cnt[l_ori,s_ori,s2]),sep="-")]]] = MDP$P[[cur_df$T_outcome[t]]][s_ori,Pi[l_ori , s_ori],s2] * (1 - dec(T_cnt[l_ori,s_ori,s2]))
      P_Aug[S_map[[paste(s2,floor(T_cnt[l_ori,s_ori,s2]) + 1,sep="-")]]] = MDP$P[[cur_df$T_outcome[t]]][s_ori,Pi[l_ori , s_ori],s2] * (dec(T_cnt[l_ori,s_ori,s2]))
    }
    if (abs(1-sum(P_Aug))>1e-10){
      warning("Transition does not sum to one")
    }
    s_next = drawS_(P_Aug,choice = cur_df$S_[t])
    c(s_new,l_new) %<-% as.numeric(unlist(str_split(S_Aug[s_next],"-")))
    TDR = TDR + gamma_T*MDP$R[[cur_df$R_outcome[t]]][s_ori,Pi[l_ori , s_ori],s_new]
    gamma_T = MDP$gamma*gamma_T
    cur_s = s_next
  }
  TDR = TDR/(1-gamma_T)
  return(TDR)
}
