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
  return(ifelse(abs(risk-1)<1e-10, ifelse(is.null(prob),min(X),min(X[prob>0])),
                max(sapply(levels, function(z) ERM(X,alpha = z,prob = prob) + log(1-risk)/z ))
  )
  )
}
library(ggplot2)
library(ggridges)


N = 10
M = 100
mu = runif(N,min=1,max=100)
sd = runif(N,min=0,max=3)

X = matrix(0,N,M)
for (i in 1:N){
  X[i,] = rnorm( M,mean=mu[i],sd = sd[i] )
}

M2 = 400
muSmall = runif(N,min=-20,max=-15)
X2 = matrix(0,N,M2)
for (i in 1:N){
  X2[i,] = rnorm( M2,mean=muSmall[i],sd = sd[i] )
}

Xbind = c(X,X2)
Xbind = Xbind - mean(Xbind)
Xbind = Xbind*(-13/min(Xbind))
d = density(Xbind)

# geom_density(fill="lightblue")
dat = data.frame(reward=Xbind)
dat$'density' = 0
ggplot(dat, aes(x=reward,y=density, fill = factor(stat(quantile)))) + stat_density_ridges(
  geom = "density_ridges_gradient", calc_ecdf = TRUE,
  quantiles = 0.5) +
  scale_fill_manual(
    name = "Probability", values = c("lightblue", "#A0A0A0A0"),
    labels = c("[0, 0.5]", "(0.5, 1]")
  )+
  annotate("segment", x = median(dat$reward), xend = max(dat$reward)*1.02, y = 0.37, yend = 0.37,colour = "darkgrey")+
  annotate("text", x = (max(dat$reward) - median(dat$reward))/2, y = 0.39, label = "confidence level")+
  geom_vline(aes(xintercept=mean(reward),color="Mean"), size=1)+
  geom_vline(aes(xintercept=median(reward),color="Median"), size=1)+
  geom_vline(aes(xintercept=mean(reward[reward <= median(reward)]),color="CVaR"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=EVAR(reward,0.999*(0:1000),risk=0.5),color="EVaR"), linetype="dashed", size=1)+
  geom_vline(aes(xintercept=min(reward)-1,color="Min"), size=1)+
  scale_color_manual(name='Measure',
                     breaks=c('Mean', 'Median', 'CVaR', 'EVaR', 'Min'),
                     values=c('Mean'='green', 'Median'='black', 'CVaR'='blue', 'EVaR'='red', 'Min'='pink'))
  