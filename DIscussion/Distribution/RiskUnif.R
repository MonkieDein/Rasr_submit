X = runif(10000)
Xs = sort(X)

L_ERM = 0.99^(1:10000)*10000
risk = 0.01*(0:99)
VaRx = quantile(X,1-risk,type = 1)
CVaRx = sapply( 1-risk,function(r) CVAR(X,r) )
Eta = sapply(L_ERM,function(a) ERM(X,a))
whichEVaRx = sapply(risk, function(r) which.Erm2Evar(L_ERM,Eta,r) )
ERMx_Via_EVaR = Eta[whichEVaRx]
EVaRx = ERMx_Via_EVaR + sapply(risk , function (r) log(1-r))/L_ERM[whichEVaRx]

df = data.frame(risk=risk,VaR = VaRx,CVaR = CVaRx,EVaR = EVaRx,selection = whichEVaRx,ERM=ERMx_Via_EVaR)

plot(df$risk,df$CVaR,col="red",type='l')
lines(df$risk,df$EVaR,col="blue")
lines(df$risk,df$ERM,col="purple")
lines(df$risk,df$VaR,col="green")

which(df$VaR<df$ERM)
which(df$CVaR>df$ERM)







