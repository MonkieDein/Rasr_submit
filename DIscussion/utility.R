
X           = (1:100)*0.01
pX          = rep(0.1,100)
ERM_lvls    = 1e10 * (0.99^(1:5000))
ERM_X       = sapply(ERM_lvls,function(alp) ERM(X,alp))
EVAR_lvl    = 0.6
h_alp       = ERM_X + log(1-EVAR_lvl)/ERM_lvls
EVAR_i      = which.max(h_alp)
EVAR        = max(h_alp)
EVAR_argsup = ERM_lvls[EVAR_i]

etX         = exp(-EVAR_argsup*X)
Z           = etX/mean(etX)

# This number sould be close to zero
mean(Z*log(Z)) + log(1-EVAR_lvl)

mean(X*Z)

D_var = (X >= quantile(X,EVAR_lvl))*max(X)
D_cvar = ((quantile(X,EVAR_lvl)-X)>=0)*X/EVAR_lvl + ((quantile(X,EVAR_lvl)-X)<0)*max(X)
D_evar = Z*0
D_evar[1] = Z[1]*pX[1]
for (i in 2:100){D_evar[i]=D_evar[i-1]+Z[i]*pX[i]}
D_erm = (1-exp(-EVAR_argsup*(X-ERM(X,EVAR_argsup) ) ))/EVAR_argsup
D_erm_shift = D_erm + (max(X) - max(D_erm))

plot(x = c(0,X),y=c(0,D_evar),xlim=c(0,max(X)),ylim=c(0,max(X)),type="l",lwd=3,ylab="D(X)",xlab="X",main="Distortion function")
points(X,D_var ,col="red")
abline(a=0,b=1,col="blue",lwd=3)
abline(v=max(which(Z>1))+0.5,col="black",lty = 2)
lines(X, D_cvar,lwd=3,col="darkgreen" )
lines(X, D_erm_shift,lwd=3,col="purple" )
legend(70, 40, legend=c("Mean", "VaR","CVaR","EVaR"),
       col=c("blue", "red","darkgreen","black"),lwd=3, cex=0.8)

U_var = X*0
U_var[quantile(X,EVAR_lvl,type=1)] = 100
U_cvar = ((quantile(X,EVAR_lvl)-X)>=0)/EVAR_lvl
U_evar = Z

plot(x = X,y=Z,xlim=c(0,100),ylim=c(0,5),type="l",lwd=3,ylab="u(X)",xlab="X",main="Utility function")
lines(X,U_cvar ,col="darkgreen",lwd=3)

lines(X,U_var ,col="red",lwd=3)

