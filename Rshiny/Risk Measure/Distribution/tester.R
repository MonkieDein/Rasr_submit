
library(ggplot2)
X = 1:100
risk = 0.10
VaR = quantile(X,risk,type=1)
CVaR = (mean(X*(X<=VaR)) + VaR*(risk - mean(X<=VaR)))/risk

VaR
CVaR 


line.data <- data.frame(xintercept = c(VaR, CVaR), Measure = c("V@R", "CV@R"),
                        color = c("red", "blue"), stringsAsFactors = FALSE)

df = data.frame(X=X)
ggplot(df,aes(x=X))+   theme(legend.position="top")+
  geom_histogram(aes(y = ..density..), alpha = 0.4,position = position_dodge(),bins=100)+
  geom_vline(aes(xintercept = xintercept, color = Measure), line.data, size = 1)
  
  



