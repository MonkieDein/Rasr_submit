setwd("C:/GITHUB/Rasr_submit")
rm(list=ls())
source("Code/RASR_code.R")

library(plotly)

library(ggplot2)
X = 1:100
risk = 0.10

VaR = quantile(X,risk,type=1)
CVaR = ifelse(abs(risk-0)<(1e-10),min(X),(mean(X*(X<=VaR)) + VaR*(risk - mean(X<=VaR)))/risk)
E = mean(X)
ERM_levels = (0.99^(1:3000))*10000
EVaR = EVAR(X,levels = ERM_levels,risk=1-risk)

line.data <- data.frame(xintercept = c(VaR, CVaR,EVaR,E), Measure = c("VaR", "CVaR","EVaR","Mean"),
                        stringsAsFactors = FALSE)
ggplotly(
ggplot(data.frame(X=X),aes(x=X,fill = after_stat(x > VaR))) + 
  theme(legend.position="right") +
  geom_histogram(aes(y = ..density..), alpha = 0.4,position = position_dodge(),bins=100)+
  geom_vline(aes(xintercept = xintercept, color = Measure), line.data, size = 1) +
  scale_color_manual(values=c("red", "darkred", "black","blue"))
)



