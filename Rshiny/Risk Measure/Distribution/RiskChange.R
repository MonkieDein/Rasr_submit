setwd("C:/GITHUB/Rasr_submit")
rm(list=ls())
source("Code/RASR_code.R")

library(plotly)
library(ggplot2)
X = 1:100
Risks = seq(0,1,0.001)

E = mean(X)
VaRs = quantile(X,Risks,type=1)
CVaRs = sapply(1:length(VaRs), function(i)
              ifelse(abs(Risks[i]-0)<(1e-10),min(X),(mean(X*(X<=VaRs[i])) + VaRs[i]*(Risks[i] - mean(X<=VaRs[i])))/Risks[i])
       )
ERM_levels = (0.99^(1:3000))*10000
ERMs = sapply(ERM_levels,function(a) ERM(X=X,alpha = a))
EVaRs = sapply(1-Risks, function(risk) max(ERMs + log(1-risk)/ERM_levels))

# line.data <- data.frame(xintercept = c(VaR, CVaR,EVaR,E), Measure = c("VaR", "CVaR","EVaR","Mean"),
                        # stringsAsFactors = FALSE)

  ggplot(data.frame(X=X),aes(x=X,fill = after_stat(x > VaR))) + 
    theme(legend.position="top")+
    theme(legend.position="right") +
    geom_histogram(aes(y = ..density..), alpha = 0.4,position = position_dodge(),bins=100)+
    geom_vline(aes(xintercept = xintercept, color = Measure), line.data, size = 1) +
    scale_color_manual(values=c("red", "darkred", "black","blue"))




