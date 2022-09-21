setwd("~/Desktop/GITHUB/Rasr_submit")
# setwd("C:/GITHUB/Rasr_submit")
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
EVaRs = sapply(1-Risks, function(risk) ifelse(abs(risk-1)<1e-10, 0,max(ERMs + log(1-risk)/ERM_levels)) )


df = rbind(data.frame(Risk = Risks, Value=VaRs, Measure = "VaR"),
           data.frame(Risk = Risks, Value=CVaRs, Measure = "CVaR"),
           data.frame(Risk = Risks, Value=EVaRs, Measure = "EVaR"),
           data.frame(Risk = Risks, Value=E, Measure = "Mean")
    )

ggplot(df,aes(x=Risk,y=Value, group = Measure)) + geom_line(aes(color=Measure))+
    theme(legend.position="right") 





