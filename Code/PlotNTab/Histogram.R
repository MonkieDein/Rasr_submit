setwd("~/Desktop/GITHUB/Rasr_submit")
rm(list=ls())
source("Code/Basic_Utils.R")
library(ggplot2)

TestFold = wdir("Eval/test/")
PlotFold = wdir(paste0(TestFold,"histogram/"))

domains = c("inventory","population","riverswim") 
domain = "riverswim"

for (domain in domains){
  
  load(file = paste0(TestFold,domain,"/evaluation.RData"))
  L = sapply(0:4000,function(l) exp(20)*0.99^l)
  risks = c("0.99","0.95","0.9")
  for (risk in risks){
    NominalX = evaluation[[paste0("Derman-",risk)]]
    RASRX = evaluation[[paste0("RASR-",risk)]]
    df = rbind(data.frame(Algorithms = "Nominal", Returns = NominalX),data.frame(Algorithms = "RASR", Returns = RASRX))
    
    pdf(file = paste0(wdir(paste0(PlotFold,domain)),"/full_",round(as.numeric(risk)*100),"confident2.pdf"), width = 18)
    print(ggplot(df, aes(x = Returns, fill = Algorithms)) + geom_histogram(position = "identity", alpha = 0.2, bins = 50) +
            geom_vline(aes(xintercept = EVAR(RASRX,levels=L,risk=as.numeric(risk)),linetype="EVaR", col = "RASR")) + 
            geom_vline(aes(xintercept = EVAR(NominalX,levels=L,risk=as.numeric(risk)),linetype="EVaR", col = "Nominal")) + 
            theme(text = element_text(size = 35),axis.text = element_text(size = 30),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"),
                  legend.position = c(0.1, .75))  + ggtitle("") + #legend.title = element_blank(), 
            scale_linetype_manual(name = "Statistics",values=c("EVaR" = "dotdash")) + # "VAR" = "solid","CVAR" = "longdash",
            scale_color_manual(name = "Algorithms",values=c("Nominal" = "red","RASR" = "blue")) + 
            theme(legend.key.size = unit(1, "cm")) + ylab("Count") )
    dev.off()

    df_tail = df[df$Returns<= max(quantile(RASRX,1-as.numeric(risk)),quantile(NominalX,1-as.numeric(risk))),]
    
    pdf(file = paste0(PlotFold,domain,"/tail_",round(as.numeric(risk)*100),"confident2.pdf"), width = 18)
    print(ggplot(df_tail, aes(x = Returns, fill = Algorithms)) + geom_histogram(position = "identity", alpha = 0.2, bins = 50) +
            geom_vline(aes(xintercept = EVAR(RASRX,levels=L,risk=as.numeric(risk)),linetype="EVaR", col = "RASR")) + 
            geom_vline(aes(xintercept = EVAR(NominalX,levels=L,risk=as.numeric(risk)),linetype="EVaR", col = "Nominal")) + 
            theme(text = element_text(size = 35),axis.text = element_text(size = 30),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                  legend.position = c(0.2, .75))  + ggtitle("") + #,legend.title = element_blank()
            scale_linetype_manual(name = "Statistics",values=c("EVaR" = "dotdash")) + # "VAR" = "solid","CVAR" = "longdash",
            scale_color_manual(name = "Algorithms",values=c("Nominal" = "red","RASR" = "blue")) + 
            theme(legend.key.size = unit(1, "cm")) + ylab("Count") ) 
    dev.off()    
  }
  

  
}









