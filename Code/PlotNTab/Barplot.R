# Set working directory as the main folder of the code
# Change to your respective directory location
setwd("~/Desktop/GITHUB/Rasr_submit")
rm(list=ls())
source("Code/Basic_Utils.R")
library(ggplot2)

TestFold = wdir("Eval/test/")
PlotFold = wdir(paste0(TestFold,"boxplot/"))

domains = c("inventory","population","riverswim") 
# domain = "riverswim"

for (domain in domains){
  
  load(file = paste0(TestFold,domain,"/RoR_df.RData"))
  risks = names(RoR_df)
  names(risks) = risks
  measures = colnames(RoR_df[[1]])
  algos = rownames(RoR_df[[1]])
  
  stats = lapply(risks, function(risk) 
    Reduce(rbind,lapply(1:ncol(RoR_df[[risk]]),function(co) 
      data.frame(algorithms = algos, Values = unlist(RoR_df[[risk]][,co]), Measure = measures[co])  ))) 
  
  for (beta in risks){
    miny = min(stats[[paste0(beta)]]$Values)
    miny = floor(miny) - floor(abs(miny*0.01))
    ranges = max(stats[[beta]]$Values-miny) - min(stats[[beta]]$Values-miny)
    ranges_set = round((0:10)*round(ranges/10,1))

    pdf(file = paste0(wdir(paste0(PlotFold,"/",domain,"/")),round(as.numeric(beta)*100),"confident.pdf"), width = 16)
    print(ggplot(data=stats[[beta]], aes(x=algorithms, y=Values-miny, fill=Measure)) + xlab("Algorithm") +
            ylab("Risk of Return")+ geom_bar(stat="identity", position="identity") + 
            scale_y_continuous(labels = function(y) y + miny, breaks = ranges_set ) + 
            theme(text = element_text(size = 35),axis.text = element_text(size = 30),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))  + ggtitle("") +
            scale_fill_manual(values = c("EVaR"="#0000FF", "CVaR"="#9C9CFF", "VaR"="#D5D5FF"))+ 
            ggtitle(paste0(domain," ",round(as.numeric(beta)*100),"% risk of return") )+
            scale_x_discrete(limits=algos) )

    dev.off()
  }
  
}









