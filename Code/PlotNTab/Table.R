setwd("~/Desktop/GITHUB/Rasr_submit")
rm(list=ls())
source("Code/Basic_Utils.R")
library(kableExtra)

TestFold = wdir("Eval/test/")
TableFold = wdir(paste0(TestFold,"table/"))

domains = c("riverswim","population","inventory" ) 
# TABLE 1 : TIME TO COMPUTE EACH ALGORITHMS
Timecompute = list()
for (domain in domains){
  loadFold = wdir(paste0("Eval/train/",domain,"/"))
  load(file = paste0(loadFold,"Train.RData"))
  Timecompute[[domain]] = data.frame(RASR = sum(Timedf$T_elapsed[Timedf$Task=="ERMprep" | Timedf$Task=="RasrErm"]),
                                     Naive = sum(Timedf$T_elapsed[Timedf$Task=="ERMprep" | Timedf$Task=="NaiveErm"]),
                                     Erik = Timedf$T_elapsed[Timedf$Task=="EpisErm"],
                                     Chow = Timedf$T_elapsed[Timedf$Task=="PflugCVaR"]) 
}
ComputationTime = Reduce(rbind, Timecompute)
rownames(ComputationTime) = names(Timecompute)
ComputationTime %>%
  kbl(caption="Time (sec) to compute each algorithm",row.names = TRUE,
      format="latex",
      align="r") %>%
  kable_minimal(full_width = F,  html_font = "Source Sans Pro")%>%
  cat(., file = paste0(TableFold,"Time.txt"))

# TABLE 2 : RISK OF RETURN OF EACH ALGORITHM
RoR = list()
risks = c("0.99","0.95","0.9")
measures = c("VAR","CVAR","EVAR")
for (risk in risks){ RoR[[risk]] = list()}
for (domain in domains){
  load(file = paste0(TestFold,domain,"/RoR_df.RData"))
  for (risk in risks){
    RoR[[risk]][[domain]] = data.frame(RoR_df[[risk]])
    # Rounding element
    RoR[[risk]][[domain]] = sapply(RoR[[risk]][[domain]] ,function(r) sapply(r,function(elem) round(elem) ))
  }
}

CombineRoR = lapply(RoR,function(Rdf) as.matrix(Reduce(cbind,Rdf)) )
# for (risk in risks){
#   colnames(CombineRoR[[risk]]) = c(sapply(domains, function(dom) sapply(measures,function(mea) paste(dom,mea) )))
# }
for (risk in risks){
  test = c(1,3,3,3)
  names(test) = c("",domains)
  max_col = apply(CombineRoR[[risk]],2,max)

  CombineRoR[[risk]] %>%
    kbl(caption=paste0(as.numeric(risk)*100, "% Risk of return"), format="latex",align="r") %>%
    add_header_above(test) %>%
    column_spec(2,bold = CombineRoR[[risk]][,1] == max_col[1]) %>%
    column_spec(3,bold = CombineRoR[[risk]][,2] == max_col[2]) %>%
    column_spec(4,bold = CombineRoR[[risk]][,3] == max_col[3]) %>%
    column_spec(5,bold = CombineRoR[[risk]][,4] == max_col[4]) %>%
    column_spec(6,bold = CombineRoR[[risk]][,5] == max_col[5]) %>%
    column_spec(7,bold = CombineRoR[[risk]][,6] == max_col[6]) %>%
    column_spec(8,bold = CombineRoR[[risk]][,7] == max_col[7]) %>%
    column_spec(9,bold = CombineRoR[[risk]][,8] == max_col[8]) %>%
    column_spec(10,bold = CombineRoR[[risk]][,9] == max_col[9]) %>%
    cat(., file = paste0(TableFold,as.numeric(risk)*100,"RoR.txt"))
}
