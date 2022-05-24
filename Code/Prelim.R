# Set working directory as the main folder of the code
# Change to your respective directory location
setwd("~/Desktop/GITHUB/Rasr_submit")
rm(list=ls())
source("Code/Basic_Utils.R")
domains = c("riverswim","inventory","population" )
domains_source <- "http://data.rmdp.xyz/domains"     # no trailing "/"
domains_paths = wdir("Domains")
for (domain in domains){
  domain_path = file.path(domains_paths,domain)
  if (dir.exists(domain_path)) {
    cat("Domain:", domain, "available, using cached version.\n")
  } else {
    cat("Domain:", domain, "unavailable, downloading...\n")
    wdir(domain_path)
    withCallingHandlers({
      domain_files <- c("parameters.csv", "true.csv", "initial.csv", 
                        "training.csv","test.csv")
      for (dfile in domain_files) {
        urlf <- paste(domains_source, domain, dfile, sep = "/")
        targetf <- file.path(domain_path, dfile)
        cat("Downloading", urlf, "to", targetf, "\n")
        download.file(urlf, targetf)
      }
    }, 
    error = function(e){
      cat("Download error! Stopping.\n")
      unlink(domain_path, recursive = TRUE, force = TRUE)
      stop(e)
    })
  }
}



  
  