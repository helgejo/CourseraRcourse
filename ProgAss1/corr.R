corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  #id <- 1:332
  files_list <- list.files(directory, full.names = TRUE)
  
  #answer <- complete(directory)
  #answer <- answer[complete.cases(answer),]
  out <- vector(mode="numeric", length=0)
  for(file in files_list){
    dat <- read.csv(file)
    nobs <- sum(complete.cases(dat))
    if(nobs > threshold){
      print(paste("file:", file))
      out <- rbind(out, cor(dat$nitrate, dat$sulfate, use = "complete.obs"))
      } 
    #else {
     # out <- rbind(out, 0)
    #  }
    }
  out <- as.vector(out)
  #out <- unlist(out[!sapply(out, is.null)])
  return(out)
  
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
}