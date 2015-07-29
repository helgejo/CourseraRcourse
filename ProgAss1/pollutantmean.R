
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
    files_list <- list.files(directory, full.names = TRUE)
    dat <- data.frame()
    for (i in files_list) {
      dat <- rbind(dat, read.csv(i))
    }
    
    dat_sel <- dat[which(dat$ID %in% id),]
    dat_subset <- dat_sel[pollutant]
    answer <- as.double(colMeans(dat_subset,na.rm = TRUE))
    return(answer)
    
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
}
