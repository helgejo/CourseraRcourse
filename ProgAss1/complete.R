complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    files_list <- list.files(directory, full.names = TRUE)
    dat <- data.frame()
    print("Getting data, please wait...")
    for (i in files_list) {
        print(paste("file" , i))
        dat <- rbind(dat, read.csv(i))
    }
    print("Completed get. Preparing data")
    dat_sel <- dat[which(dat$ID %in% id),]
    
    dat_subset <- dat_sel[complete.cases(dat_sel),]
    answer <- data.frame(table(dat_subset$ID))
    colnames(answer) <- c("id", "nobs")
    #answer <- answer[match(answer$id, id),]
    return(answer)
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
}