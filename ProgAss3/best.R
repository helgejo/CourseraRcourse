best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
    states <- unique(data[,7])
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    index <- c(11,17,23)

    ## Check that state and outcome are valid
    if(!state %in% states){stop("invalid state")}
    if(!outcome %in% outcomes){stop("invalid outcome")}
    #print(state)
    
    col <- match(outcome, outcomes)
    col <- index[col]
    
    ## Return hospital name 2 with lowest 30-day death rate for outcome in the state
    ##  The outcomes can be one of ???heart attack??? 11, ???heart failure??? 17, or ???pneumonia??? 23
    state <- data[data$State==state,]
    row <- which.min(as.double(state[,col]))
    return <- state[row,"Hospital.Name"]
}