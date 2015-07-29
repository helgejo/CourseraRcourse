rankhospital <- function(state, outcome, num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
    states <- unique(data[,7])
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    index <- c(11,17,23)
    rank <- 1
    
    ## Check that state and outcome are valid
    if(!state %in% states){stop("invalid state")}
    if(!outcome %in% outcomes){stop("invalid outcome")}
    #print(state)
    
    col <- match(outcome, outcomes)
    col <- index[col]
    
    ## Check that num is not > numbers of hospital in that state. if it is the return NA.
    state <- data[data$State==state,]
    checkNum <- length(unique(state[,"Hospital.Name"]))
    print(checkNum)
    if(num == "worst") {rank <- checkNum}
    else if(num > checkNum) {stop("NA")}
    else {rank <- num}
    
    ## Return hospital name in that state with the given rank 30-day death rate
    ## Use order function and order first on rate then on name
    
}