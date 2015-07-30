rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
        
        
        ## Check that outcome is valid
        states <- unique(data[,7])
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        index <- c(11,17,23)
        rank <- 1
        
        ## Check that state and outcome are valid
        ## if(!state %in% states){stop("invalid state")}
        if(!outcome %in% outcomes){stop("invalid outcome")}
        #print(state)
        
        col <- match(outcome, outcomes)
        col <- index[col]
        df <- data.frame(hospital=character(), state=character())
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        for(state in states) {
                state.data <- data[data$State==state,c(2,col)]
                state.ordered <- state.data[order(as.numeric(state.data[,2]), state.data[,1],na.last = NA, decreasing = FALSE),]
                
                checkNum <- length(state.ordered[,2])
                if(num == "worst") {rank <- checkNum}
                else if(num == "best") {rank <- 1}
                else if(num > checkNum) {rank <- 0}
                else {rank <- num}
                
                if(rank > 0){df <- rbind(df, cbind(state.ordered[rank,1],state))}
                else {df <- rbind(df, cbind(NA,state))}
        }
        df1 <- df[order(df[,2], decreasing = FALSE),]
        colnames(df1) <- c("hospital", "state")
        return(df1)
        
        ##The first column in the data frame is named hospital, which contains
        ##the hospital name, and the second column is named state
}