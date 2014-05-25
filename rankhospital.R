rankhospital <- function(state, outcome, num = "best") {   
    validOutcomes <- c("HEART ATTACK","HEART FAILURE","PNEUMONIA")
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    o = toupper(trim(outcome))
    s = toupper(trim(state))
    n = toupper(trim(num))
    a = function() {
        outcomeSkinny <- outcomeSkinny[!is.na(outcomeSkinny$attack),]
        outcomeSkinny[order(outcomeSkinny$attack,outcomeSkinny$hospitalName),]
    }
    f = function() {
        outcomeSkinny <- outcomeSkinny[!is.na(outcomeSkinny$failure),]
        outcomeSkinny[order(outcomeSkinny$failure,outcomeSkinny$hospitalName),]
    }
    p = function() {
        outcomeSkinny <- outcomeSkinny[!is.na(outcomeSkinny$pneumonia),]
        outcomeSkinny[order(outcomeSkinny$pneumonia,outcomeSkinny$hospitalName),]
    }
    if (o %in% validOutcomes) {
        ip <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings = 'Not Available')
        validStates <- unique(ip[,7])
        if(s %in% validStates) { 
            # state is [,7], heart attack [,11] heart failure  [,17] pneumonia” [,23] hospital name is [,2] 
            outcomeSkinny <- ip[,c(2,7,11,17,23)]
            ip <- NULL
            names(outcomeSkinny)    <- c("hospitalName","st","attack","failure","pneumonia")
            outcomeSkinny           <- subset(outcomeSkinny,outcomeSkinny$st == s)
            outcomeSkinny$attack    <- as.numeric(as.character(outcomeSkinny$attack))
            outcomeSkinny$failure   <- as.numeric(as.character(outcomeSkinny$failure))
            outcomeSkinny$pneumonia <- as.numeric(as.character(outcomeSkinny$pneumonia))
            if(o == "HEART ATTACK")  fullrows <- a() 
            if(o == "HEART FAILURE") fullrows <- f()
            if(o == "PNEUMONIA")     fullrows <- p()            
            outcomeSkinny <- NULL
            l <- nrow(fullrows)
            id <- c(1:l)
            fullrows <- cbind(id,fullrows)
            if(n == "BEST") {
                fullrows[1,2]
            } else if(n == "WORST") {
                fullrows[l,2]
            } else if(num > l) {
                return(NA)
            } else fullrows[as.numeric(n),2]
        } else {
            stop("invalid state")
        }
    } else {
        stop("invalid outcome")
    }
}
