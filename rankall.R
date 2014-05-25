rankall <- function(outcome, num = "best") {   
    validOutcomes <- c("HEART ATTACK","HEART FAILURE","PNEUMONIA")
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    o = toupper(trim(outcome))
    n = toupper(trim(num))
    a = function(dat,pos,state) {
        outcomeSkinny         <- subset(dat,!is.na(dat$attack))
        outcomeSkinny         <- outcomeSkinny[order(outcomeSkinny$attack,outcomeSkinny$hospitalName),]
        f_op                  <- outcomeSkinny[,1:2]         
        cnt                   <- nrow(f_op)
        if(pos == -1) {
           pos <- cnt    
        } 
        if(cnt < pos) {
            f_op[pos,'hospitalName'] <- '<NA>'
            f_op[pos,'st']           <- state
        }
        return(f_op[pos,])
    }
    f = function(dat,pos,state) {
        outcomeSkinny         <- subset(dat,!is.na(dat$failure))
        outcomeSkinny         <- outcomeSkinny[order(outcomeSkinny$failure,outcomeSkinny$hospitalName),]
        f_op                  <- outcomeSkinny[,1:2]         
        cnt                   <- nrow(f_op)
        if(pos == -1) {
            pos <- cnt    
        } 
        if(cnt < pos) {
            f_op[pos,'hospitalName'] <- '<NA>'
            f_op[pos,'st']           <- state
        }
        return(f_op[pos,])
    }
    p = function(dat,pos,state) {
        outcomeSkinny         <- subset(dat,!is.na(dat$pneumonia))
        outcomeSkinny         <- outcomeSkinny[order(outcomeSkinny$pneumonia,outcomeSkinny$hospitalName),]
        f_op                  <- outcomeSkinny[,1:2]         
        cnt                   <- nrow(f_op)
        if(pos == -1) {
            pos <- cnt    
        } 
        if(cnt < pos) {
            f_op[pos,'hospitalName'] <- '<NA>'
            f_op[pos,'st']           <- state
        }
        return(f_op[pos,])
    }
        
        if (o %in% validOutcomes) {
        ip <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings = c('Not Available','<NA>'))
        validStates <- unique(ip[,7])
        stateCnt    <- length(validStates)
        # state is [,7], heart attack [,11] heart failure  [,17] pneumonia” [,23] hospital name is [,2] 
        outcomeSkinny <- ip[,c(2,7,11,17,23)]
        id <- c(1:nrow(ip))
        ip <- NULL        
        op <- data.frame(hostpitalName = character(), st = character())
        names(outcomeSkinny)    <- c("hospitalName","st","attack","failure","pneumonia")
        outcomeSkinny           <- outcomeSkinny[order(outcomeSkinny$hospitalName),]
        outcomeSkinny$attack    <- as.numeric(as.character(outcomeSkinny$attack))
        outcomeSkinny$failure   <- as.numeric(as.character(outcomeSkinny$failure))
        outcomeSkinny$pneumonia <- as.numeric(as.character(outcomeSkinny$pneumonia))
        outcomeSkinny$st        <- as.factor(outcomeSkinny$st)
        for (i in 1:stateCnt){
            outcomeSubset <- subset(outcomeSkinny,st == validStates[i])
            pos <- if(n == "BEST") 1 else if(n == "WORST") -1 else as.numeric(n)
            if(o == "HEART ATTACK")  op <- rbind(op,a(outcomeSubset,pos,validStates[i]))
            if(o == "HEART FAILURE") op <- rbind(op,f(outcomeSubset,pos,validStates[i]))
            if(o == "PNEUMONIA")     op <- rbind(op,p(outcomeSubset,pos,validStates[i]))           
        }
        outcomeSkinny <- NULL
    } else {
        stop("invalid outcome")
    }
    names(op) <- c('hospital','state')
    return(op[order(op$state),])
}
