best <- function(state,outcome) {   
    validOutcomes <- c("HEART ATTACK","HEART FAILURE","PNEUMONIA")
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    o = toupper(trim(outcome))
    s = toupper(trim(state))
    a = function() {
          outcomeSkinny <- outcomeSkinny[!is.na(outcomeSkinny$attack),]
          head(outcomeSkinny[order(outcomeSkinny$attack,outcomeSkinny$hospitalName),],1)
      }
    f = function() {
          outcomeSkinny <- subset(outcomeSkinny,!is.na(outcomeSkinny$failure))
          head(outcomeSkinny[order(outcomeSkinny$failure,outcomeSkinny$hospitalName),],1)
      }
    p = function() {
          outcomeSkinny <- outcomeSkinny[!is.na(outcomeSkinny$pneumonia),]
          head(outcomeSkinny[order(outcomeSkinny$pneumonia,outcomeSkinny$hospitalName),],1)
      }
    if (o %in% validOutcomes) {
       ip <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings = 'Not Available')
       validStates <- unique(ip[,7])
       if(s %in% validStates) { 
           # state is [,7], heart attack [,11] heart failure  [,17] pneumonia” [,23] hospital name is [,2] 
           outcomeSkinny <- ip[,c(2,7,11,17,23)]
           ip <- NULL
           names(outcomeSkinny) <- c("hospitalName","st","attack","failure","pneumonia")
           outcomeSkinny <- subset(outcomeSkinny,outcomeSkinny$st == s)
           outcomeSkinny$attack    <- as.numeric(as.character(outcomeSkinny$attack))
           outcomeSkinny$failure   <- as.numeric(as.character(outcomeSkinny$failure))
           outcomeSkinny$pneumonia <- as.numeric(as.character(outcomeSkinny$pneumonia))
           if(o == "HEART ATTACK")  fullrow <- a() 
           if(o == "HEART FAILURE") fullrow <- f()
           if(o == "PNEUMONIA")     fullrow <- p()            
           outcomeSkinny <- NULL
           fullrow[,1]
     } else {
           stop("invalid state")
    }
  } else {
       stop("invalid outcome")
  }
}
