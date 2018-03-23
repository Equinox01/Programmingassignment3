## This function takes three arguments: the 2-character abbreviated name of a
## state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
## The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument.

rankhospital <- function(state,outcome,num="best") {
  
  #read outcome-of-care-measures.csv
  hospitaldata <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = 'Not Available')[ ,c(2,7,11,17,23)]
  
  ## remove "Not Available"
  hospitaldata <- na.omit(hospitaldata) 
  
  ## rename columns
  names(hospitaldata)[3]<-"Heart_Attack"
  names(hospitaldata)[4]<-"Heart_Failure"
  names(hospitaldata)[5]<-"Pneumonia"
  
  #check valid state
  validstates <- unique(hospitaldata$State)
  if (!state %in% validstates) { stop(print("invalid state"))
  } else {
    #check valid outcome
    validoutcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% validoutcomes) {stop(print("invalid outcome"))
    } else {
      #select data for state
      if (outcome == "heart attack") { colnum=3
      } else {
        if (outcome == "heart failure") { 
          colnum=4
        } else {
          colnum=5 }
      }
      statedata <- hospitaldata[which(hospitaldata$State == state),c(1,2,colnum)]
      colnames(statedata) <- c("Hospital.Name","State","Rate")
      if (num == "best") {
        ranks <- statedata[which(statedata$Rate == min(statedata$Rate)),]
        result <- (ranks$Hospital.Name)
      } else {
        if(num == "worst") {
          ranks <- statedata[which(statedata$Rate == max(statedata$Rate)),]
          result <- (ranks$Hospital.Name)
        }  else {
          ranks <- statedata[which(statedata$State == state),]
          if (nrow(ranks) < num) {
            result <- "NA" 
          } else {
            ranks <- cbind(ranks[order(ranks[,3],ranks[,1]),],1:nrow(ranks))
            result <- (ranks$Hospital.Name[num])}
        }
      }
    }
  }   
  return(result)
}


