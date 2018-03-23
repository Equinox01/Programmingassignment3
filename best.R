## This function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best 30-day mortality for the specified outcome
## in that state.

best <- function(state,outcome) {
 
  #read outcome-of-care-measures.csv
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = 'Not Available')[ ,c(2,7,11,17,23)]

  ## remove "Not Available"
  data <- na.omit(data) 
  
  ## rename columns
  names(data)[3]<-"Heart_Attack"
  names(data)[4]<-"Heart_Failure"
  names(data)[5]<-"Pneumonia"

  #check valid state
  validstates <- unique(data$State)
  if (!state %in% validstates) stop(print("invalid state"))
  else
    
    #check valid outcome
    validoutcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% validoutcomes) stop(print("invalid outcome"))

  #select data for state
  statedata <- data[which(data$State == state),] 

  ##return hopital name in that state with lowest 30-day death
  if (outcome == "heart attack") {
      besthospital <- sort(statedata[which(statedata$Heart_Attack == min(statedata$Heart_Attack)),1])}
      else if (outcome == "heart failure") {
          besthospital <- sort(statedata[which(statedata$Heart_Failure == min(statedata$Heart_Failure)),1])}
            else {
              besthospital <- sort(statedata[which(statedata$Pneumonia == min(statedata$Pneumonia)),1])}
  besthospital <- besthospital[1]
  return(besthospital)
} 