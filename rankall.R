rankall <- function(outcome , num = "best")
{
  
  #reading the data and remove 'Not Available' 
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = 'Not Available')[ ,c(2,7,11,17,23)]
 
  
  ## rename columns
  names(data)[3]<-"Heart_Attack"
  names(data)[4]<-"Heart_Failure"
  names(data)[5]<-"Pneumonia"
  
  #check the input parameters
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
  }
  #keep only the relevant columns and remove NA  
  data <- data[c(1,2,colnum)]
  data <- na.omit(data) 
  colnames(data) <- c("Hospital.Name","State","Rate")
  
  #Sorting data frame on State, Rate and HospitalName
  data <- data[order(data[,2],data[,3],data[,1]) , ]   

  #Create a list for each state
  statelist = split(data, data$State)
  
  #For each state apply the rankall arguments
  result = lapply(statelist, function(x, num) {
  
    if(class(num) == "character") {
      if(num == "best") {
        return (x$Hospital.Name[1])
      }
      else if(num == "worst") {
        return (x$Hospital.Name[nrow(x)])
      }
    }
    else {
      return (x$Hospital.Name[num])
    }
  }, num)
  
  #Return data.frame with format
  return ( data.frame(hospital=unlist(result), state=names(result)) )
}