best <- function(state,outcome){
  ## Read outcome data
  outcomedf <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!is.element(outcome,c('heart attack','heart failure','pneumonia'))){
      print ('Invalid outcome requested')
  }
  else if (!is.element(state, unique(outcomedf[,7]))){
      print ('Invalid state requested')
  }
  else
  {
    outcomestate <- outcomedf[outcomedf$State == state,]
  
    ## Return hospital name in that state with lowest 30-day death
    if (outcome == 'heart attack')
    {
      outcomestate[which(as.numeric(outcomestate[,11]) == min(as.numeric(outcomestate[,11]),na.rm=TRUE)),][,2]
    }
    else if (outcome == 'heart failure'){
      outcomestate[which(as.numeric(outcomestate[,17]) == min(as.numeric(outcomestate[,17]),na.rm=TRUE)),][,2]
    }
    else{
      outcomestate[which(as.numeric(outcomestate[,23]) == min(as.numeric(outcomestate[,23]),na.rm=TRUE)),][,2]
    }
  }
}