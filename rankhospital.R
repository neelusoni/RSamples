rankhospital <- function(state, outcome, num = "best"){
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
    numberhosp <- nrow(outcomestate)
    
    if (num == "best"){num <- 1}
    else if (num == "worst"){num <- numberhosp}
      
    orderstate <- data.frame(hospname=character(),rate=numeric(),rank=numeric())
    if (num <= numberhosp)
    {
      ## Return hospital name in that state with the given rank
      if (outcome == 'heart attack')
      {
        orderstate[,1] <- outcomestate[,2]  
        orderstate$rate <- as.numeric(outcomestate[,11])
        #orderstate <- na.omit(orderstate[order(-orderstate[,2],orderstate[,1]),])
      }
      else if (outcome == 'heart failure'){
          orderstate <- na.omit(outcomestate[order(-as.numeric(outcomestate[,17]),outcomestate[,2]),])[,c(2,17)]
      }
      else{
          orderstate <- na.omit(outcomestate[order(-as.numeric(outcomestate[,23]),outcomestate[,2]),])[,c(2,23)]
      }
      #orderstate <- orderstate[complete.cases(as.numeric(orderstate[,2]),]
      orderstate["Rank"] <- NA
      orderstate$Rank <- c(1:numberhosp)
      orderstate
      #orderstate[which(orderstate$Rank == num),][,1]
    }
    else
      NA
  }
}