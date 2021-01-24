#Written by Milton Leal

# Programming Assignment 3 - R Programming - Coursera

# Write a function called rankall that takes two arguments: an outcome name 
#(outcome) and a hospital ranking (num). The function reads the 
#outcome-of-care-measures.csv file and returns a 2-column data frame 
#containing the hospital in each state that has the ranking specified in num. 

rankall <- function(outcome, num = "best") {
  
  #reads the data
  outcome_best <- suppressWarnings(read.csv("outcome-of-care-measures.csv", 
                                            na.strings = "Not Available", 
                                            stringsAsFactors = FALSE))
  
  #checks if "outcome" input is valid
  if ((outcome %in% c("heart attack", "heart failure", 
                           "pneumonia")) == FALSE) {
    stop("invalid outcome")
  }
  
  #creates variable that stores the number of the column related to outcome
  if (outcome == "heart attack") {
    colnum <- 11
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else {
    colnum <- 23
  }
  
  #cleans the data and gets only the three important columns
  three_column_subset <- subset(outcome_best[c(2,7,colnum)])
  
  #names the columns
  names(three_column_subset) <- c("hospital", "state", "outcome")
  
  #removes NA's
  three_column_subset <- three_column_subset[complete.cases(three_column_subset), ]
  
  #orders data frame by state, outcome and hospital name, taking care of ties
  final_subset <- three_column_subset[order(three_column_subset$state, 
                                            three_column_subset$outcome,
                                            three_column_subset$hospital), ]
  
  #creates a list of data frames, being one for every state
  new <- split(final_subset, final_subset$state)
  
  #creates a new data frame
  df <- data.frame()
  
  #check which case is being asked
  if(num == "worst") {
      
      #loops through every state
      for (i in 1:54) {
        
        #appends the final data frame
        df <- rbind(df, c(new[[i]][nrow(new[[i]]),1],new[[i]][1,2]))
      }  
  }
    
  else if(num == "best"){
    
    for (i in 1:54){
      
      df <- rbind(df, c(new[[i]][1,1],new[[i]][1,2]))
    }
  }
    
  else {
    for (i in 1:54){
      
      df <- rbind(df, c(new[[i]][num,1],new[[i]][1,2]))
    }
  }
  
  #names the final data frame columns
  names(df) <- c("hospital", "state")
  
  return (df)
  
}