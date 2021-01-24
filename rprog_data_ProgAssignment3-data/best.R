#Written by Milton Leal

# Programming Assignment 3 - R Programming - Coursera

#Write a function called best that take two arguments: the 2-character 
#abbreviated name of a state and anoutcome name. The function reads the 
#outcome-of-care-measures.csv file and returns a character vector with the 
#name of the hospital that has the best (i.e. lowest) 30-day mortality 
#for the specified outcome in that state.

best <- function(state, outcome) {
  
  #reads the data
  outcome_best <- read.csv("outcome-of-care-measures.csv", 
                           colClasses = "character")
  
  #checks if "state" input is valid
  if(!any(state == outcome_best$State)) {
    stop("invalid state")
  }
  
  #checks if "outcome" input is valid
  else if ((outcome %in% c("heart attack", "heart failure", 
                           "pneumonia")) == FALSE) {
    stop("invalid outcome")
  }
  
  #subsets the data from the desired State
  outcome2 <- subset(outcome_best, State == state)
  
  #creates variable containing the index corresponding to the desired "outcome"
  
  if (outcome == "heart attack") {
    colnum <- 11
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else {
    colnum <- 23
  }
    
  #creates vector containing every entry that satisfies the criteria
  min_row <- suppressWarnings(which(as.numeric(outcome2[ ,colnum]) ==
                     min(as.numeric(outcome2[ ,colnum]), na.rm = TRUE)))
  
  #variable that store the names of the hospitals
  hospitals <- outcome2[min_row,2]
  #order the hospital names list
  hospitals <- sort(hospitals)
  #returns the first hospital on the list
  return(hospitals[1])

}