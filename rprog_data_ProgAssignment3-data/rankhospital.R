#Written by Milton Leal

# Programming Assignment 3 - R Programming - Coursera


#Write a function called rankhospital that takes three arguments: 
#the 2-character abbreviated name of a state (state), an outcome (outcome), and 
#the ranking of a hospital in that state for that outcome (num).The function 
#reads the outcome-of-care-measures.csv file and returns a character vector with 
#the name of the hospital that has the ranking specified by the num argument.

rankhospital <- function(state, outcome, num = "best") {
  
  #reads the data
  outcome_best <- suppressWarnings(read.csv("outcome-of-care-measures.csv", 
                                               na.strings = "Not Available", 
                                               stringsAsFactors = FALSE))
  
  #checks if "state" input is valid
  if(!any(state == outcome_best$State)) {
    stop("invalid state")
  }
    
  #checks if "outcome" input is valid
  else if ((outcome %in% c("heart attack", "heart failure", 
                           "pneumonia")) == FALSE) {
    stop("invalid outcome")
  }
  
  #creates a subset according to the desired State
  hospitals_state <- subset(outcome_best, State == state)
  
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
  
  #creates data frame with only important columns
  final_subset <- subset(hospitals_state[c(2,7,colnum)])
  
  #removes NA's from data.frame
  final_subset <- final_subset[complete.cases(final_subset), ]
  
  #names data frame columns
  names(final_subset) <- c("hospital", "state", "outcome")
  
  #orders data frame by outcome and if there's a tie by hospital name
  final_subset <- final_subset[order(final_subset$outcome, 
                                     final_subset$hospital), ]
  
  #checks what's the desired rank and returns the name of the hospital
  
  if(num == "worst"){
    return (final_subset[nrow(final_subset), 1])
  }
  
  else if(num == "best"){
    return (final_subset[1, 1])
  }
  
  else {
    return (final_subset[num, 1])
  }
  
}
