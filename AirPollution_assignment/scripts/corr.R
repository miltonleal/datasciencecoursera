#Written by Milton Leal - 2021/01/20

#Assignment from week 2 of R Programming course on Coursera from Johns Hopkins

#Write a function that takes a directory of data files and a threshold for 
#complete cases and calculates the correlation between sulfate and nitrate for 
#monitor locations where the number of completely observed cases (on all 
#variables) is greater than the threshold.

corr <- function (directory, threshold = 0) {
  
  #creates a list with the file names in it
  file_list <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  
  #creates a list with the total complete cases per file
  only_complete <- complete(directory)
  
  #checks if the file meets the threshold and keeps the id
  ids <- only_complete[only_complete["nobs"] > threshold, ]$id
  
  #initializes a variable that will contain the correlations
  corr_vector <- numeric()
  
  #loop through the files
  for (i in ids) {
    
    #copies the data from the csv file
    data <- read.csv(file_list[i])
    #gets only the complete cases from each file
    data_aux <- data[complete.cases(data), ]
    #computes the correlation between the desired variables
    corr_vector <- c(corr_vector, cor(data_aux[["sulfate"]], data_aux[["nitrate"]]))
    
  }
  
  return(corr_vector)
  
}
  