#Written by Milton Leal - 2021/01/20

#Assignment from week 2 of R Programming course on Coursera from Johns Hopkins

#Write a function that reads a directory full of files and reports the number 
#of completely observed cases in each data file.

complete <- function(directory, id = 1:332) {
  
  #sets the right path where to look for files
  directory <- paste(getwd(),"/", directory,"/", sep = "")
  
  #creates a list with the file names in it
  file_list <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  
  #initializes a variable that will contain the number of complete observations
  nobs <- numeric()
  
  #loop through the files
  for (i in id) {
    
    
    data <- read.csv(file_list[i])
    
    #appends to nobs the number of complete cases in each file
    nobs <- c(nobs, sum(complete.cases(data)))
  }
  
  #creates the data frame
  data.frame(id, nobs)
  
}
