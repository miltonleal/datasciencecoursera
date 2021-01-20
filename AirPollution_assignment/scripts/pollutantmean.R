#Written by Milton Leal - 2021/01/20

#Assignment from week 2 of R Programming course on Coursera from Johns Hopkins

#Write a function named 'pollutantmean' that calculates the mean of a 
#pollutant (sulfate or nitrate) across a specified list of monitors. 

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  #creates a vector that contains the name of every file we want to access
  file_list <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  
  #creates an empty vector
  data <- NA
  
  #loop through each file
  for (i in id) {
    
    #reads each file
    file_data <- read.csv(file_list[i])
    
    #gets the column data from each file and appends it to the "data" variable
    data <- c(data,file_data[[pollutant]])
  }
  
  #compute the mean
  mean(data, na.rm = TRUE)
  
}

## Tests

pollutantmean("/Users/admin/R_files/datasciencecoursera/AirPollution_assignment/specdata","sulfate",id=1:10)


pollutantmean("/Users/admin/R_files/datasciencecoursera/AirPollution_assignment/specdata","sulfate", id=1:332)
