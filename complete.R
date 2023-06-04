print("Assignment Part 2 for Week2- R Coursera, JHU")
print(R.version.string)

complete <- function(directory, id = 1:332) {
 
  # file list
  files <- list.files(directory)
  
  # Create an empty data frame
  data <- data.frame()
  
  # For each file in the directory, read into df
  for (file in files) {
    df <- read.csv(file.path(directory, file))
    
    # number of complete cases, add them
    nobs <- sum(complete.cases(df[df$ID %in% id, ]))
    
    # add row to data frame with the file name and the number of complete cases
    data <- rbind(data, data.frame(id = file, nobs = nobs))
  }
  return(data[id, c("id", "nobs")])
}