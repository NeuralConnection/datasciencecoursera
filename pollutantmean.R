print("Assignment Part 1 for Week2 - R Coursera, JHU")
print(R.version.string)

pollutantmean <- function(directory, pollutant, id = 1:332) {
   # get file list and create empty dataframe
   files <- list.files(directory)
   data <- data.frame()
  
  # FOR each file in the directory, read CSV into the df
  for (file in files) {
      df <- read.csv(file.path(directory, file))
    
    # error I was getting -- convert the df ID column>numeric 
    df$ID <- as.numeric(df$ID)
    
    if (any(sapply(id, function(x) x %in% df$ID))) {
        data <- rbind(data, df)  # add the data frame to the df
    }
  }
  
    # calculate mean you specify on pollutant
  mean(data[[pollutant]], na.rm = TRUE)
}