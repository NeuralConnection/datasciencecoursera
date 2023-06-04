print("Assignment Part 3 for Week2- R Coursera, JHU")
print(R.version.string)

corr <- function(directory, threshold = 0) {
  
  # get file list and c define correlation vector to numeric
  files <- list.files(directory)
  correlations <- numeric()
  
  # FOR each file in the directory & read csv into df
  for (file in files) {
      df <- read.csv(file.path(directory, file))
    
    # get the number of complete cases in df
    nobs <- sum(complete.cases(df))
    
    # IF the number of complete cases is greater than the threshold,
      #calc the corr between sulfate & nitrate
    if (nobs > threshold) {
      correlations <- c(correlations, cor(df$sulfate, df$nitrate, use="complete.obs"))
    }
  }
  
  #vector of correlations
  return(correlations)
}