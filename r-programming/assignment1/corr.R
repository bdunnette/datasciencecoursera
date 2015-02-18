corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  dat <- data.frame()
  
  for(i in id){
    filename <- file.path(directory, paste(formatC(i, width = 3, flag = "0"), "csv", sep="."))
    dat <- rbind(dat, read.csv(filename, header=TRUE))
  }
  
  ## Return a numeric vector of correlations
  return(corr)
}