corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  corr <- numeric()
  
  file.list <- list.files(path=directory, full.names=TRUE)
  
  for(filename in file.list){
    obs <- read.csv(filename, header=TRUE)
    good <- complete.cases(obs)
    if(sum(good) > threshold){
      complete <- obs[good,]
      corr <- c(corr, cor(complete[,"sulfate"], complete[,"nitrate"]))
    }
  }
  
  ## Return a numeric vector of correlations
  return(corr)
}