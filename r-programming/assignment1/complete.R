complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  dat <- data.frame()
  
  for(i in id){
    filename <- file.path(directory, paste(formatC(i, width = 3, flag = "0"), "csv", sep="."))
    obs <- read.csv(filename, header=TRUE)
    good <- sum(complete.cases(obs))
    dat <- rbind(dat, c(i, good))
  }
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  colnames(dat) <- c("id", "nobs")
  return(dat)
}