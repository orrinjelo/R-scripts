corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  n <- c()
  s <- c()
  cc <- c()
  for (idd in 1:332) {
    if (complete(directory,idd)[2] < threshold) 
    {
      next
    }
    spacer <- ''
    if (idd < 10) {
      spacer <- '00'
    } else if (idd < 100) {
      spacer <- '0'
    }
    
    setwd(directory)
    filename <- paste0(spacer,idd,'.csv')
    
    f <- file(filename,'r')
    data <- read.csv(f)
    close(f)
    setwd('..')
    valids <- c((is.na(data[,'nitrate']) | is.na(data[,'sulfate'])))
    n <- c(n,data[,'nitrate'][!valids])
    s <- c(s,data[,'sulfate'][!valids])
    cc <- c(cc,cor(n,s))
  }
  
  cc
}