complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  ff <- c()
  v <- c()
  valids <- 1
  for (idd in id) {
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
    valids <- c(1,(is.na(data[,'nitrate']) | is.na(data[,'sulfate'])))
    v <- c(v,sum(!valids))
    ff <- c(ff,idd)
  }
  df <- data.frame(ff,v)
  names(df) <- c('id','nobs')
  df
}