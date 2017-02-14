pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files

  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  v <- c()
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
    valids <- is.na(data[,pollutant])
    v <- c(v,data[,pollutant][!valids])
  }
  mean(v)
}