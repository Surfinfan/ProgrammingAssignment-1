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
  
  if ((pollutant != "sulfate") && (pollutant != "nitrate" )) {
    return ("ERROR")
  }
  
  todas = vector()
  
  for (sensor in id) {
    sensor_txt = paste(sprintf("%03i", sensor), "csv", sep = ".")
    sensor = paste (directory, sensor_txt, sep = "/")
    mediciones <- read.csv(sensor)
    todas <- rbind (todas, mediciones[pollutant])
  }
  
  los_ok = todas[!is.na(todas)]
  media_total = mean(los_ok)
  
  return(media_total)
}

