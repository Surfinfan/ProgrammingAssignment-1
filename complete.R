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
     
     
     resumen <- data.frame(id=numeric(), nobs=numeric())
     
     for (sensor in id) {
          sensor_txt <- paste(sprintf("%03i", sensor), "csv", sep = ".")
          sensor_txt <- paste (directory, sensor_txt, sep = "/")
          mediciones <- read.csv(sensor_txt)
          buenas <- sum(complete.cases(mediciones))
          lineaadd <- data.frame(id=sensor, nobs=buenas)
          resumen <- rbind(resumen, lineaadd)
     }
     
     return (resumen)
     
}
