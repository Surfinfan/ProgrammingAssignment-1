corr <- function(directory, threshold = 0) {
     ## 'directory' is a character vector of length 1 indicating
     ## the location of the CSV files
     
     ## 'threshold' is a numeric vector of length 1 indicating the
     ## number of completely observed observations (on all
     ## variables) required to compute the correlation between
     ## nitrate and sulfate; the default is 0
     
     ## Return a numeric vector of correlations
     
     correlaciones <- vector()
     ficheros <- dir(directory)
     
     for (fichero in ficheros) {
          fichero <- paste (directory, fichero, sep = "/")
          mediciones <- read.csv(fichero)
          buenas <- complete.cases(mediciones)
          if (sum(buenas) < threshold) {
               next
          }
          
          colsulfate <- mediciones[buenas, "sulfate"]
          colnitrate <- mediciones[buenas, "nitrate"]
          valoradd <- cor(colnitrate, colsulfate)
          correlaciones = c(correlaciones, valoradd)
     }
     
     return (correlaciones)
     
}
