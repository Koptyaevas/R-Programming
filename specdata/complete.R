complete <- function(directory, id = 1:332) {
  data <- vector(mode="character", length=length(id))
  for(i in seq_along(id)) {
    x <- id[i]
    id_string <- toString(x)
    if (x >= 1 && x <= 9) {  
      monitor <- paste("00", id_string, ".csv", sep="")
    }
    else if (x >= 10 && x <= 99) {
      id_string <- toString(x)
      monitor <- paste("0", id_string, ".csv", sep="")
    }
    else {
      id_string <- toString(x)
      monitor <- paste(id_string, ".csv", sep="") 
    }
    data[i] <- monitor      
   }
  
  #accumulator <- 0
  nobs <- vector(mode="integer", length=length(id)) 
  for(i in seq(data)) {
    airquality <- read.csv(data[i])
    good <- complete.cases(airquality)
    airquality <- airquality[good, ]
    nobs[i] <- nrow(airquality)
    #accumulator <- accumulator + sum(airquality[pollutant], na.rm = TRUE)
  }
  result = data.frame(id, nobs)
  result
}
