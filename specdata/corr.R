corr <- function(directory, threshold = 0) {
  id = 1:332
  filename <- vector(mode="character", length=length(id))
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
    filename[i] <- monitor      
  }
  result <-vector(mode="numeric", length=0)
  for(i in seq(filename)) {
    airquality <- read.csv(filename[i])
    good <- complete.cases(airquality)
    airquality <- airquality[good, ]
    if (nrow(airquality) > threshold) {
      correlation <- cor(airquality[["sulfate"]], airquality[["nitrate"]])
      result <- append(result, correlation)
    }
  }
  result
}
