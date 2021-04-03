combineData <- function(AccData, GyroData){
  
  names(AccData)[names(AccData) == "timeElapsed"] <- "timestamp"
  names(GyroData)[names(GyroData) == "timeElapsed"] <- "timestamp"
  
  combinedData <- data.frame(rep(NA, nrow(AccData)))
  combinedData$acc.timestamp <- AccData$timestamp
  combinedData$acc.x <- AccData$x
  combinedData$acc.y <- AccData$y
  combinedData$acc.z <- AccData$z
  combinedData$label <- AccData$label
  combinedData <- combinedData[,-1]
  combinedData$gyro.x <- NA
  combinedData$gyro.y <- NA
  combinedData$gyro.z <- NA
  
  for (j in 1:nrow(AccData)){
    for(i in 1:(nrow(GyroData)-1)){
      if (GyroData$timestamp[i] <= AccData$timestamp[j] & GyroData$timestamp[i+1] >= AccData$timestamp[j]){
        combinedData$gyro.x[j] <- approx(GyroData$timestamp[c(i,i+1)], GyroData$x[c(i,i+1)], xout = AccData$timestamp[j], method = "linear")$y
        combinedData$gyro.y[j] <- approx(GyroData$timestamp[c(i,i+1)], GyroData$y[c(i,i+1)], xout = AccData$timestamp[j], method = "linear")$y
        combinedData$gyro.z[j] <- approx(GyroData$timestamp[c(i,i+1)], GyroData$z[c(i,i+1)], xout = AccData$timestamp[j], method = "linear")$y
      }
    }
  }
  
  names(combinedData) <- c("timestamp", "acc.x", "acc.y", "acc.z","label", "gyro.x", "gyro.y", "gyro.z")
  
  return(combinedData)
}
