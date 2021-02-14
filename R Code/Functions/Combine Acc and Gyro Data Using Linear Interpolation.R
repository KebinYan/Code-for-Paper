combineData <- function(AccData, GyroData){
  gyroMax <- max(GyroData$x, GyroData$y, GyroData$z)
  gyroMin <- min(GyroData$x, GyroData$y, GyroData$z)
  
  combinedData <- data.frame(rep(NA, nrow(AccData)))
  combinedData$acc.timestamp <- AccData$timestamp
  combinedData$acc.x <- AccData$x
  combinedData$acc.y <- AccData$y
  combinedData$acc.z <- AccData$z
  combinedData$label <- AccData$label
  combinedData <- combinedData[,-1]
  
  for (j in 1:nrow(AccData)){
    for(i in 1:(nrow(GyroData)-1)){
      if (GyroData$timestamp[i] <= AccData$timestamp[j] & GyroData$timestamp[i+1] >= AccData$timestamp[j]){
        combinedData$gyro.x[j] <- approx(GyroData$timestamp[c(i,i+1)], GyroData$x[c(i,i+1)], xout = AccData$timestamp[j], method = "linear")$y
      }
    }
  }
  
  for (j in 1:nrow(AccData)){
    for(i in 1:(nrow(GyroData)-1)){
      if (GyroData$timestamp[i] <= AccData$timestamp[j] & GyroData$timestamp[i+1] >= AccData$timestamp[j]){
        combinedData$gyro.y[j] <- approx(GyroData$timestamp[c(i,i+1)], GyroData$y[c(i,i+1)], xout = AccData$timestamp[j], method = "linear")$y
      }
    }
  }
  
  for (j in 1:nrow(AccData)){
    for(i in 1:(nrow(GyroData)-1)){
      if (GyroData$timestamp[i] <= AccData$timestamp[j] & GyroData$timestamp[i+1] >= AccData$timestamp[j]){
        combinedData$gyro.z[j] <- approx(GyroData$timestamp[c(i,i+1)], GyroData$z[c(i,i+1)], xout = AccData$timestamp[j], method = "linear")$y
      }
    }
  }
  return(combinedData)
}
