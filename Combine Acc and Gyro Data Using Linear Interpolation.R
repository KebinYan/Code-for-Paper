
GyroData <- read.csv("~/Desktop/WFU/URECA/Huang's Data/Data/newGyroData.csv")
AccData <- read.csv("~/Desktop/WFU/URECA/Huang's Data/Data/newAccData.csv")

gyroMax <- max(GyroData$x, GyroData$y, GyroData$z)
gyroMin <- min(GyroData$x, GyroData$y, GyroData$z)

combinedData <- data.frame(rep(NA, nrow(AccData)))
combinedData$acc.timeElapsed <- AccData$timeElapsed
combinedData$acc.x <- AccData$x
combinedData$acc.y <- AccData$y
combinedData$acc.z <- AccData$z
combinedData$label <- AccData$label
combinedData <- combinedData[,-1]

for (j in 1:nrow(AccData)){
  for(i in 1:(nrow(GyroData)-1)){
    if (GyroData$timeElapsed[i] <= AccData$timeElapsed[j] & GyroData$timeElapsed[i+1] >= AccData$timeElapsed[j]){
      combinedData$gyro.x[j] <- approx(GyroData$timeElapsed[c(i,i+1)], GyroData$x[c(i,i+1)], xout = AccData$timeElapsed[j], method = "linear")$y
    }
  }
}

for (j in 1:nrow(AccData)){
  for(i in 1:(nrow(GyroData)-1)){
    if (GyroData$timeElapsed[i] <= AccData$timeElapsed[j] & GyroData$timeElapsed[i+1] >= AccData$timeElapsed[j]){
      combinedData$gyro.y[j] <- approx(GyroData$timeElapsed[c(i,i+1)], GyroData$y[c(i,i+1)], xout = AccData$timeElapsed[j], method = "linear")$y
    }
  }
}

for (j in 1:nrow(AccData)){
  for(i in 1:(nrow(GyroData)-1)){
    if (GyroData$timeElapsed[i] <= AccData$timeElapsed[j] & GyroData$timeElapsed[i+1] >= AccData$timeElapsed[j]){
      combinedData$gyro.z[j] <- approx(GyroData$timeElapsed[c(i,i+1)], GyroData$z[c(i,i+1)], xout = AccData$timeElapsed[j], method = "linear")$y
    }
  }
}