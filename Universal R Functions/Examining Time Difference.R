# calculate the time difference between each gyroscope timestamp and the closest accelerometer timestamp
CalculateTimeDifference <- function(accData, gyroData){
  time_difference <- as.data.frame(cbind(gyroData$timestamp, rep(NA,nrow(gyroData)), rep(NA,nrow(gyroData))))
  colnames(time_difference) <- c("gyro_timestamp","acc_timestamp","time_difference")
  
  for (i in 1:nrow(gyroData)){
    temp_diff <- NA
    temp_diff <- as.data.frame(rep(NA, nrow(accData)))
    for(j in 1:nrow(accData)){
      temp_diff[j,1] <- abs(gyroData$timestamp[i]-accData$timestamp[j])
    }
    time_difference[i,3] <- min(temp_diff)
    time_difference[i,2] <- accData$timestamp[which.min(unlist(temp_diff))]
  }
  
  return(time_difference)
}

# plot the time difference for all data points
GraphTimeDifference <- function(time_difference){
  library(ggplot2)
  n = nrow(time_difference)
  ggplot(time_difference, aes(x = seq(1:n), y = time_difference)) + geom_point() +xlab("")
}
