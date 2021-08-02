# calculate time difference between accelerometer timestamp and the closest gyroscope timestamp
CalculateTimeDifference <- function(accData, gyroData){
  time_difference <- as.data.frame(cbind(accData$timestamp, rep(NA,nrow(accData)), rep(NA,nrow(accData))))
  colnames(time_difference) <- c("acc_timestamp","closest_gyro_timestamp","time_difference")
  
  for (i in 1:nrow(accData)){
    temp_diff <- NA
    temp_diff <- as.data.frame(rep(NA, nrow(gyroData)))
    for(j in 1:nrow(gyroData)){
      temp_diff[j,1] <- abs(accData$timestamp[i]-gyroData$timestamp[j])
    }
    time_difference[i,3] <- min(temp_diff)
    time_difference[i,2] <- gyroData$timestamp[which.min(unlist(temp_diff))]
  }
  
  return(time_difference)
}

# plot the time difference for all data points
GraphTimeDifference <- function(time_difference){
  library(ggplot2)
  n = nrow(time_difference)
  ggplot(time_difference, aes(x = seq(1:n), y = time_difference)) + geom_point() +xlab("")
}
