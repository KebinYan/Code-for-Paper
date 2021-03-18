# GyroOnAccTime function: find the closest time point in gyroscope data and take the predicted label as the gyroscope 
# prediction for each timestamp in the accelerometer data
# Function parameters: accelerometer result data set, gyroscope result data set, time difference data set got from the time difference function
GyroOnAccTime <- function(accResult, gyroResult, timeDifference){
  # create an index column for indices in the gyroscope data set that are closest to each timestamp in accelerometer data
  index <- as.data.frame(rep(NA, nrow(accResult)))
  j=1
  for(i in 1:nrow(accResult)){
    time_diff <- 999
    while(j <= nrow(timeDifference) && accResult$timestamp[i]==timeDifference$acc_timestamp[j]){
      if(time_diff > timeDifference$time_difference[j]){
        time_diff <- timeDifference$time_difference[j]
        index[i,1] <- j
      }
      j = j+1 #since the timestamps follow an increasing pattern down the rows, there's no need to check the timestamps from beginning
    }
  }
  
  gyro_on_acc_time <- subset(gyroResult[unlist(index),])
  gyro_on_acc_time <- cbind(accResult$timestamp, gyro_on_acc_time)
  colnames(gyro_on_acc_time)[1] <- "acc.timestamp"
  return(gyro_on_acc_time) # return a subset of gyroscope result on accelerometer's timestamp 
}

# create a graph comparing true labels, accelerometer predicitons, gyroscope predcitions, and combined data predictions
# the logic of the function is the same as "PlotActivityPrediction" function in the movelet method
combinedPlotActivityPrediction <- function(accResult, gyroResult, combinedResult, xRange, activityList, activityCols, legend){
  numActivities <- length(activityList)
  
  if (is.na(xRange[1])){
    xRange <- range(combinedResult$timestamp)
  }
  
  if(legend){
    par(mfrow = c(5,1)) 
  }
  else{
    par(mfrow = c(4,1))
  }
  par(mar=c(2,1,1,1))
  
  for (i in 1:numActivities){
    plot(combinedResult$timestamp[!is.na(combinedResult$label) & combinedResult$label == activityList[i]], 
         rep(1, sum(combinedResult$label == activityList[i], na.rm = TRUE)), 
         col = activityCols[i], xlim = xRange, type = "h", yaxt = 'n',
         xlab = "Time elapsed", ylab = "", ylim = c(0,1),
         main = "Truth",
         cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1)  
    if (i != numActivities){
      par(new = TRUE)
    }
  }
  
  for (i in 1:numActivities){
    plot(accResult$timestamp[!is.na(accResult$label.predict) & accResult$label.predict == activityList[i]], 
         rep(1, sum(accResult$label.predict == activityList[i], na.rm = TRUE)), 
         col = activityCols[i], xlim = xRange, type = "h", yaxt = 'n',
         xlab = "Time elapsed", ylab = "", ylim = c(0,1),
         main = "Accelerometer Prediction",
         cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1)
    if (i != numActivities){
      par(new = TRUE)
    }
  }
  
  for (i in 1:numActivities){
    plot(gyroResult$timestamp[!is.na(gyroResult$label.predict) & gyroResult$label.predict == activityList[i]], 
         rep(1, sum(gyroResult$label.predict == activityList[i], na.rm = TRUE)), 
         col = activityCols[i], xlim = xRange, type = "h", yaxt = 'n',
         xlab = "Time elapsed", ylab = "", ylim = c(0,1),
         main = " Gyroscope Prediction",
         cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1)
    if (i != numActivities){
      par(new = TRUE)
    }
  }
  
  for (i in 1:numActivities){
    plot(combinedResult$timestamp[!is.na(combinedResult$label.predict) & combinedResult$label.predict == activityList[i]], 
         rep(1, sum(combinedResult$label.predict == activityList[i], na.rm = TRUE)), 
         col = activityCols[i], xlim = xRange, type = "h", yaxt = 'n',
         xlab = "Time elapsed", ylab = "", ylim = c(0,1),
         main = "Combined Prediction",
         cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1)
    if (i != numActivities){
      par(new = TRUE)
    }
  }
  
  if(legend)
  {
    plot.new()
    legend("center", legend=activityList, fill=activityCols, cex=0.8, ncol = 4, text.font=2, box.lty=0)
  }
}

