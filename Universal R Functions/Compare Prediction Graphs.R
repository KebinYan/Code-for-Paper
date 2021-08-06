# GyroOnAccTime function: find the closest time point in gyroscope data and take the predicted label as the gyroscope 
# prediction for each timestamp in the accelerometer data
# Function parameters: accelerometer result data set, gyroscope result data set, time difference data set got from the time difference function
GyroOnAccTime <- function(accResult, gyroResult, timeDifference){
  gyro_on_acc_time <- as.data.frame(cbind(accResult$timestamp[1], gyroResult[1,]))
  colnames(gyro_on_acc_time)[1] <- "acc.timestamp"
  for(i in 1:nrow(accResult)){
    for(j in 1:nrow(gyroResult)){
      if(timeDifference$closest_gyro_timestamp[i] == gyroResult$timestamp[j]){
        gyro_data <- gyroResult[j,]
        gyro_data$label <- accResult$label[i]
        break
      }
    }
    new_row <- cbind(accResult$timestamp[i], gyro_data)
    colnames(new_row)[1] <- "acc.timestamp"
    gyro_on_acc_time <- rbind(gyro_on_acc_time, new_row)
  }
  gyro_on_acc_time <- gyro_on_acc_time[-1,]
  return(gyro_on_acc_time) # return a subset of gyroscope result on accelerometer's timestamp 
}

# remove rows with no final prediction
RemoveNAs <- function(result){
  revised_result <- subset(result, !is.na(result$label.predict))
  return(revised_result)
}

# remove the extra end point in accelerometer result or in the gyroscope on accelerometer result
RemoveExtra <- function(revised_acc, revised_gyro_on_acc, revised_combined){
  nrow_acc <- nrow(revised_acc)
  nrow_gyro <- nrow(revised_gyro_on_acc)
  nrow_combined <- nrow(revised_combined)
  minRow <- min(nrow_acc, nrow_gyro, nrow_combined)
  
  if(nrow_acc > minRow){
    revised_acc <- revised_acc[-nrow_acc,]
  }
  if(nrow_gyro > minRow){
    revised_gyro_on_acc <- revised_gyro_on_acc[-nrow_gyro,]
  }
  if(nrow_combined > minRow){
    revised_combined <- revised_combined[-nrow_combined,]
  }
  
  out <- list(revised_acc = revised_acc, revised_gyro_on_acc = revised_gyro_on_acc, revised_combined = revised_combined)
  return(out)
}

# revise accelerometer, gyroscope, and combined prediction data sets so all three of them have same amount of data on same timestamps
getRevisedData <- function(accResult, gyroResult, combinedResult, timeDifference){
  gyro_on_acc <- GyroOnAccTime(accResult, gyroResult, timeDifference)
  revised_acc <- RemoveNAs(accResult)
  revised_gyro_on_acc <- RemoveNAs(gyro_on_acc)
  revised_combined <- RemoveNAs(combinedResult)
  revised_data <- RemoveExtra(revised_acc, revised_gyro_on_acc, revised_combined)
  return(revised_data)
}

# create a graph comparing true labels, accelerometer predicitons, gyroscope predcitions, and combined data predictions
# the logic of the function is the same as "PlotActivityPrediction" function in the movelet method
combinedPlotActivityPrediction <- function(accResult, gyroResult, combinedResult, xRange, activityList, activityCols, legend){
  numActivities <- length(activityList)
  
  accResult$timeElapsed <- (accResult$timestamp - accResult$timestamp[1])/1000
  gyroResult$timeElapsed <- (gyroResult$acc.timestamp - gyroResult$acc.timestamp[1])/1000
  combinedResult$timeElapsed <- (combinedResult$timestamp - combinedResult$timestamp[1])/1000
  
  accResult <- subset(accResult, !is.na(accResult$label))
  gyroResult <- subset(gyroResult, !is.na(gyroResult$label))
  combinedResult <- subset(combinedResult, !is.na(combinedResult$label))
  
  if (is.na(xRange[1])){
    xRange <- range(combinedResult$timeElapsed[!is.na(combinedResult$label)])
  }
  
  if(legend){
    par(mfrow = c(5,1)) 
  }
  else{
    par(mfrow = c(4,1))
  }
  par(mar=c(3,1,3,1))
  
  for (i in 1:numActivities){
    plot(combinedResult$timeElapsed[!is.na(combinedResult$label) & combinedResult$label == activityList[i]], 
         rep(1, sum(combinedResult$label == activityList[i], na.rm = TRUE)), 
         col = activityCols[i], xlim = xRange, type = "h", yaxt = 'n',
         xlab = "", ylab = "", ylim = c(0,1),
         main = "Truth",
         cex.lab=1, cex.axis=1.5, cex.main=2, cex.sub=1)  
    if (i != numActivities){
      par(new = TRUE)
    }
  }
  
  
  for (i in 1:numActivities){
    plot(accResult$timeElapsed[!is.na(accResult$label.predict) & accResult$label.predict == activityList[i]], 
         rep(1, sum(accResult$label.predict == activityList[i], na.rm = TRUE)), 
         col = activityCols[i], xlim = xRange, type = "h", yaxt = 'n',
         xlab = "", ylab = "", ylim = c(0,1),
         main = "Accelerometer-only Prediction",
         cex.lab=1, cex.axis=1.5, cex.main=2, cex.sub=1)
    if (i != numActivities){
      par(new = TRUE)
    }
  }
  
  for (i in 1:numActivities){
    plot(gyroResult$timeElapsed[!is.na(gyroResult$label.predict) & gyroResult$label.predict == activityList[i]], 
         rep(1, sum(gyroResult$label.predict == activityList[i], na.rm = TRUE)), 
         col = activityCols[i], xlim = xRange, type = "h", yaxt = 'n',
         xlab = "", ylab = "", ylim = c(0,1),
         main = " Gyroscope-only Prediction",
         cex.lab=1, cex.axis=1.5, cex.main=2, cex.sub=1)
    if (i != numActivities){
      par(new = TRUE)
    }
  }
  
  for (i in 1:numActivities){
    plot(combinedResult$timeElapsed[!is.na(combinedResult$label.predict) & combinedResult$label.predict == activityList[i]], 
         rep(1, sum(combinedResult$label.predict == activityList[i], na.rm = TRUE)), 
         col = activityCols[i], xlim = xRange, type = "h", yaxt = 'n',
         xlab = "", ylab = "", ylim = c(0,1),
         main = "Joint-sensor Prediction",
         cex.lab=1, cex.axis=1.5, cex.main=2, cex.sub=1)
    if (i != numActivities){
      par(new = TRUE)
    }
  }
  mtext("Time Elapsed (sec)", side = 1, line = 4, cex = 1.3)
  
  if(legend)
  {
    par(mar=c(1,1,2.5,1))
    plot.new()
    legend("center", legend=activityList, fill=activityCols, cex=1.7, ncol = 4, text.font=1.5, box.lty=0)
  }
}

