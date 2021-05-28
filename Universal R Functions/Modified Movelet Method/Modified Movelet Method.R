# Requires preprocessing of data: create a new variable "axes" consisting of all the time series data 

# Find the minimum discrepancy between the new movelet and the training data
minDiscrep <- function(newMovelet, activity.training, moveletPoints, distOption, useMag){
  if (nrow(activity.training) >= moveletPoints){
    activity.numMovelets <- nrow(activity.training) - moveletPoints + 1
    activity.discrep <- rep(NA, activity.numMovelets)
    discrep.temp <- matrix(NA, activity.numMovelets, ncol(newMovelet)) # Add new matrix variable discrep.temp to record the discrepancy on each axis
    if (distOption == "L2"){
      for (j in 1:activity.numMovelets){
        Movelet <- activity.training[j:(j+moveletPoints-1),]
        if (useMag == 0){
          for(k in 1:ncol(newMovelet)){ # loop: getting the discrepancy on each time series
            discrep.temp[j,k] <- sqrt(sum((newMovelet[,k] - Movelet[,k])^2))
          }
          activity.discrep[j] <- mean(discrep.temp[j,])
        } else {
          activity.discrep[j] <- sqrt(sum((newMovelet$mag - Movelet$mag)^2))
        }
      }
      metric <- min(na.omit(activity.discrep)) ##minimum L2 distance
    } else if (distOption == "correlation"){
      for (j in 1:activity.numMovelets){
        Movelet <- activity.training[j:(j+moveletPoints-1),]
        if (useMag == 0){
          for(k in 1:ncol(newMovelet)){
            discrep.temp[j,k] <- cor(newMovelet[,k], Movelet[,k])
          }
          activity.discrep[j] <- mean(discrep.temp[j,])
        } else {
          Movelet <- activity.training[j:(j+moveletPoints-1),]
          activity.discrep[j] <- cor(newMovelet$mag, Movelet$mag)
        }
      }
      metric <- max(na.omit(activity.discrep)) ##maximum correlation
    } else {
      stop("Error: unsupported distance metric")
    }  
  } else {##The training activity is shorter than the movelet length, compare training window (length n points)
    ##with first n points of new movelet
    n <- nrow(activity.training)
    Movelet <- activity.training
    discrep.temp <- rep(NA, ncol(newMovelet))
    if (distOption == "L2"){
      if (useMag == 0){
        for(k in 1:ncol(newMovelet)){
          discrep.temp[k] <- sqrt(sum((newMovelet[1:n,k] - Movelet[,k])^2))
        }
        activity.discrep <- mean(discrep.temp)
      } else {
        activity.discrep <- sqrt(sum((newMovelet$mag[1:n] - Movelet$mag)^2))
      }
      metric <- activity.discrep ##minimum L2 distance
    } else if (distOption == "correlation"){
      if (useMag == 0){
        for(k in 1:ncol(newMovelet)){
          discrep.temp[k] <- cor(newMovelet[1:n,k] - Movelet[,k])
        }
        activity.discrep <- mean(discrep.temp)
      } else {
        activity.discrep <- cor(newMovelet$mag[1:n], Movelet$mag)
      }
      metric <- activity.discrep ##maximum correlation
    } else {
      stop("Error: unsupported distance metric")
    }    
  }
  
  return(metric)
}

movelet_Bai2012_singleSensor_modified <- function(data,
                                         axes,
                                         training, 
                                         frequency, 
                                         moveletLength, 
                                         distOption, 
                                         trainingActivities, 
                                         useMag){
  
  moveletPoints <- frequency * moveletLength ##number of points in a movelet
  
  data.length <- nrow(data) ##number of points in the whole data set
  
  timeDiff <- data$timeElapsed[2:data.length] - data$timeElapsed[1:(data.length - 1)]
  ##the time difference between adjacent points in seconds
  
  print("Separation between data points in seconds varies from")
  print(sort(unique(round(timeDiff,3))))
  
  print(paste("Separation should be", 1/frequency, "seconds"))
  
  ##############################################################################
  ##Discrepancy between new movelet with dictionary movelets##
  ##############################################################################
  ##the activity predicted for the movelet
  data$movelet.label <- NA 
  
  numActivities <- length(trainingActivities)
  
  ##mat will be the matrix storing the minimum discrepancy of new movelet to training movelets
  ##the first column corresponds to first activity, etc.
  mat.minDiscrep <- matrix(NA, nrow = data.length, ncol = numActivities)
  
  for (i in 1:(data.length-moveletPoints+1)){
    
    ##The movelet beginning at the i-th timestamp in data
    
    newMovelet <- axes[i:(i+moveletPoints-1),] # new movelet consists of data from "axes"
    
    ##Compute the minimum discrepancy between the new unlabeled movelet
    ##and the movelets for each activity in the dictionary
    
    for (c in 1:numActivities){
      mat.minDiscrep[i,c] <- minDiscrep(newMovelet, 
                                        training[training$label == trainingActivities[c],], 
                                        moveletPoints, 
                                        distOption,
                                        useMag)  
    }
    
    
    ##The discrep vector gives the minimum discrepancy for each activity
    discrep <- mat.minDiscrep[i,]
    
    ##The activity (or activities) that attain the minimum discrepancy
    
    if (distOption == "L2") {
      winner <- which(discrep == min(discrep)) ##minimize L2 distance
    } else {
      winner <- which(discrep == max(discrep)) ##maximize correlation
    }
    
    
    ##Notification if there is a tie
    
    if(length(winner) > 1){
      print(paste("Ties for movelet", i))
    }
    
    ##Predicted activity for movelet beginning at i-th timestamp
    ##If there was a tie, choose the activity that was earliest in the
    ##activities vector.
    
    data$movelet.label[i] <- trainingActivities[min(winner)]
  }
  
  ##############################################################################
  ##Predict activity for each timestamp##
  ##############################################################################
  data$label.predict <- NA
  
  ##Count the number of votes for each activity  
  tally <- matrix(NA, nrow = data.length, ncol = numActivities)
  
  for (i in 1:(data.length-moveletPoints+1-moveletPoints)){
    
    ##Predictions for the movelet at timestamp i up to the movelet at 
    ##timestamp i + moveletPoints 
    moveletPredictions <- data$movelet.label[i:(i + moveletPoints)] 
    
    for (j in 1:numActivities){
      tally[i,j] <- sum(moveletPredictions == trainingActivities[j])
    }
    
    ##Choose the activity with the most votes
    ##If there is a tie, choose the activity that occurs earlier in trainingActivities
    ##vector.
    
    winner <- which(tally[i,] == max(tally[i,]))
    
    data$label.predict[i] <- trainingActivities[min(winner)]
  }
  
  tally <- tally/(moveletPoints + 1) 
  
  return(data)
}

plotActivityPrediction <- function(result, xRange, activityList, activityCols, legend){
  numActivities <- length(activityList)
  
  if (is.na(xRange[1])){
    xRange <- range(result$timestamp)
  }
  
  if(legend){
    par(mfrow = c(3,1))
  }
  else{
    par(mfrow = c(2,1))
  }
  
  for (i in 1:numActivities){
    plot(result$timestamp[!is.na(result$label) & result$label == activityList[i]], 
         rep(1, sum(result$label == activityList[i], na.rm = TRUE)), 
         col = activityCols[i], xlim = xRange, type = "h", yaxt = 'n',
         xlab = "Time elapsed", ylab = "", ylim = c(0,1),
         main = "Truth",
         cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1)  
    if (i != numActivities){
      par(new = TRUE)
    }
  }
  
  for (i in 1:numActivities){
    plot(result$timestamp[!is.na(result$label.predict) & result$label.predict == activityList[i]], 
         rep(1, sum(result$label.predict == activityList[i], na.rm = TRUE)), 
         col = activityCols[i], xlim = xRange, type = "h", yaxt = 'n',
         xlab = "Time elapsed", ylab = "", ylim = c(0,1),
         main = "Prediction",
         cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1)
    if (i != numActivities){
      par(new = TRUE)
    }
  }
  
  if(legend){
    plot.new()
    legend("center", legend=activityList, fill=activityCols, cex=0.8, ncol = 4, text.font=2, box.lty=0)
  }
}

