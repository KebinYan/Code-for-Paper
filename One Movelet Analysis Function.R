#one_movelet_analysis

#This function is used for one movelet analysis

one_movelet_analysis_graph <- function(data, axes, movelet_index, training, distOption, useMag){
  
  newMovelet <- axes[movelet_index:(movelet_index+9),]
  
  acc.max <- max(axes$acc.x, axes$acc.y, axes$acc.z)
  acc.min <- min(axes$acc.x, axes$acc.y, axes$acc.z)
  gyro.max <- max(axes$gyro.x, axes$gyro.y, axes$gyro.z)
  gyro.min <- min(axes$gyro.x, axes$gyro.y, axes$gyro.z)
  
  numActivities <- length(trainingActivities)
  
  # create a data frame to record the mean discrepancy, movelet index, and discrepancy on each axis for each activity
  minDiscrep <- data.frame("metric" = NA, "movelet" = NA)
  for(j in 1:length(names(axes))){
    minDiscrep <- data.frame(minDiscrep, names(axes)[j])
    names(minDiscrep)[j+2] <- as.character(minDiscrep[1,j+2])
    minDiscrep[j+2] <- as.numeric(minDiscrep[j+2])
  }
  minDiscrep <- minDiscrep[-1,]
  for (c in 1:numActivities){
    minDiscrep.temp <- DiscrepMatrix(newMovelet, training[training$label == trainingActivities[c],], moveletPoints, distOption,useMag)
    minDiscrep.temp.mean <- rep(NA, nrow(minDiscrep.temp))
    for(i in 1:nrow(minDiscrep.temp)){
      minDiscrep.temp.mean[i] <- mean(minDiscrep.temp[i,])
    }
    if(distOption == "L2"){
      minDiscrep[c,1] <- min(minDiscrep.temp.mean)
      minDiscrep[c,2] <- which(minDiscrep.temp.mean == minDiscrep[c,1])
      for (k in 1:ncol(axes)){
        minDiscrep[c,(k+2)] <- minDiscrep.temp[minDiscrep[c,2], k]
      }
    }
    else if(distOption == "correlation"){
      minDiscrep[c,1] <- max(minDiscrep.temp.mean)
      minDiscrep[c,2] <- which(minDiscrep.temp.mean == minDiscrep[c,1])
      for (k in 1:ncol(axes)){
        minDiscrep[c,(k+2)] <- minDiscrep.temp[minDiscrep[c,2], k]
      }
    }
  }
  
  # plots
  colors <- rainbow(ncol(axes))
  par(mar = c(2,4,2,2), mfrow = c(2,3))
  myPlot(data, axes, movelet_index, moveletPoints, acc.max, acc.min, gyro.max, gyro.min, if.new = FALSE, axis = 1, label_axis = FALSE, title = "New Movelet", discrep.value = "")
  for(i in 2:ncol(axes)){
    myPlot(data, axes, movelet_index, moveletPoints, acc.max, acc.min, gyro.max, gyro.min, if.new = TRUE, axis = i, label_axis = FALSE, title = "", discrep.value = "")
  }
  legend("topright", inset = c(-0.6,0), legend = colnames(axes), col = colors, pch = 20, lwd = 2, cex = 0.7, bty = "n", seg.len = 0.2, x.intersp = 0.1, y.intersp = 0.5)
  for(m in 1:length(trainingActivities)){
    myPlot(data = subset(training,training$label == trainingActivities[m]), axes = subset(training, training$label == trainingActivities[m]), minDiscrep[m,2], moveletPoints, acc.max, acc.min, gyro.max, gyro.min, if.new = FALSE, axis = 1, label_axis = FALSE, title = trainingActivities[m], discrep.value = paste("Discrepancy: ", round(minDiscrep[m,1], 2)))
    for(i in 2:ncol(axes)){
      myPlot(data = subset(training,training$label == trainingActivities[m]), axes = subset(training, training$label == trainingActivities[m]), minDiscrep[m,2], moveletPoints, acc.max, acc.min, gyro.max, gyro.min, if.new = TRUE, axis = i, label_axis = FALSE, title = "", discrep.value = "")
    }
    legend("topright", inset = c(-0.6,0), legend = colnames(axes), col = colors, pch = 20, lwd = 2, cex = 0.7, bty = "n", seg.len = 0.2, x.intersp = 0.1, y.intersp = 0.5)
  }
  
  for(n in 1:ncol(axes)){
    par(mfrow = c(2,3))
    myPlot(data, axes, movelet_index, moveletPoints, acc.max, acc.min, gyro.max, gyro.min, if.new = FALSE, axis = i, label_axis = TRUE, title = "New Movelet", discrep.value = "")
    legend("topright", inset = c(-0.6,0), legend = colnames(axes[n]), col = colors[n], pch = 20, lwd = 2, cex = 0.7, bty = "n", seg.len = 0.2, x.intersp = 0.1, y.intersp = 0.5)
    for(m in 1:length(trainingActivities)){
      myPlot(data = subset(training,training$label == trainingActivities[m]), axes = subset(training, training$label == trainingActivities[m]), minDiscrep[m,2], moveletPoints, acc.max, acc.min, gyro.max, gyro.min, if.new = FALSE, axis = i, label_axis = TRUE, title = trainingActivities[m], discrep.value = paste("Discrepancy: ", round(minDiscrep[m,n+2], 2)))
      legend("topright", inset = c(-0.6,0), legend = colnames(axes[n]), col = colors[n], pch = 20, lwd = 2, cex = 0.7, bty = "n", seg.len = 0.2, x.intersp = 0.1, y.intersp = 0.5)
    }
  }
}

#==================================Two helper functions of the main function=================================================================================================================================

# This is almost the same function as minDiscrep, the only difference is the returning statement: return the discrepancies with respect to all training movelets
# This function is used to calculate for a discrepancy matrix
DiscrepMatrix <- function(newMovelet, activity.training, moveletPoints, distOption, useMag){
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
        } else {
          activity.discrep[j] <- sqrt(sum((newMovelet$mag - Movelet$mag)^2))
        }
      }
      
    } else if (distOption == "correlation"){
      for (j in 1:activity.numMovelets){
        Movelet <- activity.training[j:(j+moveletPoints-1),]
        if (useMag == 0){
          for(k in 1:ncol(newMovelet)){
            discrep.temp[j,k] <- cor(newMovelet[,k], Movelet[,k])
          }
        } else {
          activity.discrep[j] <- cor(newMovelet$mag, Movelet$mag)
        }
      }
      
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
      } else {
        activity.discrep <- sqrt(sum((newMovelet$mag[1:n] - Movelet$mag)^2))
      }
      
    } else if (distOption == "correlation"){
      if (useMag == 0){
        for(k in 1:ncol(newMovelet)){
          discrep.temp[k] <- cor(newMovelet[1:n,k] - Movelet[,k])
        }
      } else {
        activity.discrep <- cor(newMovelet$mag[1:n], Movelet$mag)
      }
      
    } else {
      stop("Error: unsupported distance metric")
    }    
  }
  
  return(discrep.temp)
}

# Create an one-axis plot
myPlot <- function(data, axes, movelet_index, moveletPoints, acc.max, acc.min, gyro.max, gyro.min, if.new, axis, label_axis, title, discrep.value){
  x = data[movelet_index:(movelet_index + moveletPoints - 1),]$acc.timeElapsed
  y = axes[movelet_index:(movelet_index + moveletPoints - 1), axis]
  if(!if.new){
    plot(x = x, y = axes[movelet_index:(movelet_index + moveletPoints - 1),1], xlab = "Time Elasped", ylab = "Acceleration", ylim = c(acc.min, acc.max), type = "n", las = 1)
    title(main = title)
    par(new = TRUE)
    plot(x = x, y = axes[movelet_index:(movelet_index + moveletPoints - 1),4], xlab = "", ylab = "", axes = FALSE, type = "n", ylim = c(gyro.min, gyro.max))
    axis(side = 4, ylim = c(gyro.min, gyro.max), las = 1)
    mtext("Rotation",side = 4, cex = 0.7)
  }
  par(new = TRUE)
  lines(x = x, y = y, type = "o", pch = 20, col = colors[axis])
  text(x = x[3], y = 3, discrep.value, pos = 3)
  if(label_axis){
    text(x, y, round(y,2), cex=0.5, pos=3)
  }
}