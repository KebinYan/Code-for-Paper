## Parameters:
## data: the dataset to be standardized
## timeUnit: the unit of timestamp, either in millisecond ("ms") or second ("second")
standardizeData <- function(data, timeUnit){
  
  timeUnit <- timeUnit
  
  names(data)[names(data) == "timeElapsed"] <- "timestamp"
  
  # create a standardized dataset with standardized timestamp (0.1s between each pair of data), x,y,z axes and label
  if(timeUnit == "ms"){
    maxTimePoint <- data$timestamp[nrow(data)]+100
    timePassed <- data$timestamp[nrow(data)]-data$timestamp[1]+100
    standardizedData <- data.frame(rep(NA, timePassed/100))
    standardizedData$timestamp <- seq(data$timestamp[1], (maxTimePoint-100), by = 100)
  }
  if(timeUnit == "second"){
    maxTimePoint <- floor((data$timestamp[nrow(data)]+0.1)*10)/10
    timePassed <- floor((data$timestamp[nrow(data)]-data$timestamp[1]+0.1)*10)/10
    standardizedData <- data.frame(rep(NA, timePassed/0.1))
    standardizedData$timestamp <- seq(data$timestamp[1], (maxTimePoint-0.1), by = 0.1)
  }
  standardizedData$x <- NA
  standardizedData$y <- NA
  standardizedData$z <- NA
  standardizedData$label <- NA
  standardizedData <- standardizedData[,-1]
  
  # interpolation
  for(j in 1:nrow(standardizedData)){
    for(i in 1:(nrow(data)-1)){
      if(data$timestamp[i]<=standardizedData$timestamp[j] & data$timestamp[i+1]>=standardizedData$timestamp[j]){
        standardizedData$x[j] <- approx(data$timestamp[c(i,i+1)], data$x[c(i,i+1)], xout = standardizedData$timestamp[j], method = "linear")$y
        standardizedData$y[j] <- approx(data$timestamp[c(i,i+1)], data$y[c(i,i+1)], xout = standardizedData$timestamp[j], method = "linear")$y
        standardizedData$z[j] <- approx(data$timestamp[c(i,i+1)], data$z[c(i,i+1)], xout = standardizedData$timestamp[j], method = "linear")$y
        
        ## assign the label
        # condition 1: if the two endpoints have the same label, take such a label for the interpolated point
        if(!is.na(data$label[i] == data$label[i+1]) & data$label[i] == data$label[i+1]){
          levelVal <- data$label[i]
          standardizedData$label[j] <- levels(data$label)[levelVal]
        }
        # condition 2: if the two endpoints have different labels, take the label that is closer to the interpolated points in terms of time
        # if the interpolated point is in the middle point of the two endpoints, take the label of the left endpoint
        else if(!is.na(data$label[i] == data$label[i+1]) & data$label[i] != data$label[i+1]){
          if(abs(data$timestamp[i]-standardizedData$timestamp[j]) >= (data$timestamp[i+1]-standardizedData$timestamp[j])){
            levelVal <- data$label[i]
            standardizedData$label[j] <- levels(data$label)[levelVal]
          }
          else{
            levelVal <- data$label[i+1]
            standardizedData$label[j] <- levels(data$label)[levelVal]
          }
        }
      }
    }
  }
  
  return(standardizedData)
  
}