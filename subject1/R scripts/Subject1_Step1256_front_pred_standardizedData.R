library(data.table)
source("~/Desktop/WFU/URECA/R_template/six axes/Modified Movelet Method.R")
source("~/Desktop/WFU/URECA/R_template/Combine Acc and Gyro Data Using Linear Interpolation.R")

# training set size in seconds
trainingSec <- 5

## acc only

# training 
# load data
acc_front_walk <- read.csv("~/Desktop/data/subject1/training/standardized Data/walk/acc_front.csv")
acc_front_stand <- read.csv("~/Desktop/data/subject1/training/standardized Data/stand/acc_front.csv")
acc_front_stairUp <- read.csv("~/Desktop/data/subject1/training/standardized Data/stairUp/acc_front.csv")
acc_front_stairDown <- read.csv("~/Desktop/data/subject1/training/standardized Data/stairDown/acc_front.csv")
acc_front_chairStand1 <- read.csv("~/Desktop/data/subject1/training/standardized Data/chairStand1/acc_front.csv")

# get the middle 5 seconds for training set
acc_standTime <- setDT(acc_front_stand)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
acc_trainStand <- subset(acc_front_stand, acc_front_stand$timestamp > (acc_standTime - trainingSec/2) & acc_front_stand$timestamp < (acc_standTime + trainingSec/2))
acc_walkTime <- setDT(acc_front_walk)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
acc_trainWalk <- subset(acc_front_walk, acc_front_walk$timestamp > (acc_walkTime - trainingSec/2) & acc_front_walk$timestamp < (acc_walkTime + trainingSec/2))
acc_stairUpTime <- setDT(acc_front_stairUp)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
acc_trainStairUp<- subset(acc_front_stairUp, acc_front_stairUp$timestamp > (acc_stairUpTime - trainingSec/2) & acc_front_stairUp$timestamp < (acc_stairUpTime + trainingSec/2))
acc_stairDownTime <- setDT(acc_front_stairDown)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
acc_trainStairDown <- subset(acc_front_stairDown, acc_front_stairDown$timestamp > (acc_stairDownTime - trainingSec/2) & acc_front_stairDown$timestamp < (acc_stairDownTime + trainingSec/2))

# create a training set with all activities
acc_trainingSet <- rbind.data.frame(acc_trainWalk, acc_trainStand, acc_trainStairUp, acc_trainStairDown, acc_front_chairStand1)

# change the label in training set to the same notations as in the test set
for(i in 1:nrow(acc_trainingSet)){
  acc_trainingSet[i]$label <- ifelse(acc_trainingSet[i]$label=="sit1","sit", acc_trainingSet[i]$label)
  acc_trainingSet[i]$label <- ifelse(acc_trainingSet[i]$label=="sitToStand1","sitToStand", acc_trainingSet[i]$label)
  acc_trainingSet[i]$label <- ifelse(acc_trainingSet[i]$label=="standToSit1","standToSit", acc_trainingSet[i]$label)
}

# create a training dataset with the 3 axes and the label column
acc_training <- cbind.data.frame(acc_trainingSet$x, acc_trainingSet$y, acc_trainingSet$z, acc_trainingSet$label)
names(acc_training) <- c("acc.x", "acc.y", "acc.z", "label")

# test
# load data
test_acc_front_step1 <- read.csv("~/Desktop/data/subject1/test/Step1/standardized Data/acc_front.csv")
test_acc_front_step2 <- read.csv("~/Desktop/data/subject1/test/Step2/standardized Data/acc_front.csv")
test_acc_front_step5 <- read.csv("~/Desktop/data/subject1/test/Step5/standardized Data/acc_front.csv")
test_acc_front_step6_stairDown <- read.csv("~/Desktop/data/subject1/test/Step6/stairDown/standardized Data/acc_front.csv")
test_acc_front_step6_stairUp <- read.csv("~/Desktop/data/subject1/test/Step6/stairUp/standardized Data/acc_front.csv")

# create an axes dataset including only the axes
acc_axes_step1 <- cbind.data.frame(test_acc_front_step1$x, test_acc_front_step1$y, test_acc_front_step1$z)
names(acc_axes_step1) <- c("acc.x", "acc.y", "acc.z")
acc_axes_step2 <- cbind.data.frame(test_acc_front_step2$x, test_acc_front_step2$y, test_acc_front_step2$z)
names(acc_axes_step2) <- c("acc.x", "acc.y", "acc.z")
acc_axes_step5 <- cbind.data.frame(test_acc_front_step5$x, test_acc_front_step5$y, test_acc_front_step5$z)
names(acc_axes_step5) <- c("acc.x", "acc.y", "acc.z")
acc_axes_step6_stairDown <- cbind.data.frame(test_acc_front_step6_stairDown$x, test_acc_front_step6_stairDown$y, test_acc_front_step6_stairDown$z)
names(acc_axes_step6_stairDown) <- c("acc.x", "acc.y", "acc.z")
acc_axes_step6_stairUp <- cbind.data.frame(test_acc_front_step6_stairUp$x, test_acc_front_step6_stairUp$y, test_acc_front_step6_stairUp$z)
names(acc_axes_step6_stairUp) <- c("acc.x", "acc.y", "acc.z")

## gyro only

# training
# load data
gyro_front_walk <- read.csv("~/Desktop/data/subject1/training/standardized Data/walk/gyro_front.csv")
gyro_front_stand <- read.csv("~/Desktop/data/subject1/training/standardized Data/stand/gyro_front.csv")
gyro_front_stairUp <- read.csv("~/Desktop/data/subject1/training/standardized Data/stairUp/gyro_front.csv")
gyro_front_stairDown <- read.csv("~/Desktop/data/subject1/training/standardized Data/stairDown/gyro_front.csv")
gyro_front_chairStand1 <- read.csv("~/Desktop/data/subject1/training/standardized Data/chairStand1/gyro_front.csv")

# get the middle 5 seconds for training set
gyro_standTime <- setDT(gyro_front_stand)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
gyro_trainStand <- subset(gyro_front_stand, gyro_front_stand$timestamp > (gyro_standTime - trainingSec/2) & gyro_front_stand$timestamp < (gyro_standTime + trainingSec/2))
gyro_walkTime <- setDT(gyro_front_walk)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
gyro_trainWalk <- subset(gyro_front_walk, gyro_front_walk$timestamp > (gyro_walkTime - trainingSec/2) & gyro_front_walk$timestamp < (gyro_walkTime + trainingSec/2))
gyro_stairUpTime <- setDT(gyro_front_stairUp)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
gyro_trainStairUp<- subset(gyro_front_stairUp, gyro_front_stairUp$timestamp > (gyro_stairUpTime - trainingSec/2) & gyro_front_stairUp$timestamp < (gyro_stairUpTime + trainingSec/2))
gyro_stairDownTime <- setDT(gyro_front_stairDown)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
gyro_trainStairDown <- subset(gyro_front_stairDown, gyro_front_stairDown$timestamp > (gyro_stairDownTime - trainingSec/2) & gyro_front_stairDown$timestamp < (gyro_stairDownTime + trainingSec/2))

# create a training set with all activities
gyro_trainingSet <- rbind.data.frame(gyro_trainWalk, gyro_trainStand, gyro_trainStairUp, gyro_trainStairDown, gyro_front_chairStand1[-nrow(gyro_front_chairStand1),])

# change the label in training set to the same notations as in the test set
for(i in 1:nrow(gyro_trainingSet)){
  gyro_trainingSet[i]$label <- ifelse(gyro_trainingSet[i]$label=="sit1","sit", gyro_trainingSet[i]$label)
  gyro_trainingSet[i]$label <- ifelse(gyro_trainingSet[i]$label=="sitToStand1","sitToStand", gyro_trainingSet[i]$label)
  gyro_trainingSet[i]$label <- ifelse(gyro_trainingSet[i]$label=="standToSit1","standToSit", gyro_trainingSet[i]$label)
}

# create a training dataset with the 3 axes and the label column
gyro_training <- cbind.data.frame(gyro_trainingSet$x, gyro_trainingSet$y, gyro_trainingSet$z, gyro_trainingSet$label)
names(gyro_training) <- c("gyro.x", "gyro.y", "gyro.z", "label")

# test
# load data
test_gyro_front_step1 <- read.csv("~/Desktop/data/subject1/test/Step1/standardized Data/gyro_front.csv")
test_gyro_front_step2 <- read.csv("~/Desktop/data/subject1/test/Step2/standardized Data/gyro_front.csv")
test_gyro_front_step5 <- read.csv("~/Desktop/data/subject1/test/Step5/standardized Data/gyro_front.csv")
test_gyro_front_step6_stairDown <- read.csv("~/Desktop/data/subject1/test/Step6/stairDown/standardized Data/gyro_front.csv")
test_gyro_front_step6_stairUp <- read.csv("~/Desktop/data/subject1/test/Step6/stairUp/standardized Data/gyro_front.csv")

# create an axes dataset including only the axes
gyro_axes_step1 <- cbind.data.frame(test_gyro_front_step1$x, test_gyro_front_step1$y, test_gyro_front_step1$z)
names(gyro_axes_step1) <- c("gyro.x", "gyro.y", "gyro.z")
gyro_axes_step2 <- cbind.data.frame(test_gyro_front_step2$x, test_gyro_front_step2$y, test_gyro_front_step2$z)
names(gyro_axes_step2) <- c("gyro.x", "gyro.y", "gyro.z")
gyro_axes_step5 <- cbind.data.frame(test_gyro_front_step5$x, test_gyro_front_step5$y, test_gyro_front_step5$z)
names(gyro_axes_step5) <- c("gyro.x", "gyro.y", "gyro.z")
gyro_axes_step6_stairDown <- cbind.data.frame(test_gyro_front_step6_stairDown$x, test_gyro_front_step6_stairDown$y, test_gyro_front_step6_stairDown$z)
names(gyro_axes_step6_stairDown) <- c("gyro.x", "gyro.y", "gyro.z")
gyro_axes_step6_stairUp <- cbind.data.frame(test_gyro_front_step6_stairUp$x, test_gyro_front_step6_stairUp$y, test_gyro_front_step6_stairUp$z)
names(gyro_axes_step6_stairUp) <- c("gyro.x", "gyro.y", "gyro.z")

## combined data

# training data
# note: each gyro data set has 1 more row than acc data set b/c difference in the original time
# the last row in gyro_chairStand1 is removed to make the two datasets have the same length
training <- data.frame(rep(NA, nrow(acc_training)), NA, NA, NA, NA, NA, NA)
names(training) <- c("acc.x", "acc.y", "acc.z", "gyro.x", "gyro.y", "gyro.z", "label")
for(i in 1: nrow(training)){
  if(acc_training$label[i] == gyro_training$label[i]){
    training$acc.x[i] <- acc_training$acc.x[i]
    training$acc.y[i] <- acc_training$acc.y[i]
    training$acc.z[i] <- acc_training$acc.z[i]
    training$gyro.x[i] <- gyro_training$gyro.x[i]
    training$gyro.y[i] <- gyro_training$gyro.y[i]
    training$gyro.z[i] <- gyro_training$gyro.z[i]
    levelVal <- acc_training$label[i]
    training$label[i] <- levels(acc_training$label)[levelVal]
  }
  else{
    training[i,] <- NA
  }
}

training <- na.omit(training)

# test data

## create combined test data set based on acc timestamp
# load unstandardized gyro data
#test_gyro_front_step1 <- read.csv("~/Desktop/data/subject1/test/Step1/gyro_front.csv")
#test_gyro_front_step2 <- read.csv("~/Desktop/data/subject1/test/Step2/gyro_front.csv")
#test_gyro_front_step5 <- read.csv("~/Desktop/data/subject1/test/Step5/gyro_front.csv")
#test_gyro_front_step6_stairDown <- read.csv("~/Desktop/data/subject1/test/Step6/stairDown/gyro_front.csv")
#test_gyro_front_step6_stairUp <- read.csv("~/Desktop/data/subject1/test/Step6/stairUp/gyro_front.csv")
# interpolate gyro data
#combined_front_step1 <- combineData(test_acc_front_step1, test_gyro_front_step1)
#write.csv(combined_front_step1, "~/Desktop/data/subject1/test/Step1/standardized Data/combined_front.csv")
#combined_front_step2 <- combineData(test_acc_front_step2, test_gyro_front_step2)
#write.csv(combined_front_step2, "~/Desktop/data/subject1/test/Step2/standardized Data/combined_front.csv")
#combined_front_step5 <- combineData(test_acc_front_step5, test_gyro_front_step5)
#write.csv(combined_front_step5, "~/Desktop/data/subject1/test/Step5/standardized Data/combined_front.csv")
#combined_front_step6_stairDown <- combineData(test_acc_front_step6_stairDown, test_gyro_front_step6_stairDown)
#write.csv(combined_front_step6_stairDown, "~/Desktop/data/subject1/test/Step6/stairDown/standardized Data/combined_front.csv")
#combined_front_step6_stairUp <- combineData(test_acc_front_step6_stairUp, test_gyro_front_step6_stairUp)
#write.csv(combined_front_step6_stairUp, "~/Desktop/data/subject1/test/Step6/stairUp/standardized Data/combined_front.csv")

#load combined data
test_front_step1 <- read.csv("~/Desktop/data/subject1/test/Step1/standardized Data/combined_front.csv")
test_front_step2 <- read.csv("~/Desktop/data/subject1/test/Step2/standardized Data/combined_front.csv")
test_front_step5 <- read.csv("~/Desktop/data/subject1/test/Step5/standardized Data/combined_front.csv")
test_front_step6_stairDown <- read.csv("~/Desktop/data/subject1/test/Step6/stairDown/standardized Data/combined_front.csv")
test_front_step6_stairUp <- read.csv("~/Desktop/data/subject1/test/Step6/stairUp/standardized Data/combined_front.csv")

axes_step1 <- cbind.data.frame(test_front_step1$acc.x, test_front_step1$acc.y, test_front_step1$acc.z, test_front_step1$gyro.x, test_front_step1$gyro.y, test_front_step1$gyro.z)
names(axes_step1) <- c("acc.x", "acc.y", "acc.z", "gyro.x", "gyro.y", "gyro.z")
axes_step2 <- cbind.data.frame(test_front_step2$acc.x, test_front_step2$acc.y, test_front_step2$acc.z, test_front_step2$gyro.x, test_front_step2$gyro.y, test_front_step2$gyro.z)
names(axes_step2) <- c("acc.x", "acc.y", "acc.z", "gyro.x", "gyro.y", "gyro.z")
axes_step5 <- cbind.data.frame(test_front_step5$acc.x, test_front_step5$acc.y, test_front_step5$acc.z, test_front_step5$gyro.x, test_front_step5$gyro.y, test_front_step5$gyro.z)
names(axes_step5) <- c("acc.x", "acc.y", "acc.z", "gyro.x", "gyro.y", "gyro.z")
axes_step6_stairDown <- cbind.data.frame(test_front_step6_stairDown$acc.x, test_front_step6_stairDown$acc.y, test_front_step6_stairDown$acc.z, test_front_step6_stairDown$gyro.x, test_front_step6_stairDown$gyro.y, test_front_step6_stairDown$gyro.z)
names(axes_step6_stairDown) <- c("acc.x", "acc.y", "acc.z", "gyro.x", "gyro.y", "gyro.z")
axes_step6_stairUp <- cbind.data.frame(test_front_step6_stairUp$acc.x, test_front_step6_stairUp$acc.y, test_front_step6_stairUp$acc.z, test_front_step6_stairUp$gyro.x, test_front_step6_stairUp$gyro.y, test_front_step6_stairUp$gyro.z)
names(axes_step6_stairUp) <- c("acc.x", "acc.y", "acc.z", "gyro.x", "gyro.y", "gyro.z")

### apply the movelet method
# parameters
##number of samples per second
frequency <- 10
##Length of movelet in seconds
moveletLength <- 1
##Measure of distance for movelet method
distOption <- "L2" 
##Activities in the training data
trainingActivities <- c("walk",        
                        "stairUp",    
                        "stairDown", 
                        "stand",
                        "standToSit",
                        "sit",
                        "sitToStand")
##Indicator of whether you would like to use magnitude data or tri-axial data
useMag <- FALSE

# Apply the movelet method

combined_result_step1 <- movelet_Bai2012_singleSensor_modified(test_front_step1, axes_step1, training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(combined_result_step1, "~/Desktop/data/subject1/test/Prediction/Step1/5s Training Set/standardized data/front_pred.csv")
combined_result_step2 <- movelet_Bai2012_singleSensor_modified(test_front_step2, axes_step2, training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(combined_result_step2, "~/Desktop/data/subject1/test/Prediction/Step2/5s Training Set/standardized data/front_pred.csv")
combined_result_step5 <- movelet_Bai2012_singleSensor_modified(test_front_step5, axes_step5, training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(combined_result_step5, "~/Desktop/data/subject1/test/Prediction/Step5/5s Training Set/standardized data/front_pred.csv")
combined_result_step6_stairUp <- movelet_Bai2012_singleSensor_modified(test_front_step6_stairUp, axes_step6_stairUp, training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(combined_result_step6_stairUp, "~/Desktop/data/subject1/test/Prediction/Step6/5s Training Set/stairUp/standardized data/front_pred.csv")
combined_result_step6_stairDown <- movelet_Bai2012_singleSensor_modified(test_front_step6_stairDown, axes_step6_stairDown, training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(combined_result_step6_stairDown, "~/Desktop/data/subject1/test/Prediction/Step6/5s Training Set/stairDown/standardized data/front_pred.csv")

acc_result_step1 <- movelet_Bai2012_singleSensor_modified(test_acc_front_step1, acc_axes_step1, acc_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(acc_result_step1, "~/Desktop/data/subject1/test/Prediction/Step1/5s Training Set/standardized data/acc_front_pred.csv")
acc_result_step2 <- movelet_Bai2012_singleSensor_modified(test_acc_front_step2, acc_axes_step2, acc_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(acc_result_step2, "~/Desktop/data/subject1/test/Prediction/Step2/5s Training Set/standardized data/acc_front_pred.csv")
acc_result_step5 <- movelet_Bai2012_singleSensor_modified(test_acc_front_step5, acc_axes_step5, acc_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(acc_result_step5, "~/Desktop/data/subject1/test/Prediction/Step5/5s Training Set/standardized data/acc_front_pred.csv")
acc_result_step6_stairDown <- movelet_Bai2012_singleSensor_modified(test_acc_front_step6_stairDown, acc_axes_step6_stairDown, acc_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(acc_result_step6_stairDown, "~/Desktop/data/subject1/test/Prediction/Step6/5s Training Set/stairDown/standardized data/acc_front_pred.csv")
acc_result_step6_stairUp <- movelet_Bai2012_singleSensor_modified(test_acc_front_step6_stairUp, acc_axes_step6_stairUp, acc_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(acc_result_step6_stairUp, "~/Desktop/data/subject1/test/Prediction/Step6/5s Training Set/stairUp/standardized data/acc_front_pred.csv")

gyro_result_step1 <- movelet_Bai2012_singleSensor_modified(test_gyro_front_step1, gyro_axes_step1, gyro_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(gyro_result_step1, "~/Desktop/data/subject1/test/Prediction/Step1/5s Training Set/standardized data/gyro_front_pred.csv")
gyro_result_step2 <- movelet_Bai2012_singleSensor_modified(test_gyro_front_step2, gyro_axes_step2, gyro_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(gyro_result_step2, "~/Desktop/data/subject1/test/Prediction/Step2/5s Training Set/standardized data/gyro_front_pred.csv")
gyro_result_step5 <- movelet_Bai2012_singleSensor_modified(test_gyro_front_step5, gyro_axes_step5, gyro_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(gyro_result_step5, "~/Desktop/data/subject1/test/Prediction/Step5/5s Training Set/standardized data/gyro_front_pred.csv")
gyro_result_step5 <- movelet_Bai2012_singleSensor_modified(test_gyro_front_step5, gyro_axes_step5, gyro_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(gyro_result_step5, "~/Desktop/data/subject1/test/Prediction/Step5/5s Training Set/standardized data/gyro_front_pred.csv")
gyro_result_step6_stairDown <- movelet_Bai2012_singleSensor_modified(test_gyro_front_step6_stairDown, gyro_axes_step6_stairDown, gyro_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(gyro_result_step6_stairDown, "~/Desktop/data/subject1/test/Prediction/Step6/5s Training Set/stairDown/standardized data/gyro_front_pred.csv")
gyro_result_step6_stairUp <- movelet_Bai2012_singleSensor_modified(test_gyro_front_step6_stairUp, gyro_axes_step6_stairUp, gyro_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(gyro_result_step6_stairUp, "~/Desktop/data/subject1/test/Prediction/Step6/5s Training Set/stairUp/standardized data/gyro_front_pred.csv")

# prediction plots
activityList <- c("walk", "stand","stairUp", "stairDown","standToSit","sit","sitToStand")

##activityCols = the colors corresponding to the activities
activityCols <-  c("#000000", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#CC79A7", "#D55E00")
colorBlindBlack8  <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##xRange is a time elapsed range 
##if NA, xRange is set to the min and max of timestamp in the result data frame
xRange <- NA

##Make a two panel plot
##true labels on the top
##predicted labels on the bottom
plotActivityPrediction(combined_result_step1, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(combined_result_step2, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(combined_result_step5, xRange, activityList, activityCols, TRUE))
plotActivityPrediction(combined_result_step6_stairUp, xRange, activityList, activityCols, TRUE))
plotActivityPrediction(combined_result_step6_stairDown, xRange, activityList, activityCols, TRUE))

plotActivityPrediction(acc_result_step1, xRange, activityList, activityCols, TRUE))
plotActivityPrediction(acc_result_step2, xRange, activityList, activityCols, TRUE))
plotActivityPrediction(acc_result_step5, xRange, activityList, activityCols, TRUE))
plotActivityPrediction(acc_result_step6_stairDown, xRange, activityList, activityCols, TRUE))
plotActivityPrediction(acc_result_step6_stairUp, xRange, activityList, activityCols, TRUE))

plotActivityPrediction(gyro_result_step1, xRange, activityList, activityCols, TRUE))
plotActivityPrediction(gyro_result_step2, xRange, activityList, activityCols, TRUE))
plotActivityPrediction(gyro_result_step5, xRange, activityList, activityCols, TRUE))
plotActivityPrediction(gyro_result_step6_stairDown, xRange, activityList, activityCols, TRUE))
plotActivityPrediction(gyro_result_step6_stairUp, xRange, activityList, activityCols, TRUE))
