library(data.table)
source("~/Desktop/WFU/URECA/R_template/six axes/Modified Movelet Method.R")
source("~/Desktop/WFU/URECA/R_template/Combine Acc and Gyro Data Using Linear Interpolation.R")
# load data

# acc only
# training 
acc_front_walk <- read.csv("~/Desktop/data/subject1/training/walk/acc_front.csv")
acc_front_stand <- read.csv("~/Desktop/data/subject1/training/stand/acc_front.csv")
acc_front_stairUp <- read.csv("~/Desktop/data/subject1/training/stairUp/acc_front.csv")
acc_front_stairDown <- read.csv("~/Desktop/data/subject1/training/stairDown/acc_front.csv")
acc_front_chairStand1 <- read.csv("~/Desktop/data/subject1/training/chairStand1/acc_front.csv")

acc_standTime <- setDT(acc_front_stand)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timeElapsed
acc_trainStand <- subset(acc_front_stand, acc_front_stand$timeElapsed > (acc_standTime - 2.5) & acc_front_stand$timeElapsed < (acc_standTime + 2.5))
acc_walkTime <- setDT(acc_front_walk)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timeElapsed
acc_trainWalk <- subset(acc_front_walk, acc_front_walk$timeElapsed > (acc_walkTime - 2.5) & acc_front_walk$timeElapsed < (acc_walkTime + 2.5))
acc_stairUpTime <- setDT(acc_front_stairUp)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timeElapsed
acc_trainStairUp<- subset(acc_front_stairUp, acc_front_stairUp$timeElapsed > (acc_stairUpTime - 2.5) & acc_front_stairUp$timeElapsed < (acc_stairUpTime + 2.5))
acc_stairDownTime <- setDT(acc_front_stairDown)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timeElapsed
acc_trainStairDown <- subset(acc_front_stairDown, acc_front_stairDown$timeElapsed > (acc_stairDownTime - 2.5) & acc_front_stairDown$timeElapsed < (acc_stairDownTime + 2.5))

acc_trainingSet <- rbind.data.frame(acc_trainWalk, acc_trainStand, acc_trainStairUp, acc_trainStairDown, acc_front_chairStand1)
acc_training <- cbind.data.frame(acc_trainingSet$x, acc_trainingSet$y, acc_trainingSet$z, acc_trainingSet$label)
names(acc_training) <- c("acc.x", "acc.y", "acc.z", "label")

# test
test_acc_front <- read.csv("~/Desktop/data/subject1/test/Step1/acc_front.csv")
acc_axes <- cbind.data.frame(test_acc_front$x, test_acc_front$y, test_acc_front$z)
names(acc_axes) <- c("acc.x", "acc.y", "acc.z")

# gyro only
# training
gyro_front_walk <- read.csv("~/Desktop/data/subject1/training/walk/gyro_front.csv")
gyro_front_stand <- read.csv("~/Desktop/data/subject1/training/stand/gyro_front.csv")
gyro_front_stairUp <- read.csv("~/Desktop/data/subject1/training/stairUp/gyro_front.csv")
gyro_front_stairDown <- read.csv("~/Desktop/data/subject1/training/stairDown/gyro_front.csv")
gyro_front_chairStand1 <- read.csv("~/Desktop/data/subject1/training/chairStand1/gyro_front.csv")

gyro_standTime <- setDT(gyro_front_stand)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timeElapsed
gyro_trainStand <- subset(gyro_front_stand, gyro_front_stand$timeElapsed > (gyro_standTime - 2.5) & gyro_front_stand$timeElapsed < (gyro_standTime + 2.5))
gyro_walkTime <- setDT(gyro_front_walk)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timeElapsed
gyro_trainWalk <- subset(gyro_front_walk, gyro_front_walk$timeElapsed > (gyro_walkTime - 2.5) & gyro_front_walk$timeElapsed < (gyro_walkTime + 2.5))
gyro_stairUpTime <- setDT(gyro_front_stairUp)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timeElapsed
gyro_trainStairUp<- subset(gyro_front_stairUp, gyro_front_stairUp$timeElapsed > (gyro_stairUpTime - 2.5) & gyro_front_stairUp$timeElapsed < (gyro_stairUpTime + 2.5))
gyro_stairDownTime <- setDT(gyro_front_stairDown)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timeElapsed
gyro_trainStairDown <- subset(gyro_front_stairDown, gyro_front_stairDown$timeElapsed > (gyro_stairDownTime - 2.5) & gyro_front_stairDown$timeElapsed < (gyro_stairDownTime + 2.5))

gyro_trainingSet <- rbind.data.frame(gyro_trainWalk, gyro_trainStand, gyro_trainStairUp, gyro_trainStairDown, gyro_front_chairStand1)
gyro_training <- cbind.data.frame(gyro_trainingSet$x, gyro_trainingSet$y, gyro_trainingSet$z, gyro_trainingSet$label)
names(gyro_training) <- c("gyro.x", "gyro.y", "gyro.z", "label")

# test
test_gyro_front <- read.csv("~/Desktop/data/subject1/test/Step1/gyro_front.csv")
gyro_axes <- cbind.data.frame(test_gyro_front$x, test_gyro_front$y, test_gyro_front$z)
names(gyro_axes) <- c("gyro.x", "gyro.y", "gyro.z")

# combined data
# training
walk_front <- read.csv("~/Desktop/data/subject1/training/combined/walk_front.csv")
stand_front <- read.csv("~/Desktop/data/subject1/training/combined/stand_front.csv")
stairUp_front <- read.csv("~/Desktop/data/subject1/training/combined/stairUp_front.csv")
stairDown_front <- read.csv("~/Desktop/data/subject1/training/combined/stairDown_front.csv")
chairStand1_front <- read.csv("~/Desktop/data/subject1/training/combined/chairStand1_front.csv")

standTime <- setDT(stand_front)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$acc.timestamp
trainStand <- subset(stand_front, stand_front$acc.timestamp > (standTime - 2.5) & stand_front$acc.timestamp < (standTime + 2.5))
walkTime <- setDT(walk_front)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$acc.timestamp
trainWalk <- subset(walk_front, walk_front$acc.timestamp > (walkTime - 2.5) & walk_front$acc.timestamp < (walkTime + 2.5))
stairUpTime <- setDT(stairUp_front)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$acc.timestamp
trainStairUp<- subset(stairUp_front, stairUp_front$acc.timestamp > (stairUpTime - 2.5) & stairUp_front$acc.timestamp < (stairUpTime + 2.5))
stairDownTime <- setDT(stairDown_front)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$acc.timestamp
trainStairDown <- subset(stairDown_front, stairDown_front$acc.timestamp > (stairDownTime - 2.5) & stairDown_front$acc.timestamp < (stairDownTime + 2.5))

trainingSet <- rbind.data.frame(trainWalk, trainStand, trainStairUp, trainStairDown, chairStand1_front)
training <- cbind.data.frame(trainingSet$acc.x, trainingSet$acc.y, trainingSet$acc.z, trainingSet$gyro.x, trainingSet$gyro.y, trainingSet$gyro.z, trainingSet$label)
names(training) <- c("acc.x", "acc.y", "acc.z", "gyro.x", "gyro.y", "gyro.z", "label")

# test
test_front <- combineData(test_acc_front, test_gyro_front)
names(test_front) <- c("timestamp", "acc.x", "acc.y", "acc.z","label", "gyro.x", "gyro.y", "gyro.z")
write.csv(test_front, "~/Desktop/data/subject1/test/Step1/combined_front.csv")

axes <- cbind.data.frame(test_front$acc.x, test_front$acc.y, test_front$acc.z, test_front$gyro.x, test_front$gyro.y, test_front$gyro.z)
names(axes) <- c("acc.x", "acc.y", "acc.z", "gyro.x", "gyro.y", "gyro.z")


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
                        "stand")
##Indicator of whether you would like to use magnitude data or tri-axial data
useMag <- FALSE

# Apply the movelet method
combined_result <- movelet_Bai2012_singleSensor_modified(test_front, axes, training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(combined_result, "~/Desktop/data/subject1/test/Prediction/Step1/front_pred.csv")
acc_result <- movelet_Bai2012_singleSensor_modified(test_acc_front, acc_axes, acc_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(acc_result, "~/Desktop/data/subject1/test/Prediction/Step1/acc_front_pred.csv")
gyro_result <- movelet_Bai2012_singleSensor_modified(test_gyro_front, gyro_axes, gyro_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(gyro_result, "~/Desktop/data/subject1/test/Prediction/Step1/gyro_front_pred.csv")


# prediction plots
activityList <- c("walk", "stand","stairUp", "stairDown")

##activityCols = the colors corresponding to the activities
activityCols <- c("green", "black", "red", "blue")

##xRange is a time elapsed range 
##if NA, xRange is set to the min and max of timeElapsed in the result data frame
xRange <- NA

##Make a two panel plot
##true labels on the top
##predicted labels on the bottom
plotActivityPrediction(combined_result, xRange, activityList, activityCols)
plotActivityPrediction(acc_result, xRange, activityList, activityCols)
plotActivityPrediction(gyro_result, xRange, activityList, activityCols)
