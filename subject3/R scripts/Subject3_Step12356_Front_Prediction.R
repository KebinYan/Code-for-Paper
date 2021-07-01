library(data.table)
library(gridExtra)
source("~/Desktop/WFU/URECA/R_template/six axes/Modified Movelet Method.R")
source("~/Desktop/WFU/URECA/R_template/Combine Acc and Gyro Data Using Linear Interpolation.R")

# training set size in seconds
trainingSec <- 5000

# acc only

# training 
# load data
acc_front_walk <- read.csv("~/Desktop/csv/subject3/training/walk/acc_front.csv")
acc_front_stand <- read.csv("~/Desktop/csv/subject3/training/stand/acc_front.csv")
acc_front_stairUp <- read.csv("~/Desktop/csv/subject3/training/stairUp/acc_front.csv")
acc_front_stairDown <- read.csv("~/Desktop/csv/subject3/training/stairDown/acc_front.csv")
acc_front_chairStand1 <- read.csv("~/Desktop/csv/subject3/training/chairStand1/acc_front.csv")

# get the middle 5 seconds for training set
acc_standTime <- setDT(acc_front_stand)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
acc_trainStand <- subset(acc_front_stand, acc_front_stand$timestamp > (acc_standTime - trainingSec/2) & acc_front_stand$timestamp < (acc_standTime + trainingSec/2))
acc_walkTime <- setDT(acc_front_walk)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
acc_trainWalk <- subset(acc_front_walk, acc_front_walk$timestamp > (acc_walkTime - trainingSec/2) & acc_front_walk$timestamp < (acc_walkTime + trainingSec/2))
acc_stairUpTime <- setDT(acc_front_stairUp)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
acc_trainStairUp<- subset(acc_front_stairUp, acc_front_stairUp$timestamp > (acc_stairUpTime - trainingSec/2) & acc_front_stairUp$timestamp < (acc_stairUpTime + trainingSec/2))
acc_stairDownTime <- setDT(acc_front_stairDown)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
acc_trainStairDown <- subset(acc_front_stairDown, acc_front_stairDown$timestamp > (acc_stairDownTime - trainingSec/2) & acc_front_stairDown$timestamp < (acc_stairDownTime + trainingSec/2))

acc_front_sit <- subset(acc_front_chairStand1, label == "sit1")
acc_sitTime <- setDT(acc_front_sit)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
acc_trainSit <- subset(acc_front_sit, acc_front_sit$timestamp > (acc_sitTime - trainingSec/2) & acc_front_sit$timestamp < (acc_sitTime + trainingSec/2))
acc_front_sitToStand <- subset(acc_front_chairStand1, label == "sitToStand1")
acc_front_standToSit <- subset(acc_front_chairStand1, label == "standToSit1")

# create a training set with all activities
acc_trainingSet <- rbind.data.frame(acc_trainWalk, acc_trainStand, acc_trainStairUp, acc_trainStairDown, acc_trainSit, acc_front_sitToStand, acc_front_standToSit)

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
test_acc_front_step1 <- read.csv("~/Desktop/csv/subject3/test/Step1/acc_front.csv")
test_acc_front_step2 <- read.csv("~/Desktop/csv/subject3/test/Step2/acc_front.csv")
test_acc_front_step3_fastWalk <- read.csv("~/Desktop/csv/subject3/test/Step3/fast_walk/acc_front.csv")
test_acc_front_step3_normalWalk <- read.csv("~/Desktop/csv/subject3/test/Step3/normal_walk/acc_front.csv")
test_acc_front_step3_slowWalk <- read.csv("~/Desktop/csv/subject3/test/Step3/slow_walk/acc_front.csv")
test_acc_front_step5 <- read.csv("~/Desktop/csv/subject3/test/Step5/acc_front.csv")
test_acc_front_step6_stairDown <- read.csv("~/Desktop/csv/subject3/test/Step6/stairDown/acc_front.csv")
test_acc_front_step6_stairUp <- read.csv("~/Desktop/csv/subject3/test/Step6/stairUp/acc_front.csv")

# create an axes dataset including only the axes
acc_axes_step1 <- cbind.data.frame(test_acc_front_step1$x, test_acc_front_step1$y, test_acc_front_step1$z)
names(acc_axes_step1) <- c("acc.x", "acc.y", "acc.z")
acc_axes_step2 <- cbind.data.frame(test_acc_front_step2$x, test_acc_front_step2$y, test_acc_front_step2$z)
names(acc_axes_step2) <- c("acc.x", "acc.y", "acc.z")
acc_axes_step3_fastWalk <- cbind.data.frame(test_acc_front_step3_fastWalk$x, test_acc_front_step3_fastWalk$y, test_acc_front_step3_fastWalk$z)
names(acc_axes_step3_fastWalk) <- c("acc.x", "acc.y", "acc.z")
acc_axes_step3_normalWalk <- cbind.data.frame(test_acc_front_step3_normalWalk$x, test_acc_front_step3_normalWalk$y, test_acc_front_step3_normalWalk$z)
names(acc_axes_step3_normalWalk) <- c("acc.x", "acc.y", "acc.z")
acc_axes_step3_slowWalk <- cbind.data.frame(test_acc_front_step3_slowWalk$x, test_acc_front_step3_slowWalk$y, test_acc_front_step3_slowWalk$z)
names(acc_axes_step3_slowWalk) <- c("acc.x", "acc.y", "acc.z")
acc_axes_step5 <- cbind.data.frame(test_acc_front_step5$x, test_acc_front_step5$y, test_acc_front_step5$z)
names(acc_axes_step5) <- c("acc.x", "acc.y", "acc.z")
acc_axes_step6_stairDown <- cbind.data.frame(test_acc_front_step6_stairDown$x, test_acc_front_step6_stairDown$y, test_acc_front_step6_stairDown$z)
names(acc_axes_step6_stairDown) <- c("acc.x", "acc.y", "acc.z")
acc_axes_step6_stairUp <- cbind.data.frame(test_acc_front_step6_stairUp$x, test_acc_front_step6_stairUp$y, test_acc_front_step6_stairUp$z)
names(acc_axes_step6_stairUp) <- c("acc.x", "acc.y", "acc.z")

# gyro only

# training
# load data
gyro_front_walk <- read.csv("~/Desktop/csv/subject3/training/walk/gyro_front.csv")
gyro_front_stand <- read.csv("~/Desktop/csv/subject3/training/stand/gyro_front.csv")
gyro_front_stairUp <- read.csv("~/Desktop/csv/subject3/training/stairUp/gyro_front.csv")
gyro_front_stairDown <- read.csv("~/Desktop/csv/subject3/training/stairDown/gyro_front.csv")
gyro_front_chairStand1 <- read.csv("~/Desktop/csv/subject3/training/chairStand1/gyro_front.csv")

# get the middle 5 seconds for training set
gyro_standTime <- setDT(gyro_front_stand)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
gyro_trainStand <- subset(gyro_front_stand, gyro_front_stand$timestamp > (gyro_standTime - trainingSec/2) & gyro_front_stand$timestamp < (gyro_standTime + trainingSec/2))
gyro_walkTime <- setDT(gyro_front_walk)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
gyro_trainWalk <- subset(gyro_front_walk, gyro_front_walk$timestamp > (gyro_walkTime - trainingSec/2) & gyro_front_walk$timestamp < (gyro_walkTime + trainingSec/2))
gyro_stairUpTime <- setDT(gyro_front_stairUp)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
gyro_trainStairUp<- subset(gyro_front_stairUp, gyro_front_stairUp$timestamp > (gyro_stairUpTime - trainingSec/2) & gyro_front_stairUp$timestamp < (gyro_stairUpTime + trainingSec/2))
gyro_stairDownTime <- setDT(gyro_front_stairDown)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
gyro_trainStairDown <- subset(gyro_front_stairDown, gyro_front_stairDown$timestamp > (gyro_stairDownTime - trainingSec/2) & gyro_front_stairDown$timestamp < (gyro_stairDownTime + trainingSec/2))

gyro_front_sit <- subset(gyro_front_chairStand1, label == "sit1")
gyro_sitTime <- setDT(gyro_front_sit)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
gyro_trainSit <- subset(gyro_front_sit, gyro_front_sit$timestamp > (gyro_sitTime - trainingSec/2) & gyro_front_sit$timestamp < (gyro_sitTime + trainingSec/2))

gyro_front_sitToStand <- subset(gyro_front_chairStand1, label == "sitToStand1")
gyro_front_standToSit <- subset(gyro_front_chairStand1, label == "standToSit1")

# create a training set with all activities
gyro_trainingSet <- rbind.data.frame(gyro_trainWalk, gyro_trainStand, gyro_trainStairUp, gyro_trainStairDown, gyro_trainSit, gyro_front_sitToStand, gyro_front_standToSit)

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
test_gyro_front_step1 <- read.csv("~/Desktop/csv/subject3/test/Step1/gyro_front.csv")
test_gyro_front_step2 <- read.csv("~/Desktop/csv/subject3/test/Step2/gyro_front.csv")
test_gyro_front_step3_fastWalk <- read.csv("~/Desktop/csv/subject3/test/Step3/fast_walk/gyro_front.csv")
test_gyro_front_step3_normalWalk <- read.csv("~/Desktop/csv/subject3/test/Step3/normal_walk/gyro_front.csv")
test_gyro_front_step3_slowWalk <- read.csv("~/Desktop/csv/subject3/test/Step3/slow_walk/gyro_front.csv")
test_gyro_front_step5 <- read.csv("~/Desktop/csv/subject3/test/Step5/gyro_front.csv")
test_gyro_front_step6_stairDown <- read.csv("~/Desktop/csv/subject3/test/Step6/stairDown/gyro_front.csv")
test_gyro_front_step6_stairUp <- read.csv("~/Desktop/csv/subject3/test/Step6/stairUp/gyro_front.csv")

# create an axes dataset including only the axes
gyro_axes_step1 <- cbind.data.frame(test_gyro_front_step1$x, test_gyro_front_step1$y, test_gyro_front_step1$z)
names(gyro_axes_step1) <- c("gyro.x", "gyro.y", "gyro.z")
gyro_axes_step2 <- cbind.data.frame(test_gyro_front_step2$x, test_gyro_front_step2$y, test_gyro_front_step2$z)
names(gyro_axes_step2) <- c("gyro.x", "gyro.y", "gyro.z")
gyro_axes_step3_fastWalk <- cbind.data.frame(test_gyro_front_step3_fastWalk$x, test_gyro_front_step3_fastWalk$y, test_gyro_front_step3_fastWalk$z)
names(gyro_axes_step3_fastWalk) <- c("gyro.x", "gyro.y", "gyro.z")
gyro_axes_step3_normalWalk <- cbind.data.frame(test_gyro_front_step3_normalWalk$x, test_gyro_front_step3_normalWalk$y, test_gyro_front_step3_normalWalk$z)
names(gyro_axes_step3_normalWalk) <- c("gyro.x", "gyro.y", "gyro.z")
gyro_axes_step3_slowWalk <- cbind.data.frame(test_gyro_front_step3_slowWalk$x, test_gyro_front_step3_slowWalk$y, test_gyro_front_step3_slowWalk$z)
names(gyro_axes_step3_slowWalk) <- c("gyro.x", "gyro.y", "gyro.z")
gyro_axes_step5 <- cbind.data.frame(test_gyro_front_step5$x, test_gyro_front_step5$y, test_gyro_front_step5$z)
names(gyro_axes_step5) <- c("gyro.x", "gyro.y", "gyro.z")
gyro_axes_step6_stairDown <- cbind.data.frame(test_gyro_front_step6_stairDown$x, test_gyro_front_step6_stairDown$y, test_gyro_front_step6_stairDown$z)
names(gyro_axes_step6_stairDown) <- c("gyro.x", "gyro.y", "gyro.z")
gyro_axes_step6_stairUp <- cbind.data.frame(test_gyro_front_step6_stairUp$x, test_gyro_front_step6_stairUp$y, test_gyro_front_step6_stairUp$z)
names(gyro_axes_step6_stairUp) <- c("gyro.x", "gyro.y", "gyro.z")

# combined data

# training

# create combined data sets
combined_walk <- combineData(acc_front_walk, gyro_front_walk)
combined_stand <- combineData(acc_front_stand, gyro_front_stand)
combined_stairUp <- combineData(acc_front_stairUp, gyro_front_stairUp)
combined_stairDown <- combineData(acc_front_stairDown, gyro_front_stairDown)
combined_chairStand1 <- combineData(acc_front_chairStand1, gyro_front_chairStand1)
write.csv(combined_walk, "~/Desktop/csv/subject3/training/combined/walk_front.csv")
write.csv(combined_stand, "~/Desktop/csv/subject3/training/combined/stand_front.csv")
write.csv(combined_stairUp, "~/Desktop/csv/subject3/training/combined/stairUp_front.csv")
write.csv(combined_stairDown, "~/Desktop/csv/subject3/training/combined/stairDown_front.csv")
write.csv(combined_chairStand1, "~/Desktop/csv/subject3/training/combined/chairStand1_front.csv")

# load combined data: the data sets were created from above code
walk_front <- read.csv("~/Desktop/csv/subject3/training/combined/walk_front.csv")
stand_front <- read.csv("~/Desktop/csv/subject3/training/combined/stand_front.csv")
stairUp_front <- read.csv("~/Desktop/csv/subject3/training/combined/stairUp_front.csv")
stairDown_front <- read.csv("~/Desktop/csv/subject3/training/combined/stairDown_front.csv")
chairStand1_front <- read.csv("~/Desktop/csv/subject3/training/combined/chairStand1_front.csv")

# get the middle 5 seconds for training set
standTime <- setDT(stand_front)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
trainStand <- subset(stand_front, stand_front$timestamp > (standTime - trainingSec/2) & stand_front$timestamp < (standTime + trainingSec/2))
walkTime <- setDT(walk_front)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
trainWalk <- subset(walk_front, walk_front$timestamp > (walkTime - trainingSec/2) & walk_front$timestamp < (walkTime + trainingSec/2))
stairUpTime <- setDT(stairUp_front)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
trainStairUp<- subset(stairUp_front, stairUp_front$timestamp > (stairUpTime - trainingSec/2) & stairUp_front$timestamp < (stairUpTime + trainingSec/2))
stairDownTime <- setDT(stairDown_front)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
trainStairDown <- subset(stairDown_front, stairDown_front$timestamp > (stairDownTime - trainingSec/2) & stairDown_front$timestamp < (stairDownTime + trainingSec/2))

front_sit <- subset(chairStand1_front, label == "sit1")
sitTime <- setDT(front_sit)[, if(.N > 1) .SD[ceiling(.N/2):ceiling(.N/2)] else .SD]$timestamp
trainSit <- subset(front_sit, front_sit$timestamp > (sitTime - trainingSec/2) & front_sit$timestamp < (sitTime + trainingSec/2))

front_sitToStand <- subset(chairStand1_front, label == "sitToStand1")
front_standToSit <- subset(chairStand1_front, label == "standToSit1")

# create a training set with all activities
trainingSet <- rbind.data.frame(trainWalk, trainStand, trainStairUp, trainStairDown, front_sit, front_sitToStand, front_standToSit)

# change the label in training set to the same notations as in the test set
for(i in 1:nrow(trainingSet)){
  trainingSet[i]$label <- ifelse(trainingSet[i]$label=="sit1","sit", trainingSet[i]$label)
  trainingSet[i]$label <- ifelse(trainingSet[i]$label=="sitToStand1","sitToStand", trainingSet[i]$label)
  trainingSet[i]$label <- ifelse(trainingSet[i]$label=="standToSit1","standToSit", trainingSet[i]$label)
}

# create a training dataset with the 3 axes and the label column
training <- cbind.data.frame(trainingSet$acc.x, trainingSet$acc.y, trainingSet$acc.z, trainingSet$gyro.x, trainingSet$gyro.y, trainingSet$gyro.z, trainingSet$label)
names(training) <- c("acc.x", "acc.y", "acc.z", "gyro.x", "gyro.y", "gyro.z", "label")

# test

# apply the interpolation method to make timestamp for accelerometer and gyroscope the same
test_front_step1 <- combineData(test_acc_front_step1, test_gyro_front_step1)
write.csv(test_front_step1, "~/Desktop/csv/subject3/test/Step1/combined_front.csv")
test_front_step2 <- combineData(test_acc_front_step2, test_gyro_front_step2)
write.csv(test_front_step2, "~/Desktop/csv/subject3/test/Step2/combined_front.csv")
test_front_step3_fastWalk <- combineData(test_acc_front_step3_fastWalk, test_gyro_front_step3_fastWalk)
write.csv(test_front_step3_fastWalk, "~/Desktop/csv/subject3/test/Step3/fast_walk/combined_front.csv")
test_front_step3_normalWalk <- combineData(test_acc_front_step3_normalWalk, test_gyro_front_step3_normalWalk)
write.csv(test_front_step3_normalWalk, "~/Desktop/csv/subject3/test/Step3/normal_walk/combined_front.csv")
test_front_step3_slowWalk <- combineData(test_acc_front_step3_slowWalk, test_gyro_front_step3_slowWalk)
write.csv(test_front_step3_slowWalk, "~/Desktop/csv/subject3/test/Step3/slow_walk/combined_front.csv")
test_front_step5 <- combineData(test_acc_front_step5, test_gyro_front_step5)
write.csv(test_front_step5, "~/Desktop/csv/subject3/test/Step5/combined_front.csv")
test_front_step6_stairDown <- combineData(test_acc_front_step6_stairDown, test_gyro_front_step6_stairDown)
write.csv(test_front_step6_stairDown, "~/Desktop/csv/subject3/test/Step6/stairDown/combined_front.csv")
test_front_step6_stairUp <- combineData(test_acc_front_step6_stairUp, test_gyro_front_step6_stairUp)
write.csv(test_front_step6_stairUp, "~/Desktop/csv/subject3/test/Step6/stairUp/combined_front.csv")

# the data sets were created by the above codes
test_front_step1 <- read.csv("~/Desktop/csv/subject3/test/Step1/combined_front.csv")
test_front_step2 <- read.csv("~/Desktop/csv/subject3/test/Step2/combined_front.csv")
test_front_step5 <- read.csv("~/Desktop/csv/subject3/test/Step5/combined_front.csv")
test_front_step3_fastWalk <- read.csv("~/Desktop/csv/subject3/test/Step3/fast_walk/combined_front.csv")
test_front_step3_normalWalk <- read.csv("~/Desktop/csv/subject3/test/Step3/normal_walk/combined_front.csv")
test_front_step3_slowWalk <- read.csv("~/Desktop/csv/subject3/test/Step3/slow_walk/combined_front.csv")
test_front_step6_stairDown <- read.csv("~/Desktop/csv/subject3/test/Step6/stairDown/combined_front.csv")
test_front_step6_stairUp <- read.csv("~/Desktop/csv/subject3/test/Step6/stairUp/combined_front.csv")

# create an axes dataset including only the axes
axes_step1 <- cbind.data.frame(test_front_step1$acc.x, test_front_step1$acc.y, test_front_step1$acc.z, test_front_step1$gyro.x, test_front_step1$gyro.y, test_front_step1$gyro.z)
names(axes_step1) <- c("acc.x", "acc.y", "acc.z", "gyro.x", "gyro.y", "gyro.z")
axes_step2 <- cbind.data.frame(test_front_step2$acc.x, test_front_step2$acc.y, test_front_step2$acc.z, test_front_step2$gyro.x, test_front_step2$gyro.y, test_front_step2$gyro.z)
names(axes_step2) <- c("acc.x", "acc.y", "acc.z", "gyro.x", "gyro.y", "gyro.z")
axes_step3_fastWalk <- cbind.data.frame(test_front_step3_fastWalk$acc.x, test_front_step3_fastWalk$acc.y, test_front_step3_fastWalk$acc.z, test_front_step3_fastWalk$gyro.x, test_front_step3_fastWalk$gyro.y, test_front_step3_fastWalk$gyro.z)
names(axes_step3_fastWalk) <- c("acc.x", "acc.y", "acc.z", "gyro.x", "gyro.y", "gyro.z")
axes_step3_normalWalk <- cbind.data.frame(test_front_step3_normalWalk$acc.x, test_front_step3_normalWalk$acc.y, test_front_step3_normalWalk$acc.z, test_front_step3_normalWalk$gyro.x, test_front_step3_normalWalk$gyro.y, test_front_step3_normalWalk$gyro.z)
names(axes_step3_normalWalk) <- c("acc.x", "acc.y", "acc.z", "gyro.x", "gyro.y", "gyro.z")
axes_step3_slowWalk <- cbind.data.frame(test_front_step3_slowWalk$acc.x, test_front_step3_slowWalk$acc.y, test_front_step3_slowWalk$acc.z, test_front_step3_slowWalk$gyro.x, test_front_step3_slowWalk$gyro.y, test_front_step3_slowWalk$gyro.z)
names(axes_step3_slowWalk) <- c("acc.x", "acc.y", "acc.z", "gyro.x", "gyro.y", "gyro.z")
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
write.csv(combined_result_step1, "~/Desktop/csv/subject3/test/Prediction/Step1/front_pred.csv")
combined_result_step2 <- movelet_Bai2012_singleSensor_modified(test_front_step2, axes_step2, training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(combined_result_step2, "~/Desktop/csv/subject3/test/Prediction/Step2/front_pred.csv")
combined_result_step3_fastWalk <- movelet_Bai2012_singleSensor_modified(test_front_step3_fastWalk, axes_step3_fastWalk, training, frequency, moveletLength, distOption, trainingActivities, useMag)
for(i in 1:nrow(combined_result_step3_fastWalk)){
  if(combined_result_step3_fastWalk$label[i] == "fast"){
    combined_result_step3_fastWalk$label[i] = "walk"
  }
}
write.csv(combined_result_step3_fastWalk, "~/Desktop/csv/subject3/test/Prediction/Step3/fast_walk/front_pred_fast.csv")
combined_result_step3_normalWalk <- movelet_Bai2012_singleSensor_modified(test_front_step3_normalWalk, axes_step3_normalWalk, training, frequency, moveletLength, distOption, trainingActivities, useMag)
for(i in 1:nrow(combined_result_step3_normalWalk)){
  if(combined_result_step3_normalWalk$label[i] == "normal"){
    combined_result_step3_normalWalk$label[i] = "walk"
  }
}
write.csv(combined_result_step3_normalWalk, "~/Desktop/csv/subject3/test/Prediction/Step3/normal_walk/front_pred_normal.csv")
combined_result_step3_slowWalk <- movelet_Bai2012_singleSensor_modified(test_front_step3_slowWalk, axes_step3_slowWalk, training, frequency, moveletLength, distOption, trainingActivities, useMag)
for(i in 1:nrow(combined_result_step3_slowWalk)){
  if(combined_result_step3_slowWalk$label[i] == "slow"){
    combined_result_step3_slowWalk$label[i] = "walk"
  }
}
write.csv(combined_result_step3_slowWalk, "~/Desktop/csv/subject3/test/Prediction/Step3/slow_walk/front_pred_slow.csv")
combined_result_step5 <- movelet_Bai2012_singleSensor_modified(test_front_step5, axes_step5, training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(combined_result_step5, "~/Desktop/csv/subject3/test/Prediction/Step5/front_pred.csv")
combined_result_step6_stairUp <- movelet_Bai2012_singleSensor_modified(test_front_step6_stairUp, axes_step6_stairUp, training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(combined_result_step6_stairUp, "~/Desktop/csv/subject3/test/Prediction/Step6/stairUp/front_pred.csv")
combined_result_step6_stairDown <- movelet_Bai2012_singleSensor_modified(test_front_step6_stairDown, axes_step6_stairDown, training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(combined_result_step6_stairDown, "~/Desktop/csv/subject3/test/Prediction/Step6/stairDown/front_pred.csv")

acc_result_step1 <- movelet_Bai2012_singleSensor_modified(test_acc_front_step1, acc_axes_step1, acc_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(acc_result_step1, "~/Desktop/csv/subject3/test/Prediction/Step1/acc_front_pred.csv")
acc_result_step2 <- movelet_Bai2012_singleSensor_modified(test_acc_front_step2, acc_axes_step2, acc_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(acc_result_step2, "~/Desktop/csv/subject3/test/Prediction/Step2/acc_front_pred.csv")
acc_result_step3_fastWalk <- movelet_Bai2012_singleSensor_modified(test_acc_front_step3_fastWalk, acc_axes_step3_fastWalk, acc_training, frequency, moveletLength, distOption, trainingActivities, useMag)
for(i in 1:nrow(acc_result_step3_fastWalk)){
  if(acc_result_step3_fastWalk$label[i] == "fast"){
    acc_result_step3_fastWalk$label[i] = "walk"
  }
}
write.csv(acc_result_step3_fastWalk, "~/Desktop/csv/subject3/test/Prediction/Step3/fast_walk/acc_front_pred_fast.csv")
acc_result_step3_normalWalk <- movelet_Bai2012_singleSensor_modified(test_acc_front_step3_normalWalk, acc_axes_step3_normalWalk, acc_training, frequency, moveletLength, distOption, trainingActivities, useMag)
for(i in 1:nrow(acc_result_step3_normalWalk)){
  if(acc_result_step3_normalWalk$label[i] == "normal"){
    acc_result_step3_normalWalk$label[i] = "walk"
  }
}
write.csv(acc_result_step3_normalWalk, "~/Desktop/csv/subject3/test/Prediction/Step3/normal_walk/acc_front_pred_normal.csv")
acc_result_step3_slowWalk <- movelet_Bai2012_singleSensor_modified(test_acc_front_step3_slowWalk, acc_axes_step3_slowWalk, acc_training, frequency, moveletLength, distOption, trainingActivities, useMag)
for(i in 1:nrow(acc_result_step3_slowWalk)){
  if(acc_result_step3_slowWalk$label[i] == "slow"){
    acc_result_step3_slowWalk$label[i] = "walk"
  }
}
write.csv(acc_result_step3_slowWalk, "~/Desktop/csv/subject3/test/Prediction/Step3/slow_walk/acc_front_pred_slow.csv")
acc_result_step5 <- movelet_Bai2012_singleSensor_modified(test_acc_front_step5, acc_axes_step5, acc_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(acc_result_step5, "~/Desktop/csv/subject3/test/Prediction/Step5/acc_front_pred.csv")
acc_result_step6_stairDown <- movelet_Bai2012_singleSensor_modified(test_acc_front_step6_stairDown, acc_axes_step6_stairDown, acc_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(acc_result_step6_stairDown, "~/Desktop/csv/subject3/test/Prediction/Step6/stairDown/acc_front_pred.csv")
acc_result_step6_stairUp <- movelet_Bai2012_singleSensor_modified(test_acc_front_step6_stairUp, acc_axes_step6_stairUp, acc_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(acc_result_step6_stairUp, "~/Desktop/csv/subject3/test/Prediction/Step6/stairUp/acc_front_pred.csv")

gyro_result_step1 <- movelet_Bai2012_singleSensor_modified(test_gyro_front_step1, gyro_axes_step1, gyro_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(gyro_result_step1, "~/Desktop/csv/subject3/test/Prediction/Step1/gyro_front_pred.csv")
gyro_result_step2 <- movelet_Bai2012_singleSensor_modified(test_gyro_front_step2, gyro_axes_step2, gyro_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(gyro_result_step2, "~/Desktop/csv/subject3/test/Prediction/Step2/gyro_front_pred.csv")
gyro_result_step3_fastWalk <- movelet_Bai2012_singleSensor_modified(test_gyro_front_step3_fastWalk, gyro_axes_step3_fastWalk, gyro_training, frequency, moveletLength, distOption, trainingActivities, useMag)
for(i in 1:nrow(gyro_result_step3_fastWalk)){
  if(gyro_result_step3_fastWalk$label[i] == "fast"){
    gyro_result_step3_fastWalk$label[i] = "walk"
  }
}
write.csv(gyro_result_step3_fastWalk, "~/Desktop/csv/subject3/test/Prediction/Step3/fast_walk/gyro_front_pred_fast.csv")
gyro_result_step3_normalWalk <- movelet_Bai2012_singleSensor_modified(test_gyro_front_step3_normalWalk, gyro_axes_step3_normalWalk, gyro_training, frequency, moveletLength, distOption, trainingActivities, useMag)
for(i in 1:nrow(gyro_result_step3_normalWalk)){
  if(gyro_result_step3_normalWalk$label[i] == "normal"){
    gyro_result_step3_normalWalk$label[i] = "walk"
  }
}
write.csv(gyro_result_step3_normalWalk, "~/Desktop/csv/subject3/test/Prediction/Step3/normal_walk/gyro_front_pred_normal.csv")
gyro_result_step3_slowWalk <- movelet_Bai2012_singleSensor_modified(test_gyro_front_step3_slowWalk, gyro_axes_step3_slowWalk, gyro_training, frequency, moveletLength, distOption, trainingActivities, useMag)
for(i in 1:nrow(gyro_result_step3_slowWalk)){
  if(gyro_result_step3_slowWalk$label[i] == "slow"){
    gyro_result_step3_slowWalk$label[i] = "walk"
  }
}
write.csv(gyro_result_step3_slowWalk, "~/Desktop/csv/subject3/test/Prediction/Step3/slow_walk/gyro_front_pred_slow.csv")
gyro_result_step5 <- movelet_Bai2012_singleSensor_modified(test_gyro_front_step5, gyro_axes_step5, gyro_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(gyro_result_step5, "~/Desktop/csv/subject3/test/Prediction/Step5/gyro_front_pred.csv")
gyro_result_step6_stairDown <- movelet_Bai2012_singleSensor_modified(test_gyro_front_step6_stairDown, gyro_axes_step6_stairDown, gyro_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(gyro_result_step6_stairDown, "~/Desktop/csv/subject3/test/Prediction/Step6/stairDown/gyro_front_pred.csv")
gyro_result_step6_stairUp <- movelet_Bai2012_singleSensor_modified(test_gyro_front_step6_stairUp, gyro_axes_step6_stairUp, gyro_training, frequency, moveletLength, distOption, trainingActivities, useMag)
write.csv(gyro_result_step6_stairUp, "~/Desktop/csv/subject3/test/Prediction/Step6/stairUp/gyro_front_pred.csv")

# prediction plots
activityList <- c("walk", "stand","stairUp", "stairDown","standToSit","sit","sitToStand")

##activityCols = the colors corresponding to the activities
activityCols <- c("#000000", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#CC79A7", "#D55E00")

##xRange is a time elapsed range 
##if NA, xRange is set to the min and max of timeElapsed in the result data frame
xRange <- NA

##Make a two panel plot
##true labels on the top
##predicted labels on the bottom
plotActivityPrediction(combined_result_step1, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(combined_result_step2, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(combined_result_step3_fastWalk, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(combined_result_step3_normalWalk, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(combined_result_step3_slowWalk, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(combined_result_step5, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(combined_result_step6_stairUp, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(combined_result_step6_stairDown, xRange, activityList, activityCols, TRUE)

plotActivityPrediction(acc_result_step1, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(acc_result_step2, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(acc_result_step3_fastWalk, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(acc_result_step3_normalWalk, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(acc_result_step3_slowWalk, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(acc_result_step5, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(acc_result_step6_stairDown, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(acc_result_step6_stairUp, xRange, activityList, activityCols, TRUE)

plotActivityPrediction(gyro_result_step1, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(gyro_result_step2, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(gyro_result_step3_fastWalk, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(gyro_result_step3_normalWalk, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(gyro_result_step3_slowWalk, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(gyro_result_step5, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(gyro_result_step6_stairDown, xRange, activityList, activityCols, TRUE)
plotActivityPrediction(gyro_result_step6_stairUp, xRange, activityList, activityCols, TRUE)