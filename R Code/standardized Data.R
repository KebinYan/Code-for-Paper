source("~/Desktop/WFU/URECA/R_template/Standardizing Data.R")

# training 
## acc
acc_front_walk <- read.csv("~/Desktop/data/subject1/training/walk/acc_front.csv")
acc_front_stand <- read.csv("~/Desktop/data/subject1/training/stand/acc_front.csv")
acc_front_stairUp <- read.csv("~/Desktop/data/subject1/training/stairUp/acc_front.csv")
acc_front_stairDown <- read.csv("~/Desktop/data/subject1/training/stairDown/acc_front.csv")
acc_front_chairStand1 <- read.csv("~/Desktop/data/subject1/training/chairStand1/acc_front.csv")

acc_front_walk_standardized <- standardizeData(acc_front_walk, "second")
write.csv(acc_front_walk_standardized, "~/Desktop/data/subject1/training/standardized Data/walk/acc_front.csv")
acc_front_stand_standardized <- standardizeData(acc_front_stand, "second")
write.csv(acc_front_stand_standardized, "~/Desktop/data/subject1/training/standardized Data/stand/acc_front.csv")
acc_front_stairUp_standardized <- standardizeData(acc_front_stairUp, "second")
write.csv(acc_front_stairUp_standardized, "~/Desktop/data/subject1/training/standardized Data/stairUp/acc_front.csv")
acc_front_stairDown_standardized <- standardizeData(acc_front_stairDown, "second")
write.csv(acc_front_stairDown_standardized, "~/Desktop/data/subject1/training/standardized Data/stairDown/acc_front.csv")
acc_front_chairStand1_standardized <- standardizeData(acc_front_chairStand1, "second")
write.csv(acc_front_chairStand1_standardized, "~/Desktop/data/subject1/training/standardized Data/chairStand1/acc_front.csv")

## gyro
gyro_front_walk <- read.csv("~/Desktop/data/subject1/training/walk/gyro_front.csv")
gyro_front_stand <- read.csv("~/Desktop/data/subject1/training/stand/gyro_front.csv")
gyro_front_stairUp <- read.csv("~/Desktop/data/subject1/training/stairUp/gyro_front.csv")
gyro_front_stairDown <- read.csv("~/Desktop/data/subject1/training/stairDown/gyro_front.csv")
gyro_front_chairStand1 <- read.csv("~/Desktop/data/subject1/training/chairStand1/gyro_front.csv")

gyro_front_walk_standardized <- standardizeData(gyro_front_walk, "second")
write.csv(gyro_front_walk_standardized, "~/Desktop/data/subject1/training/standardized Data/walk/gyro_front.csv")
gyro_front_stand_standardized <- standardizeData(gyro_front_stand, "second")
write.csv(gyro_front_stand_standardized, "~/Desktop/data/subject1/training/standardized Data/stand/gyro_front.csv")
gyro_front_stairUp_standardized <- standardizeData(gyro_front_stairUp, "second")
write.csv(gyro_front_stairUp_standardized, "~/Desktop/data/subject1/training/standardized Data/stairUp/gyro_front.csv")
gyro_front_stairDown_standardized <- standardizeData(gyro_front_stairDown, "second")
write.csv(gyro_front_stairDown_standardized, "~/Desktop/data/subject1/training/standardized Data/stairDown/gyro_front.csv")
gyro_front_chairStand1_standardized <- standardizeData(gyro_front_chairStand1, "second")
write.csv(gyro_front_chairStand1_standardized, "~/Desktop/data/subject1/training/standardized Data/chairStand1/gyro_front.csv")

# test

## acc
test_acc_front_step1 <- read.csv("~/Desktop/data/subject1/test/Step1/acc_front.csv")
test_acc_front_step2 <- read.csv("~/Desktop/data/subject1/test/Step2/acc_front.csv")
test_acc_front_step5 <- read.csv("~/Desktop/data/subject1/test/Step5/acc_front.csv")
test_acc_front_step6_stairDown <- read.csv("~/Desktop/data/subject1/test/Step6/stairDown/acc_front.csv")
test_acc_front_step6_stairUp <- read.csv("~/Desktop/data/subject1/test/Step6/stairUp/acc_front.csv")

test_acc_front_step1_standardized <- standardizeData(test_acc_front_step1, "ms")
test_acc_front_step2_standardized <- standardizeData(test_acc_front_step2, "ms")
test_acc_front_step5_standardized <- standardizeData(test_acc_front_step5, "ms")
test_acc_front_step6_stairUp_standardized <- standardizeData(test_acc_front_step6_stairUp, "ms")
test_acc_front_step6_stairDown_standardized <- standardizeData(test_acc_front_step6_stairDown, "ms")
write.csv(test_acc_front_step1_standardized, "~/Desktop/data/subject1/test/Step1/standardized Data/acc_front.csv")
write.csv(test_acc_front_step2_standardized, "~/Desktop/data/subject1/test/Step2/standardized Data/acc_front.csv")
write.csv(test_acc_front_step5_standardized, "~/Desktop/data/subject1/test/Step5/standardized Data/acc_front.csv")
write.csv(test_acc_front_step6_stairUp_standardized, "~/Desktop/data/subject1/test/Step6/stairUp/standardized Data/acc_front.csv")
write.csv(test_acc_front_step6_stairDown_standardized, "~/Desktop/data/subject1/test/Step6/stairDown/standardized Data/acc_front.csv")

## gyro
test_gyro_front_step1 <- read.csv("~/Desktop/data/subject1/test/Step1/gyro_front.csv")
test_gyro_front_step2 <- read.csv("~/Desktop/data/subject1/test/Step2/gyro_front.csv")
test_gyro_front_step5 <- read.csv("~/Desktop/data/subject1/test/Step5/gyro_front.csv")
test_gyro_front_step6_stairDown <- read.csv("~/Desktop/data/subject1/test/Step6/stairDown/gyro_front.csv")
test_gyro_front_step6_stairUp <- read.csv("~/Desktop/data/subject1/test/Step6/stairUp/gyro_front.csv")

test_gyro_front_step1_standardized <- standardizeData(test_gyro_front_step1, "ms")
test_gyro_front_step2_standardized <- standardizeData(test_gyro_front_step2, "ms")
test_gyro_front_step5_standardized <- standardizeData(test_gyro_front_step5, "ms")
test_gyro_front_step6_stairUp_standardized <- standardizeData(test_gyro_front_step6_stairUp, "ms")
test_gyro_front_step6_stairDown_standardized <- standardizeData(test_gyro_front_step6_stairDown, "ms")
write.csv(test_gyro_front_step1_standardized, "~/Desktop/data/subject1/test/Step1/standardized Data/gyro_front.csv")
write.csv(test_gyro_front_step2_standardized, "~/Desktop/data/subject1/test/Step2/standardized Data/gyro_front.csv")
write.csv(test_gyro_front_step5_standardized, "~/Desktop/data/subject1/test/Step5/standardized Data/gyro_front.csv")
write.csv(test_gyro_front_step6_stairUp_standardized, "~/Desktop/data/subject1/test/Step6/stairUp/standardized Data/gyro_front.csv")
write.csv(test_gyro_front_step6_stairDown_standardized, "~/Desktop/data/subject1/test/Step6/stairDown/standardized Data/gyro_front.csv")


# interpolation check
data <- test_gyro_front_step6_stairDown
standardizedData <- test_gyro_front_step6_stairDown_standardized
Min <- -4
Max <- 4

diff <- nrow(data)-nrow(standardizedData)
standardizedData$x.original <- data$x[1:(nrow(data)-diff)]
standardizedData$y.original <- data$y[1:(nrow(data)-diff)]
standardizedData$z.original <- data$z[1:(nrow(data)-diff)]
standardizedData$ori.timestamp <- data$timestamp[1:(nrow(data)-diff)]

cols <- c("x" = "#FF9990", "y" = "#00BA38", "z" = "#619CFF", "interpolated" = "#000000")
a <-10
b <-50
ggplot(standardizedData) + geom_line(data = standardizedData[a:b,], aes(x = ori.timestamp, y = x.original, color = "x")) + geom_point(data = standardizedData[a:b,], aes(x = ori.timestamp, y = x.original, color = "x"), shape = 19) + geom_point(data = standardizedData[a:b,], aes(x = timestamp, y = x, color = "interpolated"), shape = 8) + xlab("timestamp") + ylab("Angular Velocity") + ylim(Min, Max) + scale_colour_manual(name="Data",values=cols, guide = guide_legend(override.aes = list(shape = c(8,19))))
ggplot(standardizedData) + geom_line(data = standardizedData[a:b,], aes(x = ori.timestamp, y = y.original, color = "y")) + geom_point(data = standardizedData[a:b,], aes(x = ori.timestamp, y = y.original, color = "y"), shape = 19) + geom_point(data = standardizedData[a:b,], aes(x = timestamp, y = y, color = "interpolated"), shape = 8) + xlab("timestamp") + ylab("Angular Velocity") + ylim(Min, Max) + scale_colour_manual(name="Data",values=cols, guide = guide_legend(override.aes = list(shape = c(8,19))))
ggplot(standardizedData) + geom_line(data = standardizedData[a:b,], aes(x = ori.timestamp, y = z.original, color = "z")) + geom_point(data = standardizedData[a:b,], aes(x = ori.timestamp, y = z.original, color = "z"), shape = 19) + geom_point(data = standardizedData[a:b,], aes(x = timestamp, y = z, color = "interpolated"), shape = 8) + xlab("timestamp") + ylab("Angular Velocity") + ylim(Min, Max) + scale_colour_manual(name="Data",values=cols, guide = guide_legend(override.aes = list(shape = c(8,19))))
