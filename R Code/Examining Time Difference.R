options(digits = 12)
# load data
test_gyro_front_step1 <- read.csv("~/Desktop/data/subject1/test/Step1/gyro_front.csv")
test_acc_front_step1 <- read.csv("~/Desktop/data/subject1/test/Step1/acc_front.csv")

test_gyro_front_step2 <- read.csv("~/Desktop/data/subject1/test/Step1/gyro_front.csv")
test_acc_front_step2 <- read.csv("~/Desktop/data/subject1/test/Step1/acc_front.csv")

time_difference <- as.data.frame(cbind(test_gyro_front_step1$timestamp, rep(NA,nrow(test_gyro_front_step1)), rep(NA,nrow(test_gyro_front_step1))))
colnames(time_difference) <- c("gyro_timestamp","acc_timestamp","time_difference")

# calculate time difference between gyroscope timestamp and the closest accelerometer timestamp
for (i in 1:nrow(test_gyro_front_step1)){
  temp_diff <- NA
  temp_diff <- as.data.frame(rep(NA, nrow(test_acc_front_step1)))
  for(j in 1:nrow(test_acc_front_step1)){
    temp_diff[j,1] <- abs(test_gyro_front_step1$timestamp[i]-test_acc_front_step1$timestamp[j])
  }
  time_difference[i,3] <- min(temp_diff)
  time_difference[i,2] <- test_acc_front_step1$timestamp[which.min(unlist(temp_diff))]
}

plot(time_difference$gyro_timestamp[1:20], type = "p", pch = 1, col = "blue")
points(time_difference$acc_timestamp[1:20], col = "red", pch = 1)
legend("bottomright" , legend = c("accelerometer", "gyroscope") , pch = 1, col = c("red","blue"))

plot(time_difference$gyro_timestamp[(nrow(time_difference)-20):nrow(time_difference)], type = "p", pch = 1, col = "blue")
points(time_difference$acc_timestamp[(nrow(time_difference)-20):nrow(time_difference)], col = "red", pch = 1)
legend("bottomright" , legend = c("accelerometer", "gyroscope") , pch = 1, col = c("red","blue"))

plot(x = seq(1:nrow(time_difference)), y = time_difference$time_difference, ylab = "time difference")
axis(2, at=time_difference$time_difference,labels=time_difference$time_difference)

# plot the first and last 50 data points
ggplot(time_difference[1:50,]) + geom_point(aes(x=seq(1:50), y=gyro_timestamp), color = "blue") +  geom_point(aes(x=seq(1:50), acc_timestamp, color = "red")) + theme(legend.position = "none")
ggplot(time_difference[(nrow(time_difference)-49):nrow(time_difference),]) + geom_point(aes(x=seq(1:50), y=gyro_timestamp), col = "blue") +  geom_point(aes(x=seq(1:50), acc_timestamp, col = "red")) + theme(legend.position = "none")

# plot the time difference for all data points
ggplot(time_difference, aes(x = seq(1:1582), y = time_difference)) + geom_point() +xlab("")


#same code for step 2

time_difference2 <- as.data.frame(cbind(test_gyro_front_step2$timestamp, rep(NA,nrow(test_gyro_front_step2)), rep(NA,nrow(test_gyro_front_step2))))
colnames(time_difference2) <- c("gyro_timestamp","acc_timestamp","time_difference")

for (i in 1:nrow(test_gyro_front_step2)){
  temp_diff <- NA
  temp_diff <- as.data.frame(rep(NA, nrow(test_acc_front_step2)))
  for(j in 1:nrow(test_acc_front_step2)){
    temp_diff[j,1] <- abs(test_gyro_front_step2$timestamp[i]-test_acc_front_step2$timestamp[j])
  }
  time_difference2[i,3] <- min(temp_diff)
  time_difference2[i,2] <- test_acc_front_step2$timestamp[which.min(unlist(temp_diff))]
}

plot(time_difference2$gyro_timestamp[1:100], type = "p", pch = 1, col = "blue")
points(time_difference2$acc_timestamp[1:100], col = "red", pch = 1)
legend("bottomright" , legend = c("accelerometer", "gyroscope") , pch = 1, col = c("red","blue"))

plot(time_difference2$gyro_timestamp[(nrow(time_difference2)-20):nrow(time_difference2)], type = "p", pch = 1, col = "blue")
points(time_difference2$acc_timestamp[(nrow(time_difference2)-20):nrow(time_difference2)], col = "red", pch = 1)
legend("bottomright" , legend = c("accelerometer", "gyroscope") , pch = 1, col = c("red","blue"))

plot(x = seq(1:nrow(time_difference2)), y = time_difference2$time_difference)

ggplot(time_difference2[1:50,]) + geom_point(aes(x=seq(1:50), y=gyro_timestamp), color = "blue") +  geom_point(aes(x=seq(1:50), acc_timestamp, color = "red")) + theme(legend.position = "none")
ggplot(time_difference2[(nrow(time_difference)-49):nrow(time_difference),]) + geom_point(aes(x=seq(1:50), y=gyro_timestamp), col = "blue") +  geom_point(aes(x=seq(1:50), acc_timestamp, col = "red")) + theme(legend.position = "none")
ggplot(time_difference2, aes(x = seq(1:1582), y = time_difference)) + geom_point() +xlab("")

