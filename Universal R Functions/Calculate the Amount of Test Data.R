# load data
# subject 1
sub1_step1 <- read.csv("~/Desktop/csv/subject1/test/Prediction/Step1/revised_acc_front_pred.csv")
sub1_step2 <- read.csv("~/Desktop/csv/subject1/test/Prediction/Step2/revised_acc_front_pred.csv")
sub1_step3_fastWalk <- read.csv("~/Desktop/csv/subject1/test/Prediction/Step3/fast_walk/revised_acc_front_pred_fast.csv")
sub1_step3_normalWalk <- read.csv("~/Desktop/csv/subject1/test/Prediction/Step3/normal_walk/revised_acc_front_pred_normal.csv")
sub1_step3_slowWalk <- read.csv("~/Desktop/csv/subject1/test/Prediction/Step3/slow_walk/revised_acc_front_pred_slow.csv")
sub1_step5 <- read.csv("~/Desktop/csv/subject1/test/Prediction/Step5/revised_acc_front_pred.csv")
sub1_step6_stairDown <- read.csv("~/Desktop/csv/subject1/test/Prediction/Step6/stairDown/revised_acc_front_pred.csv")
sub1_step6_stairUp <- read.csv("~/Desktop/csv/subject1/test/Prediction/Step6/stairUp/revised_acc_front_pred.csv")

# subject 2
sub2_step1 <- read.csv("~/Desktop/csv/subject2/test/Prediction/Step1/revised_acc_front_pred.csv")
sub2_step2 <- read.csv("~/Desktop/csv/subject2/test/Prediction/Step2/revised_acc_front_pred.csv")
sub2_step3_fastWalk <- read.csv("~/Desktop/csv/subject2/test/Prediction/Step3/fast_walk/revised_acc_front_pred_fast.csv")
sub2_step3_normalWalk <- read.csv("~/Desktop/csv/subject2/test/Prediction/Step3/normal_walk/revised_acc_front_pred_normal.csv")
sub2_step3_slowWalk <- read.csv("~/Desktop/csv/subject2/test/Prediction/Step3/slow_walk/revised_acc_front_pred_slow.csv")
sub2_step5 <- read.csv("~/Desktop/csv/subject2/test/Prediction/Step5/revised_acc_front_pred.csv")
sub2_step6_stairDown <- read.csv("~/Desktop/csv/subject2/test/Prediction/Step6/stairDown/revised_acc_front_pred.csv")
sub2_step6_stairUp <- read.csv("~/Desktop/csv/subject2/test/Prediction/Step6/stairUp/revised_acc_front_pred.csv")

# subject 3
sub3_step1 <- read.csv("~/Desktop/csv/subject3/test/Prediction/Step1/revised_acc_front_pred.csv")
sub3_step2 <- read.csv("~/Desktop/csv/subject3/test/Prediction/Step2/revised_acc_front_pred.csv")
sub3_step3_fastWalk <- read.csv("~/Desktop/csv/subject3/test/Prediction/Step3/fast_walk/revised_acc_front_pred_fast.csv")
sub3_step3_normalWalk <- read.csv("~/Desktop/csv/subject3/test/Prediction/Step3/normal_walk/revised_acc_front_pred_normal.csv")
sub3_step3_slowWalk <- read.csv("~/Desktop/csv/subject3/test/Prediction/Step3/slow_walk/revised_acc_front_pred_slow.csv")
sub3_step5 <- read.csv("~/Desktop/csv/subject3/test/Prediction/Step5/revised_acc_front_pred.csv")
sub3_step6_stairDown <- read.csv("~/Desktop/csv/subject3/test/Prediction/Step6/stairDown/revised_acc_front_pred.csv")
sub3_step6_stairUp <- read.csv("~/Desktop/csv/subject3/test/Prediction/Step6/stairUp/revised_acc_front_pred.csv")

# subject 4
sub4_step1 <- read.csv("~/Desktop/csv/subject4/test/Prediction/Step1/revised_acc_front_pred.csv")
sub4_step2 <- read.csv("~/Desktop/csv/subject4/test/Prediction/Step2/revised_acc_front_pred.csv")
sub4_step3_fastWalk <- read.csv("~/Desktop/csv/subject4/test/Prediction/Step3/fast_walk/revised_acc_front_pred_fast.csv")
sub4_step3_normalWalk <- read.csv("~/Desktop/csv/subject4/test/Prediction/Step3/normal_walk/revised_acc_front_pred_normal.csv")
sub4_step3_slowWalk <- read.csv("~/Desktop/csv/subject4/test/Prediction/Step3/slow_walk/revised_acc_front_pred_slow.csv")
sub4_step5 <- read.csv("~/Desktop/csv/subject4/test/Prediction/Step5/revised_acc_front_pred.csv")
sub4_step6_stairDown <- read.csv("~/Desktop/csv/subject4/test/Prediction/Step6/stairDown/revised_acc_front_pred.csv")
sub4_step6_stairUp <- read.csv("~/Desktop/csv/subject4/test/Prediction/Step6/stairUp/revised_acc_front_pred.csv")

getData <- function(step1, 
                    step2, 
                    step3_fastWalk, 
                    step3_normalWalk, 
                    step3_slowWalk, 
                    step5, 
                    step6_stairUp, 
                    step6_stairDown){
  step1$label <- as.factor(step1$label)
  step2$label <- as.factor(step2$label)
  step3 <- rbind(step3_fastWalk, step3_normalWalk, step3_slowWalk)
  step3$label <- as.factor(step3$label)
  step5$label <- as.factor(step5$label)
  step6 <- rbind(step6_stairUp, step6_stairDown)
  step6$label <- as.factor(step6$label)
  summary <- rbind(step1, step2, step3, step5, step6)
  return(summary)
}

sub1_data <- getData(sub1_step1, 
                     sub1_step2, 
                     sub1_step3_fastWalk, 
                     sub1_step3_normalWalk, 
                     sub1_step3_slowWalk, 
                     sub1_step5, 
                     sub1_step6_stairUp, 
                     sub1_step6_stairDown)
sub2_data <- getData(sub2_step1, 
                     sub2_step2, 
                     sub2_step3_fastWalk, 
                     sub2_step3_normalWalk, 
                     sub2_step3_slowWalk, 
                     sub2_step5, 
                     sub2_step6_stairUp, 
                     sub2_step6_stairDown)
sub3_data <- getData(sub3_step1, 
                     sub3_step2, 
                     sub3_step3_fastWalk, 
                     sub3_step3_normalWalk, 
                     sub3_step3_slowWalk, 
                     sub3_step5, 
                     sub3_step6_stairUp, 
                     sub3_step6_stairDown)
sub4_data <- getData(sub4_step1, 
                     sub4_step2, 
                     sub4_step3_fastWalk, 
                     sub4_step3_normalWalk, 
                     sub4_step3_slowWalk, 
                     sub4_step5, 
                     sub4_step6_stairUp, 
                     sub4_step6_stairDown)

summ_table <- cbind("sub1" = summary(sub1_data$label), 
                    "sub2" = summary(sub2_data$label),
                    "sub3" = summary(sub3_data$label),
                    "sub4" = summary(sub4_data$label))
