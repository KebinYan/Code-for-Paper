library(ggplot2)
library(tidyverse)
library(caret)
plotConfusionMatrix <- function(result, Activities, Legend){
  have = FALSE
  for(i in 1:length(Activities)){
    for(j in 1:length(levels(result$label))){
      if(levels(result$label)[j]==Activities[i]){
        have = TRUE
      }
    }
    if(have == FALSE){
      levels(result$label) <- c(levels(result$label), Activities[i])
    }
    have = FALSE
  }
  
  have = FALSE
  for(i in 1:length(Activities)){
    for(j in 1:length(levels(result$label.predict))){
      if(levels(result$label.predict)[j]==Activities[i]){
        have = TRUE
      }
    }
    if(have == FALSE){
      levels(result$label.predict) <- c(levels(result$label.predict), result$label[i])
    }
    have = FALSE
  }
  
  confusionMat<- confusionMatrix(result$label.predict, result$label)$table
  for(i in 1:nrow(confusionMat)){
    for(j in 1:ncol(confusionMat)){
      confusionMat[i,j] <- round(confusionMat[i,j]/margin.table(confusionMatrix(result$label.predict, result$label)$table,2)[j],4)*100
    }
  }
  confusionMat <- as.data.frame.matrix(confusionMat)
  confusionMat <- confusionMat %>% rownames_to_column() %>% gather(colname, percentage, -rowname)
  confusionMat <- na.omit(confusionMat)
  if(!Legend){
    ggplot(confusionMat, aes(x = colname, y= rowname, fill=percentage)) + geom_tile() + theme_bw() + coord_equal() + scale_fill_distiller(palette="Blues", direction=1, limits = range(0:100)) + geom_text(aes(label=percentage), color="black", size = 4.5) + xlab("Truth") + ylab("Prediction") + theme(axis.text.x = element_text(angle=15, size = 13, vjust = 0.5), axis.text.y = element_text(size = 13), text = element_text(size = 13), legend.position = "none")
  }
  else{
    ggplot(confusionMat, aes(x = colname, y= rowname, fill=percentage)) + geom_tile() + theme_bw() + coord_equal() + scale_fill_distiller(palette="Blues", direction=1, limits = range(0:100)) + geom_text(aes(label=percentage), color="black", size = 4.5) + xlab("Truth") + ylab("Prediction") + theme(axis.text.x = element_text(angle=15, size = 13, vjust = 0.5), axis.text.y = element_text(size = 13), text = element_text(size = 13), legend.position = "right", legend.title = element_text(size = 13))
  }
}
