library(ggplot2)
library(tidyverse)
library(caret)
plotConfusionMatrix <- function(result, plotTitle){
  have = FALSE
  for(i in 1:length(levels(result$label.predict))){
    for(j in 1:length(levels(result$label))){
      if(levels(result$label.predict)[i]==levels(result$label)[j]){
        have = TRUE
      }
    }
    if(have == FALSE){
      levels(result$label) <- c(levels(result$label), levels(result$label.predict[i]))
    }
    have = FALSE
  }
  
  have = FALSE
  for(i in 1:length(levels(result$label))){
    for(j in 1:length(levels(result$label.predict))){
      if(levels(result$label)[i]==levels(result$label.predict)[j]){
        have = TRUE
      }
    }
    if(have == FALSE){
      levels(result$label.predict) <- c(levels(result$label.predict), levels(result$label[i]))
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
  ggplot(confusionMat, aes(x = colname, y= rowname, fill=percentage)) + geom_tile() + theme_bw() + coord_equal() + scale_fill_distiller(palette="Blues", direction=1) + labs(title = plotTitle) + geom_text(aes(label=percentage), color="black") + xlab("Truth") + ylab("Prediction")
}

