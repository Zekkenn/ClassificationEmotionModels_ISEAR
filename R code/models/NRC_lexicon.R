# NRC EMOTION LEXICON

# NECESSARY LIBRARIES

library(syuzhet)
library(ggplot2)
library(mldr)

#source('~/ClassificationEmotionModels_ISEAR/R code/data_exploration.R')

# NRC EMOTION ANALYSIS

nrc_analysis <- function(data){
  
  # TEST NRC MODEL 
  nrc_data <- get_nrc_sentiment(data$SIT)
  nrc_data$other <- rowMeans(nrc_data[, c(2,7,8)])
  nrc_data <- nrc_data[,c(1,3,4,5,6)]
  str(nrc_data)
  # PREDICTION - BASED ON HIGHEST PROBABILITY
  nrc_prob <- prop.table(as.matrix(nrc_data),1)
  predict_emot <- data.frame( EMOT = colnames(nrc_prob)[max.col(nrc_prob,ties.method="first")])
  levels(predict_emot$EMOT)
  
  # CONFUSION MATRIX
  data$EMOT <- factor(data$EMOT, levels = c("joy","fear","anger","sadness","disgust")) # RELATED LABELS WITH NRC
  cm <- confusionMatrix(predict_emot$EMOT,data$EMOT, mode = "everything")
  
  # PLOTS
  #visualize_data(predict_emot$EMOT)
  #nrc_plots(nrc_data,cm)
  return(cm)
}

nrc_plots <- function(nrc_data, conf.matrix){
  
  # GENERAL - EMOTIONAL WORDS FOUNDED - COUNT
  td<-data.frame(t(nrc_data))
  td_new <- data.frame(rowSums(td))
  names(td_new)[1] <- "count"
  td_new <- cbind("emotion" = rownames(td_new), td_new)
  rownames(td_new) <- NULL
  p <- qplot(emotion, data=td_new, weight=count, geom="bar",fill=emotion)+ggtitle("ISEAR Emotions words detected")
  print(p)
  
  # GENERAL - EMOTIONAL WORDS FOUNDED - PERCENTAGE
  barplot(
    sort(colSums(prop.table(nrc_data))), 
    horiz = TRUE, 
    cex.names = 0.7, 
    las = 1, 
    main = "Emotions Percentage in ISEAR", xlab="Percentage"
  )
  
  # Confusion Matrix
  plot(conf.matrix$table, main = "Confusion Matrix")
  conf.matrix
}

predict.lex <- function(modelLex, data){
  pred <- "lol"
  return(pred)
}

# ISEAR LABELS TRANSFORMATION

# FILTER BY : CHANGE SHAME & GUILT LABELS TO OTHER  
isear_filter_other <- function(data){
  levels(data$EMOT) <- c(levels(data$EMOT), "other")
  data$EMOT[data$EMOT %in% c("shame","guilt")] <- "other"
  return(data)
}

# FILTER BY : REMOVE ROWS WITH SHAME & GUILT DATA
isear_filter_without <- function(data){
  return(data.filter <- subset(data, subset = EMOT %in% c("joy","fear","anger","sadness","disgust")))
}