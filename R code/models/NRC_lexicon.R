# NRC EMOTION LEXICON

# NECESSARY LIBRARIES

library(syuzhet)
library(ggplot2)
library(mldr)

#source('~/ClassificationEmotionModels_ISEAR/R code/data_exploration.R')
# ============================== ROCR METRICS ===================
# $ AUC        :List of 1
# ..$ SVM_Test:List of 7
# .. ..$ joy     : num 0.665
# .. ..$ fear    : num 0.667
# .. ..$ anger   : num 0.63
# .. ..$ sadness : num 0.593
# .. ..$ disgust : num 0.572
# .. ..$ macro   : num 0.625
# .. ..$ micro   : num 0.615


# NRC EMOTION ANALYSIS

nrc_analysis <- function(x,y){
  
  data <- data.frame(SIT = x, EMOT = y, stringsAsFactors = FALSE)
  data$EMOT <- factor(data$EMOT)
  
  # TEST NRC MODEL 
  nrc_data <- get_nrc_sentiment(data$SIT)
  nrc_data <- nrc_data[,levels(data$EMOT)]
  # nrc_data$other <- rowMeans(nrc_data[, -levels(data$EMOT)])

  # PREDICTION - BASED ON HIGHEST PROBABILITY
  nrc_prob <- prop.table(as.matrix(nrc_data),1)
  predict_emot <- data.frame( EMOT = colnames(nrc_prob)[max.col(nrc_prob,ties.method="first")])
  
  # CONFUSION MATRIX
  cm <- confusionMatrix(predict_emot$EMOT,data$EMOT, mode = "everything")
  
  modelNRC <- list( lexicon = nrc_data, train.prediction = predict_emot, train.result = cm)
  
  # PLOTS
  #visualize_data(predict_emot$EMOT)
  #nrc_plots(nrc_data,cm)
  return(modelNRC)
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

predict.lex <- function(data){
  pred <- get_nrc_sentiment(data$SIT)
  pred$other <- 0
  pred <- pred[getLevels()]
  nrc_prob <- prop.table(as.matrix(pred),1)
  predict_emot <- colnames(nrc_prob)[max.col(nrc_prob,ties.method="first")]
  predict_emot[is.na(predict_emot)] <- "other"
  predict_emot <- factor(predict_emot)
  levels(predict_emot) <- getLevels()
  return(predict_emot)
}

predict.lex.prob <- function(data){
  pred <- get_nrc_sentiment(data$SIT)
  pred$other <- 0
  pred <- pred[getLevels()]
  nrc_prob <- prop.table(as.matrix(pred),1)
  nrc_prob[ is.na(nrc_prob) ] <- 0
  return(nrc_prob)
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