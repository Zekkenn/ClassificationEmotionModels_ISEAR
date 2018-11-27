# Naive Bayes Classification Model

library(e1071)
library(multiROC)
library(dummies)
library(bnlearn)

train.naiveBayes <- function( data, emot ){
  data.train <- apply(data, 2, convert_count)
  
  search_grid <- expand.grid(
    usekernel = c(FALSE),
    fL = 0:1,
    adjust = seq(0,5,by = 1))
  
  train_control <- trainControl(
    method = "cv", 
    number = 10)
  
  fit <- train(
    x = as.data.frame(data.train), y = emot, method = "nb",
    trControl = train_control,
    tuneGrid = search_grid)
  return(fit)
}


plot.roc <- function(true_label, pred){
  true_label <- dummies::dummy(true_label)
  true_label <- data.frame(true_label)
  colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
  colnames(true_label) <- c("joy", "fear", "anger", "sadness", "disgust", "shame", "guilt")
  colnames(true_label) <- paste(colnames(true_label), "_true")
  
  pred <- data.frame(pred)
  colnames(pred) <- c("joy", "fear", "anger", "sadness", "disgust", "shame", "guilt")
  colnames(pred) <- paste(colnames(pred), "_pred_NeuralNet")
  final_df <- cbind(true_label, pred)
  roc_res <- multi_roc(final_df, force_diag=T)
  
  plot_roc_df <- plot_roc_data(roc_res)
  
  ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
    geom_path(aes(color = Group, linetype=Method), size=1) +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
                 colour='grey', linetype = 'dotdash') +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5), 
          legend.justification=c(1, 0), legend.position=c(.95, .05),
          legend.title=element_blank(), 
          legend.background = element_rect(fill=NULL, size=0.5, 
                                           linetype="solid", colour ="black"))
  
  
}

predict.bayes <- function(modelBayes, data, y){
  predBayes <- predict(modelBayes, data)
  levels(predBayes) <- y
  return(predBayes)
}

predict.bayes.prob <- function(modelBayes, data){
  predBayes <- predict(modelBayes, data, type="raw")
  return(predBayes)
}


# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}