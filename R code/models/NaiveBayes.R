# Naive Bayes Classification Model

library(e1071)
library(multiROC)

train.naiveBayes <- function( data ){
  return( naiveBayes( labels_model ~ .,  data = data ) )
}

predict.naiveBayes <- function( model, data ){
  return( predict( model, data, type = 'raw' ) )
}

#
plot.roc <- function(true_label, pred){
  true_label <- dummies::dummy(true_label)
  true_label <- data.frame(true_label)
  colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
  colnames(true_label) <- c("joy", "fear", "anger", "sadness", "disgust", "shame", "guilt")
  colnames(true_label) <- paste(colnames(true_label), "_true")
  
  pred <- data.frame(pred)
  colnames(pred) <- c("joy", "fear", "anger", "sadness", "disgust", "shame", "guilt")
  colnames(pred) <- paste(colnames(pred), "_pred_NaiveBayes")
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