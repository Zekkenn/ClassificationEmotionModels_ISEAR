# Neural Network Classification Model

library(neuralnet)
library(nnet)
library(ggplot2)

# ------------------------------------------------------------------------------------------
# --------------------------------- Global Variables ---------------------------------------
# ------------------------------------------------------------------------------------------
ecuation <- NULL

setEcuation <- function(ec){
  ecuation <<- ec
}

getEcuation_8 <- function(n){
  f <- as.formula(paste("l1 + l2 + l3 + l4 + l5 + l6 + l7 + l8 ~ ", 
                        paste(n[!n %in% c("`l1`","`l2`","`l3`", "`l4`","`l5`","`l6`", "`l7`", "`l8`")], 
                              collapse = " + ")))
  return(f)
}

getEcuation_5 <- function(n){
  f <- as.formula(paste("l1 + l2 + l3 + l4 + l5 ~ ", 
                        paste(n[!n %in% c("`l1`","`l2`","`l3`", "`l4`","`l5`")], 
                              collapse = " + ")))
  return(f)
}

getEcuation_labels <- function(n){
  lbs <- c("l1","l2","l3", "l4","l5", "l6", "l7", "l8", "l9", "l10", "l11", "l12", "l13", "l14", "l15", "l16")
  return( lbs[1:n] )
}

train.nn <- function(x, y){
  
  data <- as.data.frame(as.matrix(x))
  data$labels_model <- factor(y)
  train <- pre_proc(data, length(levels(data$labels_model) ))
  
  n <- sprintf("`%s`", names(train))
  f <- ecuation(n)
  
  nn <- neuralnet(f, data = train, hidden = c(100), act.fct = "logistic", linear.output = FALSE, lifesign = "minimal")
  
}

pre_proc <- function(data, n){
  size <- (dim(data)[2]-1)
  data <- cbind(data[, 1:size], class.ind(as.factor(data$labels_model)))
  names(data) <- c(names(data)[ 1:size ], getEcuation_labels(n))
  return(data)
}

predict.nn <- function(modelNN, data, y){
  predNN <- predict.nn.prob(modelNN, data)
  predNN <- max.col(predNN)
  predNN <- factor( predNN )
  levels(predNN) <- y
  return(predNN)
}

predict.nn.prob <- function(modelNN, data){
  size <- dim(data)[2]-1
  test <- data[,1:size]
  pred_with <- compute( modelNN, test )
  return(pred_with$net.result)
}

# ====== TEST ========
# > test <- data[[2]]
# > size <- dim(test)[2]-1
# > test <- test[,1:size]
# > pred_with <- compute( model_nn_without, test )
# > pred_with_nn_factor <- max.col(pred_with$net.result)
# > pred_with_nn_factor <- factor( pred_with_nn_factor )
# > confusionMatrix( pred_with_nn_factor, data_new[[2]]$labels_model )