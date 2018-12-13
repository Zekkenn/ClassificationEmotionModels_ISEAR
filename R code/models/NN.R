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
  
  data.nn <- as.data.frame(as.matrix(x))
  data.nn$labels_model <- factor(y)
  
  ctrl <- trainControl(method = "cv", classProbs=TRUE)
  grid <- expand.grid(.decay = c(0.5, 0.1) )
  nn <- train(
    labels_model ~., data = data.nn, method = "nnet",
    trControl = ctrl, linout = 0, MaxNWts=20100, maxit=500 )
  
  # nn <- nnet(labels_model~., data=data, size=1, maxit=1000, MaxNWts = 15000)
  
  # train <- pre_proc(data, length(levels(data$labels_model) ))
  # 
  # n <- sprintf("`%s`", names(train))
  # f <- ecuation(n)
  # 
  # nn <- neuralnet(f, data = train, hidden = c(100), act.fct = "logistic", linear.output = FALSE, lifesign = "minimal")
  # 
  return(nn)
}

pre_proc <- function(data, n){
  size <- (dim(data)[2]-1)
  data <- cbind(data[, 1:size], class.ind(as.factor(data$labels_model)))
  names(data) <- c(names(data)[ 1:size ], getEcuation_labels(n))
  return(data)
}

getData.nn <- function(modelNN, data.nn){
  words.dict <- attr(modelNN$terms, "term.labels")
  data.nn <- DocumentTermMatrix(Corpus(VectorSource(data.nn)), list(dictionary = as.vector(words.dict)))
  return(data)
}

predict.nn <- function(modelNN, data.nn, y){
  predNN <- predict.nn.prob(modelNN, data.nn)
  predNN <- max.col(predNN)
  predNN <- factor( predNN )
  levels(predNN) <- y
  return(predNN)
}

predict.nn.prob <- function(modelNN, data.nn){
  data.nn <- getData.nn(modelNN, data.nn)
  pred_with <- predict( modelNN, data.nn )
  return(pred_with)
}

# ====== TEST ========
# > test <- data[[2]]
# > size <- dim(test)[2]-1
# > test <- test[,1:size]
# > pred_with <- compute( model_nn_without, test )
# > pred_with_nn_factor <- max.col(pred_with$net.result)
# > pred_with_nn_factor <- factor( pred_with_nn_factor )
# > confusionMatrix( pred_with_nn_factor, data_new[[2]]$labels_model )