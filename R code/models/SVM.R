#SVM classificationModel

library(psych)
library(kernlab)
# library(R.matlab)
library(gmodels)
library(caret)
library(e1071)
# library(RTextTools)

# ================================================================== #
# == Uncomment when you need - Be careful with the number of cores== #

# Parallel Processing Linux
# library(doMC) only avalible on linux
# registerDoMC(cores=4)

# Parallel Processing Windows 
# library(snow)
# library(doParallel)
# workers=makeCluster(4,type="SOCK")
# registerDoParallel(workers)

train.svm <- function(x, y){
  type = "svmRadial"
  ctrl <- trainControl(method = "cv", number = 10, classProbs=TRUE)
  grid <- expand.grid(sigma = c(.01, .015, 0.2),
                      C = c(0.05,0.1,0.25,0.5,0.75,0.9,1))
  
  fit <- train(
    x = as.data.frame(as.matrix(x)), y = y, method = type,
    trControl = ctrl,
    tuneGrid = grid)
  
  return(fit)
}

# y is a list with the labels of the dataset, example:
# y <- list("joy" = "X1", "fear" = "X2", "anger" = "X3", "sadness" = "X4", "disgust" = "X5")
predict.svm <- function(modelSVM, data, y){
  predSVM <- predict(modelSVM, data)
  levels(predSVM) <- y
  return(predSVM)
}

predict.svm.prob <- function(modelSVM, data) {
  predSVM <- predict(modelSVM, data, type = "prob")
  predSVM <- as.matrix(predSVM)
  return(predSVM)
}