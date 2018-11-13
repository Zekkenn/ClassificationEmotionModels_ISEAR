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

train.svm <- function(data.train, type = "svmLinear", lvls = list("joy" = "X1", "fear" = "X2", "anger" = "X3", "sadness" = "X4", "disgust" = "X5", "shame" = "X6", "guilt" = "X7")){
  ctrl <- trainControl(method = "cv", classProbs = TRUE)
  grid <- expand.grid(C = c(0.05, 0.1, 0.25, 0.5, 0.75))
  levels(data.train$labels_model) <- lvls
  svm.model <- train( labels_model ~ ., data = data.train, method = type, trControl = ctrl, tuneGrid = grid )
  return(svm.model)
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