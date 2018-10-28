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

train.svm <- function(data.train, type = "svmLinear", lvls = c("X1", "X2", "X3", "X4", "X5")){
  ctrl <- trainControl(method = "cv", classProbs = TRUE)
  grid <- expand.grid(C = c(0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5))
  levels(data.train$labels_model) <- lvls
  svm.model <- train( labels_model ~ ., data = data.train, method = type, trControl = ctrl, tuneGrid = grid )
  return(svm.model)
}
