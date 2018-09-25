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

train.svm <- function(data.train, type = "svmLinear"){
  ctrl <- trainControl(method = "cv", number = 6)
  grid <- expand.grid(C = c(0.5))
  svm.model <- train( labels_model ~ ., data = data.train, method = type, trControl = ctrl, tuneGrid = grid )
  return(svm.model)
}