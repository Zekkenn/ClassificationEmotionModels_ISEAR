#SVM classificationModel

library(psych)
library(kernlab)
library(R.matlab)
library(gmodels)
library(ROCR)
library(caret)
library(e1071)
library(RTextTools)

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

train.svm <- function(data.train){
  ctrl <- trainControl(method = "cv", number = 6)
  svm_linear <- train( labels_model ~ ., data = data[[1]], method = "svmLinear", trControl = ctrl, tuneLength = 6 )
}