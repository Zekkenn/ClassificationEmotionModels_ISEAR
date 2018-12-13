#SVM classificationModel

library(psych)
library(kernlab)
# library(R.matlab)
library(gmodels)
library(caret)
library(e1071)
# library(RTextTools)

# ======== Roc values of Train and test, respectively ============== #
# ====================== TRAIN =============================== #
# $ AUC        :List of 1
# ..$ SVM_Test:List of 7
# .. ..$ joy     : num 1
# .. ..$ fear    : num 1
# .. ..$ anger   : num 0.967
# .. ..$ sadness : num 0.891
# .. ..$ disgust : num 0.993
# .. ..$ macro   : num 0.97
# .. ..$ micro   : num 0.975
# ===================== TEST ============================#
# $ AUC        :List of 1
# ..$ SVM_Test:List of 7
# .. ..$ joy     : num 0.449
# .. ..$ fear    : num 0.51
# .. ..$ anger   : num 0.518
# .. ..$ sadness : num 0.483
# .. ..$ disgust : num 0.519
# .. ..$ macro   : num 0.496
# .. ..$ micro   : num 0.492
# ===================== F1 ===============================#
#     Class: joy    Class: fear   Class: anger Class: sadness Class: disgust 
#     0.8854735      0.9390642      0.7883598      0.6308345      0.8443554

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
  type = "svmLinear"
  ctrl <- trainControl(method = "cv", classProbs=TRUE, savePred=T)
  grid <- expand.grid(C = c(0.1,0.25,0.5,0.75,1))
  data.svm <- as.data.frame( as.matrix(x) )
  data.svm$labels_model <- y
  
  fit <- train(
    labels_model ~., data = data.svm, method = type,
    trControl = ctrl,
    tuneGrid = grid)
  
  return(fit)
}

# y is a list with the labels of the dataset, example:
# y <- list("joy" = "X1", "fear" = "X2", "anger" = "X3", "sadness" = "X4", "disgust" = "X5")
predict.svm <- function(modelSVM, data.svm, y){
  words.dict <- sort(colnames(modelSVM$trainingData))[-1]
  words.dict <- as.vector(words.dict)
  x.rep.train <- DocumentTermMatrix(Corpus(VectorSource(data.svm)), list( dictionary = words.dict ))
  x.rep.train <- as.data.frame(as.matrix(x.rep.train))
  predSVM <- predict(modelSVM, x.rep.train)
  levels(predSVM) <- y
  return(predSVM)
}

predict.svm.prob <- function(modelSVM, data.svm, y) {
  words.dict <- sort(colnames(modelSVM$trainingData))[-1]
  words.dict <- as.vector(words.dict)
  x.rep.train <- DocumentTermMatrix(Corpus(VectorSource(data.svm)), list( dictionary = words.dict ))
  x.rep.train <- as.data.frame(as.matrix(x.rep.train))
  x.rep.train[ is.na(x.rep.train) ] <- 0
  predSVM <- predict(modelSVM, x.rep.train, type = "prob")
  levels(predSVM) <- y
  return(predSVM)
}