
setwd("C:/Users/ASUS/Documents/Sebastian/ClassificationEmotionModels_ISEAR")

source("R code/data_loader.R")
source("R code/models/NN.R")
source("R code/models/SVM.R")
source("R code/models/NaiveBayes.R")

dataSem <- getData.SemEval(path = "SemEval_14/AffectiveText.test")
dataIsear <- getData.ISEAR(path = "py_isear_dataset/isear.csv")

dataIsear <- dataIsear[ !(dataIsear$EMOT %in% c("guilt", "shame")), ]

bagSem <- bag.of.words(dataSem)
bagIsear <- bag.of.words(dataIsear)

#--------Train------------
modelBayes <- train.naiveBayes(bagIsear)
modelSVM <- train.svm(bagIsear)
modelNN <- train.nn(bagIsear)
