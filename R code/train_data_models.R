
#setwd("C:/Users/ASUS/Documents/Sebastian/ClassificationEmotionModels_ISEAR")

source("R code/data_loader.R")
source("R code/models/NN.R")
source("R code/models/SVM.R")
source("R code/models/NaiveBayes.R")
source("R code/models/NRC_lexicon.R")

dataSem <- getData.SemEval(path = "SemEval_14/AffectiveText.test")
dataIsear <- getData.ISEAR(path = "py_isear_dataset/isear.csv")

dataIsear <- dataIsear[ !(dataIsear$EMOT %in% c("guilt", "shame")), ]
levels(dataIsear$EMOT) <- list("joy" = "1", "fear" = "2", "anger" = "3", "sadness" = "4", "disgust" = "5")

bagSem <- bag.of.words(dataSem)
bagIsear <- bag.of.words(dataIsear)

bagSem <- bagSem[ !is.na(bagSem$labels_model), ]

#--------Train------------
modelBayes <- train.naiveBayes(bagIsear)
modelSVM <- train.svm(bagIsear)
modelNN <- train.nn(bagIsear)

#--------Save Models--------
saveRDS(modelBayes, file = "R code/models.save/modelBayes.rds")
saveRDS(modelSVM, file = "R code/models.save/modelSVM.rds")
saveRDS(modelNN, file = "R code/models.save/modelNN.rds")

#--------Load Models--------
modelSVM <- readRDS("R code/models.save/modelSVM.rds")
modelBayes <- readRDS("R code/models.save/modelBayes.rds")
modelNN <- readRDS("R code/models.save/modelNN.rds")

#-------Composition---------
predSVM <- predict(modelSVM, bagSem)
levels(predSVM) <- list("joy" = "X1", "fear" = "X2", "anger" = "X3", "sadness" = "X4", "disgust" = "X5")
predBayes <- predict(modelBayes, bagSem)

size <- dim(bagSem)[2]-1
test <- bagSem[,1:size]
pred_with <- compute( modelNN, test )
predNN <- max.col(pred_with$net.result)
predNN <- factor( predNN )
levels(predNN) <- list("joy" = "1", "fear" = "2", "anger" = "3", "sadness" = "4", "disgust" = "5")
confusionMatrix( predNN, bagSem$labels_model )

predLex <- 0

#---------Evaluation----------------
cnfBayes <- confusionMatrix( predBayes, bagSem$labels_model )
mean(cnfBayes$byClass[,"Precision"])
mean(cnfBayes$byClass[,"Recall"])
mean(cnfBayes$byClass[,"F1"])

#--------deep learning-------
#h2o.init(nthreads=-1, max_mem_size="3G")
#h2o.removeAll()



