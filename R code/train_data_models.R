
#setwd("C:/Users/ASUS/Documents/Sebastian/ClassificationEmotionModels_ISEAR")

source("R code/data_loader.R")
source("R code/models/NN.R")
source("R code/models/SVM.R")
source("R code/models/NaiveBayes.R")
source("R code/models/NRC_lexicon.R")

dataSem <- getData.SemEval(path = "SemEval_14/AffectiveText.test")
dataIsear <- getData.ISEAR(path = "py_isear_dataset/isear.csv")

dataIsear <- dataIsear[ !(dataIsear$EMOT %in% c("guilt", "shame")), ]
levels(dataIsear$EMOT) <- getLevels()

bagSem <- bag.of.words(dataSem)
bagIsear <- bag.of.words(dataIsear)

bagSem <- bagSem[ !is.na(bagSem$labels_model), ]

#--------Train------------
modelBayes <- train.naiveBayes(bagIsear)
modelSVM <- train.svm(bagIsear)
modelNN <- train.nn(bagIsear)

#--------Save Models--------
saveRDS(modelBayes, file = "R code/models.save/modelBayesSem.rds")
saveRDS(modelSVM, file = "models.save/modelSVMSem_test1.rds")
saveRDS(modelNN, file = "R code/models.save/modelNNSem.rds")

#--------Load Models--------
modelSVM <- readRDS("models.save/modelSVMSem_test1.rds")
modelBayes <- readRDS("R code/models.save/modelBayesSem.rds")
modelNN <- readRDS("R code/models.save/modelNNSem.rds")

#--------Load Models Composed--------
emot_classifier <- readRDS("models.save/emot_classifier/emot_classifier.rds")
emot_classifier_spa <- readRDS("models.save/emot_classifier_spa/emot_classifier.rds")
emot_classifier_stem <- readRDS("models.save/emot_classifier_stem/emot_classifier.rds")

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
cm <- confusionMatrix( predBayes, bagSem$labels_model )
cm$byClass["Class: other",] <- 0
mean(cm$byClass[,"Precision"])
mean(cm$byClass[,"Recall"])
mean(cm$byClass[,"F1"])

#--------deep learning-------
#h2o.init(nthreads=-1, max_mem_size="3G")
#h2o.removeAll()

# Charge data
set.seed(34)
ss <- getRaw.complete("../py_isear_dataset/isear.csv","../SemEval_14/AffectiveText.test")
isear.raw <- partition.data(c(0.8,1),ss[[1]])
isear.raw.train <- isear.raw[[1]]
isear.raw.test <- isear.raw[[2]]

data <- data.frame(SIT = isear.raw.train$SIT, EMOT = isear.raw.train$EMOT, stringsAsFactors = FALSE)
data$EMOT <- factor(data$EMOT)
data <- preproccess.data(data,stemming = FALSE,language="english")
x.rep.train <- DocumentTermMatrix(Corpus(VectorSource(data$SIT)))

data.test.is <- data.frame(SIT = isear.raw.test$SIT, EMOT = isear.raw.test$EMOT, stringsAsFactors = FALSE)
data.test.is$EMOT <- factor(data.test.is$EMOT)
data.test.is <- preproccess.data(data.test.is,stemming = FALSE,language="english")
x.rep.test <- DocumentTermMatrix(Corpus(VectorSource(data.test.is$SIT)))

data <- data.frame(SIT = ss[[1]]$SIT, EMOT = ss[[1]]$EMOT, stringsAsFactors = FALSE)
data$EMOT <- factor(data$EMOT)
data <- preproccess.data(data,stemming = FALSE,language="english")
x.rep <- DocumentTermMatrix(Corpus(VectorSource(data$SIT)))


