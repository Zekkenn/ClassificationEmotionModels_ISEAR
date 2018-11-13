# ======================================== IMPORTS =========================================

#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)

source("R code/data_loader.R")
source("R code/data_exploration.R")
source("R code/models/NN.R")
source("R code/models/SVM.R")
source("R code/models/NaiveBayes.R")
source("R code/models/NRC_lexicon.R")

# ==========================================================================================
# ================================ EMOTIONAL CLASIFICATION MODEL ===========================
# ==========================================================================================


# ===================================== TRAINING STAGE =====================================


# X - A Chr Vector with Sentences or Documents
# Y - A Chr Vector with the corresponding emotions
# LANGUAGE - A string specifying the type of speech or dialect
# PREPROCESS - A logical: should be the data preProcess
# REPRES - A string specifying the type of data representation for training
# TUNEBAGG - A String specifying the decision heuristic
ecm <- function(data, language = "English", preProcess = TRUE, repres = "Bag", tuneBagg = "Simple", dataSet = "ISEAR"){
  # Init
  switch (dataSet,
    ISEAR = {
      setLevels(list("joy" = "1", "fear" = "2", "anger" = "3", "sadness" = "4", "disgust" = "5", "shame" = "6", "guilt" = "7","other" = "8"))
    },
    SemEval = {
      setLevels(list("joy" = "1", "fear" = "2", "anger" = "3", "sadness" = "4", "disgust" = "5", "other" = "6"))
    }
  )
  
  # PREPROCESSING STAGE
  if(preProcess){  data <- preproccess.data(data)  }
  
  data <- partition.data( c(0.8, 1), data )
  dataTrain <- data[[1]]
  dataTest <- data[[2]]
  
  # CHARACTERISTICS REPRESENTATION STAGE
  x.rep.train <- list(); x.rep.test <- list()
  switch(repres,
      Bag={ # Bag Of Words Case
        x.rep.train <- bag.of.words(dataTrain)
        x.rep.test <- bag.of.words(dataTest, test = TRUE)
      }
  )
  
  
  # MODEL CONSTRUCTION & TRAINING STAGE
  emot_classifier <- ecm.train(x,x.rep.train,y,tuneBagg)
  
  # SAVE MODEL
  saveRDS(emot_classifier, file = "models.save/emot_classifier.rds")
  
  return(emot_classifier)
}


# X - A Chr Vector with Sentences or Documents
# X.REP - A Matrix containing the character representation of X
# Y - A Chr Vector with the corresponding emotions
# TUNEBAGG - A String specifying the decision heuristic
ecm.train <- function(x, x.rep, y, tuneBagg){

  # MACHINE LEARNING MODELS
  modelBayes <- train.naiveBayes(x.rep)
  modelSVM <- train.svm(x.rep)
  modelNN <- train.nn(x.rep)
  
  # KNOWLEDGE MODELS
  modelNRC <- nrc_analysis(data.frame(SIT = x, EMOT = y))

  modelBagg <- list(tuneBagg = tuneBagg,
                    BAYES = modelBayes, SVM = modelSVM, NN = modelNN, NRC = modelNRC)
  return(modelBagg)
}




# ================================== PREDICTION STAGE ======================================


# ECM.MODEL - Bagging model with the respective decision heuristic & models
# PRED.DATA - DATA OR SENTENCES TO BE PREDICTED
ecm.prediction <- function(ecm.model, pred.data){
  emotional.response <- NULL
  switch (ecm.model$tuneBagg,
          Simple = {
            emotional.response <- baggPred.simple(ecm.model,pred.data)
          },
          Expert = {
            emotional.response <- baggPred.expert(ecm.model,pred.data)
          },
          Probabilities = {
            emotional.response <- baggPred.prob(ecm.model,pred.data)
          }
  )
  
  emotional.response <- factor(emotional.response, levels = getLevels())
  return(emotional.response)
}



baggPred.simple <- function(ecm.model, pred.data){
  pred.data <- pred.data[ pred.data$EMOT != "surprise", ]
  bagOfWords <- bag.of.words(pred.data, test = TRUE)
  lbsSVM <- predict.svm(ecm.model$SVM, bagOfWords, list("joy" = "X1", "fear" = "X2", "anger" = "X3", "sadness" = "X4", "disgust" = "X5", "other" = "X6"))
  lbsNN <- predict.nn(ecm.model$NN, bagOfWords, getLevels())
  lbsLex <- predict.lex(ecm.model$NRC, pred.data)
  lbsBayes <- predict.bayes(ecm.model$BAYES, bagOfWords, getLevels())
  
  #Evaluation
  pred <- class.ind(lbsSVM) + class.ind(lbsNN) +  class.ind(lbsBayes) + class.ind(lbsLex)
  
  pred <- factor(colnames(pred)[max.col(pred,ties.method="first")])
  
  return(pred)
  
}

baggPred.expert <- function(ecm.model, pred.data){
  
}

baggPred.prob <- function(ecm.model, pred.data){
  bagOfWords <- bag.of.words(pred.data)
  lbsSVM <- predict.svm.prob(ecm.model$SVM, bagOfWords, getLevels())
  lbsNN <- predict.nn.prob(ecm.model$NN, bagOfWords, getLevels())
  lbsLex <- predict.lex.prob(ecm.model$NRC, pred.data)
  lbsBayes <- predict.bayes.prob(ecm.model$BAYES, bagOfWords)
}




# ================================== EVALUATION STAGE ======================================


#
ecm.evaluation <- function(data.prediction, data.reference){
  
  # CONFUSION MATRIX
  cm <- confusionMatrix( data.prediction, data.reference )
  
  plot(cm$table, main = "Confusion Matrix")
  write(cm, stdout())
}