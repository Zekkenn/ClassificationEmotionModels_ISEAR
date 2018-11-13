# ======================================== IMPORTS =========================================

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("data_loader.R")
source("data_exploration.R")
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
ecm <- function(x, y, language = "English", preProcess = TRUE, repres = "Bag", tuneBagg = "Simple"){
  # Init
  setLevels(levels(y))
  
  # PREPROCESSING STAGE
  if(preProcess){  x <- preproccess.data(data.frame(SIT = x))  }
  
  # CHARACTERISTICS REPRESENTATION STAGE
  x.rep <- x
  switch(repres,
      Bag={ # Bag Of Words Case
        x.rep <- bag.of.words(data.frame(SIT = x))
      }
  )
  
  # MODEL CONSTRUCTION & TRAINING STAGE
  emot_classifier <- ecm.train(x,x.rep,y,tuneBagg)
  
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
  bagOfWords <- bag.of.words(pred.data, sparse = 0.9999999)
  lbsSVM <- predict.svm(ecm.model$SVM, bagOfWords, list("joy" = "X1", "fear" = "X2", "anger" = "X3", "sadness" = "X4", "disgust" = "X5", "other" = "X6"))
  lbsNN <- predict.nn(ecm.model$NN, bagOfWords, getLevels())
  lbsLex <- predict.lex(ecm.model$NRC, pred.data)
  lbsBayes <- predict.bayes(ecm.model$BAYES, bagOfWords, getLevels())
  
  #Evaluation
  pred <- class.ind(lbsSVM) + class.ind(lbsNN) +  class.ind(lbsBayes)
  
  
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