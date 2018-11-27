# ======================================== IMPORTS =========================================

this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

source("data_loader.R")
source("data_exploration.R")
source("models/NN.R")
source("models/SVM.R")
source("models/NaiveBayes.R")
source("models/NRC_lexicon.R")

# ==========================================================================================
# ================================ EMOTIONAL CLASIFICATION MODEL ===========================
# ==========================================================================================

# set.seed(34)
# ss <- getRaw.complete("../py_isear_dataset/isear.csv","../SemEval_14/AffectiveText.test")
# isear.raw <- partition.data(c(0.8,1),ss[[1]])
# isear.raw.train <- isear.raw[[1]]
# isear.raw.test <- isear.raw[[2]]
# model.ecm <- ecm(isear.raw.train$SIT,isear.raw.train$EMOT,stem = FALSE)

# ===================================== TRAINING STAGE =====================================

# X - A Chr Vector with Sentences or Documents
# Y - A Chr Vector with the corresponding emotions
# LANGUAGE - A string specifying the type of speech or dialect
# PREPROCESS - A logical: should be the data preProcess
# REPRES - A string specifying the type of data representation for training
# TUNEBAGG - A String specifying the decision heuristic
ecm <- function(x, y, language = "english", preProcess = TRUE, repres = "Bag", tuneBagg = "Simple", stem = TRUE){
  # Init
  setLevels(levels(y))
  
  str("Initializing Model ....")
  data <- data.frame(SIT = x, EMOT = y, stringsAsFactors = FALSE)
  data$EMOT <- factor(data$EMOT)

  # PREPROCESSING STAGE
  if(preProcess){  data <- preproccess.data(data,stemming = stem,language=language)  }
  
  # CHARACTERISTICS REPRESENTATION STAGE
  str("Representation Stage ...")
  switch(repres,
      Bag={ # Bag Of Words Case
        x.rep.train <- DocumentTermMatrix(Corpus(VectorSource(data$SIT)))
        # x.rep.train <- bag.of.words(data)
      }
  )
  
  
  # MODEL CONSTRUCTION & TRAINING STAGE
  emot_classifier <- ecm.train(data$SIT,x.rep.train,data$EMOT,tuneBagg)
  
  # SAVE MODEL
  str("Saving Model ...")
  saveRDS(emot_classifier, file = "models.save/emot_classifier.rds")
  
  return(emot_classifier)
}


# X - A Chr Vector with Sentences or Documents
# X.REP - A Matrix containing the character representation of X
# Y - A Chr Vector with the corresponding emotions
# TUNEBAGG - A String specifying the decision heuristic
ecm.train <- function(x, x.rep, y, tuneBagg){

  # MACHINE LEARNING MODELS
  str("Training NaiveBayes ...")
  modelBayes <- train.naiveBayes(x.rep,y)
  str("Training SVM ...")
  modelSVM <- train.svm(x.rep, y)
  str("Training NN ...")
  ## modelNN <- train.nn(x.rep, y)
  modelNN <- c("nothing here")
  
  # KNOWLEDGE MODELS
  modelNRC <- nrc_analysis(x, y)

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
  bagOfWords <- bag.of.words(pred.data, test = TRUE)
  lbsSVM <- predict.svm(ecm.model$SVM, bagOfWords, list("joy" = "X1", "fear" = "X2", "anger" = "X3", "sadness" = "X4", "disgust" = "X5","other" = "X6"))
  lbsNN <- predict.nn(ecm.model$NN, bagOfWords, getLevels())
  lbsLex <- predict.lex(ecm.model$NRC, pred.data)
  lbsBayes <- predict.bayes(ecm.model$BAYES, bagOfWords, getLevels())
  
  #Evaluation
  pred <- class.ind(lbsSVM) + class.ind(lbsNN) +  class.ind(lbsBayes) + class.ind(lbsLex)
  
  pred <- factor(colnames(pred)[max.col(pred,ties.method="first")])
  levels(pred) <- getLevels()
  
  return(pred)
  
}

baggPred.expert <- function(ecm.model, pred.data){
  bagOfWords <- bag.of.words(pred.data, test = TRUE)
  lbsSVM <- predict.svm(ecm.model$SVM, bagOfWords, list("joy" = "X1", "fear" = "X2", "anger" = "X3", "sadness" = "X4", "disgust" = "X5","other" = "X6"))
  lbsNN <- predict.nn(ecm.model$NN, bagOfWords, getLevels())
  lbsLex <- predict.lex(ecm.model$NRC, pred.data)
  lbsBayes <- predict.bayes(ecm.model$BAYES, bagOfWords, getLevels())
  
  #Evaluation
  pred <- class.ind(lbsSVM) + class.ind(lbsNN) +  class.ind(lbsBayes) + class.ind(lbsLex)
  
  pred <- factor(colnames(pred)[max.col(pred,ties.method="first")])
  levels(pred) <- getLevels()
  
  return(pred)
}

baggPred.prob <- function(ecm.model, pred.data){
  bagOfWords <- bag.of.words(pred.data)
  lbsSVM <- predict.svm.prob(ecm.model$SVM, bagOfWords)
  lbsNN <- predict.nn.prob(ecm.model$NN, bagOfWords)
  lbsBayes <- predict.bayes.prob(ecm.model$BAYES, bagOfWords)
  
  colnames(lbsSVM) <- colnames(lbsBayes)
  colnames(lbsNN) <- colnames(lbsBayes)
  
  pred <- lbsSVM + lbsNN + lbsBayes
  
  pred <- factor(colnames(pred)[max.col(pred,ties.method="first")])
  levels(pred) <- getLevels()
  return(pred)
}




# ================================== EVALUATION STAGE ======================================


ecm.evaluation <- function(data.prediction, data.reference){
  
  # CONFUSION MATRIX
  cm <- confusionMatrix( data.prediction, data.reference )
  
  plot(cm$table, main = "Confusion Matrix")
  write(cm, stdout())
}
