# Composed Model
# modelSVM  ---------> Support vector machine model  -------> Bag of words
# modelLex  ---------> Lexicon model                 -------> Data in text
# modelNN   ---------> Neural Network model          -------> Bag of words
# modelBayes --------> Bayes Network Model           -------> Bag of words

loadFiles <- function(){
  source("R code/data_loader.R")
  source("R code/models/NN.R")
  source("R code/models/SVM.R")
  source("R code/models/NaiveBayes.R")
  source("R code/models/NRC_lexicon.R")
}

#Evaluation function
evalSimple <- function(lbsSVM, lbsNN, lbsBayes, lbsLex){
  
}

#Data is in the basic form, a data frame with SIT and EMOT labels
simpleComposedModel <- function( modelSVM, modelNN, modelBayes, ModelLex, data){
  bagOfWords <- bag.of.words(data)
  lbsSVM <- predict.svm(modelSVM, bagOfWords, getLevels())
  lbsNN <- predict.nn(modelNN, bagOfWords, getLevels())
  lbsLex <- predict.lex(ModelLex, data)
  lbsBayes <- predict.bayes(modelBayes, bagOfWords)
  
  #Evaluation
  result <- evalSimple(lbsSVM, lbsNN, lbsBayes, lbsLex)
  return(result)
}

maxProbModel <- function( modelSVM, ModelLex, modelNN, modelBayes, data){
  
}

test <- function(){
  a <<- 5
}
