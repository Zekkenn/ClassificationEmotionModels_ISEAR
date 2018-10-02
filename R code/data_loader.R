library(qdap)
library(tm)
library(SnowballC)
library(wordcloud)
#library(RColorBrewer)
library(caret)
#library(readr)
library(tokenizers)
library(dplyr)

# ================================ DATA MAIN ==============================================

# DATA TO OBTAIN
getData.ISEAR <- function(path){
  
  # READ & GET IMPORTANT ATTRIBUTES
  data <- read.csv(path, header = TRUE, sep = "|", stringsAsFactors = FALSE)
  data <- data[c("COUN","SUBJ","AGE","EMOT","Field1","SIT","LONG","INTS")]
  
  # EMOTIONS TAGGED
  data <- data[(data$Field1 != ""),]
  data$EMOT <- factor(data$EMOT)
  levels(data$EMOT) <- list("joy" = "1", "fear" = "2", "anger" = "3", "sadness" = "4", "disgust" = "5", "shame" = "6", "guilt" = "7")
  
  return(data)
}


# GET ALL DATA WITHOUT NOISE
#   EMOT label -> emotion tag
#   SIT label -> Text
getPreproc.Data.ISEAR <- function(path = ""){
  
  if (path == "") {
    path <- paste(getwd(),"isear.csv", sep = "/")
  }
  data.raw <- getData.ISEAR(path)
  data.prep <- preproccess.ISEAR(data.raw)
  
  return(data.prep)
}


# DATA PRE_PROCESS
preproccess.ISEAR <- function(isear.data){
  
  # DELETE ROWS WITH 1 LENGTH SENTENCES
  pos <- which(sapply(tokenize_words(isear.data$SIT), length) == 1)
  isear.data <- isear.data[-pos,]
  
  # DELETE ALL NON-ALPHANUMERIC CHARACTERS & additional whitespace
  #isear.data$SIT <- sapply(isear.data$SIT, function(x) gsub("[^a-zA-Z0-9']", " ", x)) # non-alphanumeric characters
  #isear.data$SIT <- sapply(isear.data$SIT, function(x) gsub("\\s+", " ", x)) # additional whitespace
  #isear.data$SIT <- sapply(isear.data$SIT, function(x) gsub(" $", "", x)) # end Whitespace
  
  # LOWER CASE
  # isear.data$SIT <- sapply(isear.data$SIT, tolower)
  
  # TM - STEMMING
  isear.docs <- Corpus(VectorSource(isear.data$SIT))
  isear.docs <- tm_map(isear.docs, content_transformer(removeNumbers)) # Remove numbers
  isear.docs <- tm_map(isear.docs, content_transformer(tolower)) # Transform words to lower
  isear.docs <- tm_map(isear.docs, content_transformer(removeWords), stopwords("english")) # Remove english common stopwords e.g "the", "is", "of", etc
  isear.docs <- tm_map(isear.docs, content_transformer(removePunctuation)) # Remove punctuations
  isear.docs <- tm_map(isear.docs, stripWhitespace) # Eliminate extra white spaces
  
  isear.docs <- tm_map(isear.docs, stemDocument) # Text stemming (reduces words to their root form)
  #isear.docs <- tm_map(isear.docs, removeWords, c("clintonemailcom", "stategov", "hrod")) # Remove additional stopwords
  
  # GET STEMMING SENTENCES BACK TO ISEAR.DATA
  isear.data$SIT <- sapply(isear.docs, identity)
  
  return(isear.data)
}








# ========================= DATA FOR TRAINING ML MODELS ====================================

# CREATE THE BAG OF WORDS
bag.of.words <- function(isear.data, sparse = 0.999, train = TRUE){
  isear.docs <- Corpus(VectorSource(isear.data))
  words.dict <- list()
  if ( train == TRUE ){
    #Remove Sparse Terms
    isear.dtm <- DocumentTermMatrix(isear.docs)
    isear.dtm <- removeSparseTerms(isear.dtm, sparse)
    words.dict <- findFreqTerms(isear.dtm, 1)
    d <- lapply(words.dict, write, file="dict.txt", append=F)
    d <- NULL
  } else {
    words.dict <- scan("dict.txt", what = character())
    isear.dtm <- DocumentTermMatrix(isear.docs, list( dictionary = words.dict ))
  }
  
  mat <- as.matrix(isear.dtm)

  return(mat)
}

# GET TRAIN, TEST AND VALIDATION DATA TURNED INTO THE BAG OF WORDS
get.bagOfWords.allPartData <- function(path = ""){
  
  # GET THE BAG OF WORDS AND ITS LABELS
  data <- getPreproc.Data.ISEAR(path)
  levels( data$EMOT ) <- list("1" = "joy", "2" = "fear", "3" = "anger", "4" = "sadness", "5" = "disgust", "6" = "shame", "7" = "guilt")
  data$EMOT <- as.numeric(data$EMOT)
  
  mat <- bag.of.words(data$SIT, sparse = 0.99)
  # Delete all rows that have all columns in zero and normalize
  row_sub <- apply(mat, 1, function(row) all(row ==0 ))
  mat <- mat[!row_sub,]
  mat <- scale(mat)
  
  bagOfWords <- cbind( mat, data$EMOT[!row_sub] )
  
  colnames( bagOfWords )[ ncol(bagOfWords) ] <- "labels_model"
  bagOfWords <- as.data.frame(bagOfWords)
  bagOfWords$labels_model <- factor(bagOfWords$labels_model)
  levels(data$EMOT) <- list("joy" = "1", "fear" = "2", "anger" = "3", "sadness" = "4", "disgust" = "5", "shame" = "6", "guilt" = "7")
  # PARTITION THE DATA
  data <- partition.data( c(0.8, 1), bagOfWords )
  
  return(data)
}


# PARTITIONED DATA TO TRAIN
partition.data <- function(values, data){
  size <- dim(data)[1]
  data <- data[ sample(nrow(data), nrow(data)), ]
  values <- values*size
  values <- append(values, c(1), after = 0)
  data_part <- c()
  for ( i in 1:(length(values)-1) ){
    data_part[[i]] <- data[ values[i]:values[i+1], ]
  }
  return(data_part)
}







