library(qdap)
library(tm)
library(SnowballC)
library(wordcloud)
#library(RColorBrewer)
library(caret)
#library(readr)
library(tokenizers)
library(plyr)
library(dplyr)
library("XML")
library("methods")
#library("gpuR")
#library(sets)

# ==========================================================================================
# ================================ MAIN PREPARED DATA ======================================
# ==========================================================================================

# ------------------------------------------------------------------------------------------
# --------------------------------- Global Variables ---------------------------------------
# ------------------------------------------------------------------------------------------
#Get Emotions based on the dataset
datasetLvls <- c()
datasetLvlsNum <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

# OBTAIN & PREPROCESS
# type <- should be one of : {ISEAR,SemEval}
getPrep.Data <- function(path = "", type = "ISEAR"){
  
  if (path == "") {
    path <- paste(getwd(),"isear.csv", sep = "/")
  }
  data.raw <- getData.ISEAR(path)
  data.prep <- preproccess.data(data.raw)
  
  return(data.prep)
}

# OBTAIN ISEAR(TRAIN) & SEMEVAL(TEST) WITH RELATED EMOTS
# RELATED EMOTS : {JOY,FEAR,ANGER,SADNESS,DISGUST}
getRaw.complete <- function(isear.path,semeval.path){
  # Obtain Raws
  raw.isear <- getData.ISEAR(isear.path)
  raw.semEval <- getData.SemEval(path = semeval.path)
  
  # Get related emots
  emots.betw <- related.emots(raw.isear,raw.semEval)
  
  isear.train <- raw.isear[raw.isear$EMOT %in% emots.betw,]
  isear.train$EMOT <- factor(isear.train$EMOT, levels = emots.betw)
  
  semeval.test <- raw.semEval[raw.semEval$EMOT %in% emots.betw,]
  semeval.test$EMOT <- factor(semeval.test$EMOT, levels = emots.betw)
  
  return(list(isear.train,semeval.test))
}

getPre.complete <- function(isear.path,semeval.path){
  ss <- getRaw.complete(isear.path,semeval.path)
  pre.isear <- preproccess.data(ss[[1]])
  pre.semeval <- preproccess.data(ss[[2]])
  return(list(pre.isear,pre.semeval))
}

# type.data : {pre,raw}
# rep : {bagOfWords}
getRepr.complete <- function(isear.path,semeval.path,type.data = "pre",rep = "bagOfWords"){
  if (type.data == "pre") {
    ss <- getPre.complete(isear.path,semeval.path)
  }else if (type.data == "raw"){
    ss <- getRaw.complete(isear.path,semeval.path)
  }else {
    return(NULL)
  }
  
  if (rep == "bagOfWords") {
    bag.isear <- bag.of.words(ss[[1]])
    bag.semeval <- bag.of.words(ss[[2]])
  } else {
    return(NULL)
  }
  
  return(list(bag.isear,bag.semeval))
}

# ==========================================================================================
# ================================ DATA EXTRACTION =========================================
# ==========================================================================================


# ====================================== ISEAR =============================================

# LOAD ISEAR DATA
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

# ==================================== SEMEVAL =============================================

# LOAD SEMEVAL:14 DATA selecting the most intense emotion
# type <- should be one of : {trial,test}
getData.SemEval <- function(path, type = "test"){
  # Convert the Corpus xml file to a data frame.
  affective.type <- paste("affectivetext",type,sep = "_")
  affective.post <- paste(affective.type,"xml",sep = ".")
  affective.emot <- paste(affective.type,"emotions.gold",sep = ".")
  affective.val <- paste(affective.type,"valence.gold",sep = ".")
  
  xmldataframe <- xmlToDataFrame(paste(path,affective.post,sep = "/"), stringsAsFactors = FALSE)
  emots <- read.csv(paste(path,affective.emot,sep = "/"), stringsAsFactors = FALSE, header = FALSE, sep = " ", col.names = c("id","anger","disgust","fear","joy","sadness","surprise"))
  valence <- read.csv(paste(path,affective.val,sep = "/"), stringsAsFactors = FALSE, header = FALSE, sep = " ", col.names = c("id","valence"))
  
  emots <- emots[-1]
  emots <- emots[,c(4,3,1,5,2,6)]
  emots.max <- apply(emots, 1, which.max)
  emots.max <- factor(x = emots.max)
  levels(emots.max) <- list("joy" = "1", "fear" = "2", "anger" = "3", "sadness" = "4", "disgust" = "5", "surprise" = "6")
  
  semEval <- data.frame(SIT = xmldataframe$text, EMOT = emots.max, VAL = valence$valence , stringsAsFactors = FALSE)
  return(semEval)
}









# ==========================================================================================
# =============================== DATA PRE-PROCESSING ======================================
# ==========================================================================================

# DATA PRE_PROCESS
preproccess.data <- function(data, stemming = TRUE){

  # DELETE ROWS WITH 1 LENGTH SENTENCES
  pos <- which(sapply(tokenize_words(data$SIT), length) %in% c(0,1,2))
  if (length(pos) != 0) {data <- data[-pos,]}
  
  # DELETE ALL NON-ALPHANUMERIC CHARACTERS & additional whitespace
  #data$SIT <- sapply(data$SIT, function(x) gsub("[^a-zA-Z0-9']", " ", x)) # non-alphanumeric characters
  #data$SIT <- sapply(data$SIT, function(x) gsub("\\s+", " ", x)) # additional whitespace
  #data$SIT <- sapply(data$SIT, function(x) gsub(" $", "", x)) # end Whitespace
  
  # LOWER CASE
  # data$SIT <- sapply(data$SIT, tolower)
  
  # TM - STEMMING
  corpus <- Corpus(VectorSource(data$SIT))
  corpus.clean <- corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(removeWords, stopwords(kind="en")) %>%
    tm_map(stripWhitespace)
  
  corpus.clean <- ifelse(stemming, tm_map(corpus.clean, stemDocument), corpus.clean)
  
  # GET STEMMING SENTENCES BACK TO DATA
  data$SIT <- sapply(corpus.clean, identity)
  
  return(data)
}




# ==========================================================================================
# =================== DATA REPRESENTATION FOR TRAINING ML MODELS ===========================
# ==========================================================================================

# ================================= BAG OF WORDS ===========================================

# CREATE THE BAG OF WORDS
bag.of.words <- function(data, sparse = 0.999, init = FALSE, test = FALSE){
  docs <- Corpus(VectorSource(data$SIT))
  words.dict <- list()
  if ( init == TRUE ){
    #Remove Sparse Terms
    dtm <- DocumentTermMatrix(docs)
    dtm <- removeSparseTerms(dtm, sparse)
    createDict(dtm)
  } else {
    words.dict <- scan("dict.txt", what = character())
    dtm <- DocumentTermMatrix(docs, list( dictionary = words.dict ))
  }
  
  mat <- as.matrix(dtm)
  
  if(!test){
    # Delete all rows that have all columns in zero and normalize
    row_sub <- apply(mat, 1, function(row) all(row ==0 ))
    mat <- mat[!row_sub,]
    mat <- scale(mat)
    
    bagOfWords <- cbind( mat, data$EMOT[!row_sub] )
  } else {
    mat <- scale(mat)
    bagOfWords <- cbind( mat, data$EMOT )
  }
  
  colnames( bagOfWords )[ ncol(bagOfWords) ] <- "labels_model"
  bagOfWords <- as.data.frame(bagOfWords)
  bagOfWords[ is.na(bagOfWords) ] <- 0
  
  bagOfWords$labels_model <- factor(bagOfWords$labels_model)
  levels(bagOfWords$labels_model) <- getLevels()
  
  return(bagOfWords)
}

# Create a Dictionary 
createDict <- function(dtm){
  words.dict <- list()
  tryCatch({
    words.dict <- scan("dict.txt", what = character())
  }, warning = function(w) {}, error = function(e) {})
  words.data <- findFreqTerms(dtm, 1)
  words.dict <- unique(c( words.data, words.dict ))
  write(file = "dict.txt", append = F, x = "")
  d <- lapply(words.dict, write, file="dict.txt", append=T)
  d <- NULL
}

# GET TRAIN, TEST AND VALIDATION DATA TURNED INTO THE BAG OF WORDS
get.bagOfWords.allPartData <- function(path = ""){
  
  # GET THE BAG OF WORDS AND ITS LABELS
  data <- getPrep.Data(path)
  levels( data$EMOT ) <- getLevelsNum()
  data$EMOT <- as.numeric(data$EMOT)
  mat <- bag.of.words(data, sparse = 0.999)
  # Delete all rows that have all columns in zero and normalize
  row_sub <- apply(mat, 1, function(row) all(row ==0 ))
  mat <- mat[!row_sub,]
  mat <- scale(mat)
  
  bagOfWords <- cbind( mat, data$EMOT[!row_sub] )
  
  colnames( bagOfWords )[ ncol(bagOfWords) ] <- "labels_model"
  bagOfWords <- as.data.frame(bagOfWords)
  bagOfWords$labels_model <- factor(bagOfWords$labels_model)
  levels(data$EMOT) <- getLevels()
  # PARTITION THE DATA
  data <- partition.data( c(0.8, 1), bagOfWords )
  
  return(data)
}




# ==========================================================================================
# ==================================== UTILITIES ===========================================
# ==========================================================================================

# Partition data into the num of values and their size
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

# OBTAIN RELATED EMOTS BETWEEN TWO DATASETS
related.emots <- function(data1,data2){
  return(intersect(levels(data1$EMOT),levels(data2$EMOT)))
}

#Example of code setLevels(levels(dataIsear$EMOT))
# list("joy" = "1", "fear" = "2", "anger" = "3", "sadness" = "4", "disgust" = "5", "other" = "6")
setLevels <- function(lvls){
  datasetLvls <<- lvls
}

getLevels <- function(){
  return(datasetLvls)
}

getLevelsNum <- function(){
  return( datasetLvlsNum[1:length(datasetLvls)] )
}

# Compare two datasets
compareDatasets <- function(set1, set2){
  dataWordsSet1 <- DocumentTermMatrix( Corpus(VectorSource(set1$SIT)) )$dimnames$Terms
  dataWordsSet2 <- DocumentTermMatrix( Corpus(VectorSource(set2$SIT)) )$dimnames$Terms
  
  inters <- intersect(dataWordsSet1, dataWordsSet2)
  diffSet1 <- setdiff(dataWordsSet1, dataWordsSet2)
  diffSet2 <- setdiff(dataWordsSet2, dataWordsSet1)
  
}

