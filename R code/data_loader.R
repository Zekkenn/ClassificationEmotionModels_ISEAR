#setwd("/home/suntse/Documents/Universidad/trab_dir/ClassificationEmotionModels_ISEAR")

file <- "py_isear_dataset/isear.csv"
#mat <- read.csv(file, header = TRUE, sep = "|", dec = ".")

library(qdap)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(caret)
library(readr)

# DATA MAIN

# get_all_data <- function(){
#   return( mat )
# }
# 
# get_data <- function(labels_data){
#   return( mat[,labels_data] )
# }

# GET TRAIN, TEST AND VALIDATION DATA TURNED INTO THE BAG OF WORDS
get.bagOfWords.allPartData <- function(path = ""){
  
  # GET THE BAG OF WORDS AND ITS LABELS
  data <- getPreproc.Data.ISEAR(path)
  levels( data$EMOT ) <- list("1" = "joy", "2" = "fear", "3" = "anger", "4" = "sadness", "5" = "disgust", "6" = "shame", "7" = "guilt")
  data$EMOT <- as.numeric(data$EMOT)
  bagOfWords <- cbind( bag.of.words(data), data$EMOT )
  colnames( bagOfWords )[ ncol(bagOfWords) ] <- "labels"
  
  # PARTITION THE DATA
  data <- partition.data( c(0.6, 0.8, 1), bagOfWords )
    
  return(data)
}

# Get All Data without noise ---> EMOT label -> emotion tag
# SIT label -> Text
getPreproc.Data.ISEAR <- function(path = ""){
  
  if (path == "") {
    path <- paste(getwd(),"isear.csv", sep = "/")
  }
  data.raw <- getData.ISEAR(path)
  data.prep <- preproccess.ISEAR(data.raw)
  
  return(data.prep)
}

# PARTITIONED DATA TO TRAIN
partition.data <- function(values, data){
  size <- dim(data)[1]
  values <- values*size
  values <- append(values, c(1), after = 0)
  data_part <- c()
  for ( i in 1:(length(values)-1) ){
    data_part[[i]] <- data[ values[i]:values[i+1], ]
  }
  return(data_part)
}

# CREATE THE BAG OF WORDS
bag.of.words <- function(isear.data){
  isear.docs <- Corpus(VectorSource(isear.data$SIT))
  isear.dtm <- DocumentTermMatrix(isear.docs)
  mat <- as.matrix(isear.dtm)
  return(mat)
}

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

# DATA PRE_PROCESSING

preproccess.ISEAR <- function(isear.data){
  
  # DELETE ALL NON-ALPHANUMERIC CHARACTERS & additional whitespace
  isear.data$SIT <- sapply(isear.data$SIT, function(x) gsub("[^a-zA-Z0-9']", " ", x)) # non-alphanumeric characters
  isear.data$SIT <- sapply(isear.data$SIT, function(x) gsub("\\s+", " ", x)) # additional whitespace
  isear.data$SIT <- sapply(isear.data$SIT, function(x) gsub(" $", "", x)) # end Whitespace
  
  
  # LOWER CASE
  isear.data$SIT <- sapply(isear.data$SIT, tolower)
  
  
  # TM - STEMMING
  isear.docs <- Corpus(VectorSource(isear.data$SIT))
  isear.docs <- tm_map(isear.docs, removeNumbers) # Remove numbers
  isear.docs <- tm_map(isear.docs, removeWords, stopwords("english")) # Remove english common stopwords e.g "the", "is", "of", etc
  isear.docs <- tm_map(isear.docs, removePunctuation) # Remove punctuations
  isear.docs <- tm_map(isear.docs, stripWhitespace) # Eliminate extra white spaces
  
  isear.docs <- tm_map(isear.docs, stemDocument) # Text stemming (reduces words to their root form)
  isear.docs <- tm_map(isear.docs, removeWords, c("clintonemailcom", "stategov", "hrod")) # Remove additional stopwords
  
  # ========== UNCOMMENT THE NEXT LINES FOR WORDCLOUD WARN : SLOW COMP TIME =================
  # FRECUENCY MATRIX OF WORDS
  # dtm <- TermDocumentMatrix(isear.docs)
  # isear.freq <- as.matrix(dtm)
  # isear.freq <- sort(rowSums(isear.freq),decreasing=TRUE)
  # isear.freq <- data.frame(word = names(isear.freq),freq=isear.freq)
  
  
  # GENERATE WORDCLOUD
  #generate.wordcloud(isear.freq, "ISEARS's  words - Most Used")
  # =========================================================================================
  
  # GET STEMMING SENTENCES BACK TO ISEAR.DATA
  isear.data$SIT <- sapply(isear.docs, identity)
  
  return(isear.data)
}


generate.wordcloud <- function(data.freq, data.title = "Most Used Words"){
  # Generate a wordcloud based on a data frame of frequencies
  par(bg="grey30")
  png(file="WordCloud.png",width=1000,height=700, bg="grey30")
  write("Generating wordCloud .....", stdout())
  wordcloud(data.freq$word, data.freq$freq, col=terrain.colors(length(data.freq$word), alpha=0.9), random.order=FALSE, rot.per=0.3 )
  write("Done, Saved on Working Directory", stdout())
  title(main = data.title, font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
  dev.off()
}
