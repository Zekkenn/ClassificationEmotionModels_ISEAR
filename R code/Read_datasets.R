# Read Datasets from folder /datasets - the path should be in ClassificationEmotionModels_ISEAR folder

library("readr")

readData_MovieReview <- function(){
  negfiles <- paste("datasets/txt_sentoken/neg/", 
                    list.files("datasets/txt_sentoken/neg", pattern = ".*.txt"), 
                    sep = "")
  posfiles <- paste("datasets/txt_sentoken/pos/",
                    list.files("datasets/txt_sentoken/pos", pattern = ".*.txt"),
                    sep = "")
  posData = lapply(posfiles, read_file)
  negData = lapply(negfiles, read_file)
  data <- cbind(posData, rep(1, length(posData)))
  data <- rbind( data, cbind(negData, rep(0, length(negData))) )
  return(data)
}

# bagFrame <- as.data.frame( cbind( bag, data[,2] ) )
# bagFrame <- as.data.frame(bag )
# bagFrame$labels_models <- unlist(data[,2], use.names=FALSE)
# dataFrame <- as.data.frame(data)
# names(dataFrame) <- c("X", "labels_model")
# train.svm(dataFrame, lvls = c("X1", "X2"))