setwd("C:\Users\ASUS\Documents\Sebastian\ClassificationEmotionModels_ISEAR")

file <- "py_isear_dataset/isear.csv"
mat <- read.csv(file, header = TRUE, sep = "|", dec = ".")

get_all_data <- function(){
  return( mat )
}

get_data <- function(labels_data){
  return( mat[,labels_data] )
}

