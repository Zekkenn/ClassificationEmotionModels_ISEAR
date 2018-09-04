# Neural Network Classification Model

library(neuralnet)
library(nnet)
library(ggplot2)

train.nn <- function(data){
  
  size <- (dim(data)[2]-1)
  train <- cbind(data[, 1:size], class.ind(as.factor(data$labels_model)))
  names(train) <- c(names(train)[ 1:size ],"l1","l2","l3", "l4","l5","l6", "l7")
  
  n <- sprintf("`%s`", names(train))
  f <- as.formula(paste("l1 + l2 + l3 + l4 + l5 + l6 + l7 ~ ", 
                        paste(n[!n %in% c("`l1`","`l2`","`l3`", "`l4`","`l5`","`l6`", "`l7`")], 
                              collapse = " + ")))
  
  
  
  nn <- neuralnet(f, data = train, hidden = c(50, 10), act.fct = "logistic", linear.output = FALSE, lifesign = "minimal")
}
