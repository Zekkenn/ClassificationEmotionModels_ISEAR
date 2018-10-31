# Neural Network Classification Model

library(neuralnet)
library(nnet)
library(ggplot2)

getEcuation_7 <- function(){
  f <- as.formula(paste("l1 + l2 + l3 + l4 + l5 + l6 + l7 ~ ", 
                        paste(n[!n %in% c("`l1`","`l2`","`l3`", "`l4`","`l5`","`l6`", "`l7`")], 
                              collapse = " + ")))
  return(f)
}

getEcuation_5 <- function(){
  f <- as.formula(paste("l1 + l2 + l3 + l4 + l5 ~ ", 
                        paste(n[!n %in% c("`l1`","`l2`","`l3`", "`l4`","`l5`")], 
                              collapse = " + ")))
  return(f)
}

train.nn <- function(data){
  
  train <- pre_proc(data)
  
  n <- sprintf("`%s`", names(train))
  f <- getEcuation_5()
  
  nn <- neuralnet(f, data = train, hidden = c(500), act.fct = "logistic", linear.output = FALSE, lifesign = "minimal")
  
}

pre_proc <- function(data){
  size <- (dim(data)[2]-1)
  data <- cbind(data[, 1:size], class.ind(as.factor(data$labels_model)))
  names(data) <- c(names(data)[ 1:size ],"l1","l2","l3", "l4","l5")
  return(data)
}

# ====== TEST ========
# > test <- data[[2]]
# > size <- dim(test)[2]-1
# > test <- test[,1:size]
# > pred_with <- compute( model_nn_without, test )
# > pred_with_nn_factor <- max.col(pred_with$net.result)
# > pred_with_nn_factor <- factor( pred_with_nn_factor )
# > confusionMatrix( pred_with_nn_factor, data_new[[2]]$labels_model )