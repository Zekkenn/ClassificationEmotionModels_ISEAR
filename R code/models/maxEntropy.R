# Maximum entropy classifier

library(maxent)

source("R code/data_loader.R")

data <- getData.ISEAR("py_isear_dataset/isear.csv")
train.t <- data.frame( X = data$SIT, y = data$Field1 )
all.data <- partition.data( c(0.8, 1), train.t )
isear.docs <- Corpus(VectorSource( all.data[[1]]$X ))
isear.docs <- DocumentTermMatrix(isear.docs)
isear.docs <- as.compressed.matrix(isear.docs)
model <- maxent::maxent(isear.docs, all.data[[1]]$y, use_sgd = T)
y.p <- predict(model, isear.docs)
pred <- factor(y.p[,1])
confusionMatrix(pred, all.data[[1]]$y)

isear.docs <- Corpus(VectorSource( all.data[[2]]$X ))
isear.docs <- DocumentTermMatrix(isear.docs)
isear.docs <- as.compressed.matrix(isear.docs)
y.p <- predict(model, isear.docs)
pred <- factor(y.p[,1])
confusionMatrix(pred, all.data[[2]]$y)
