# Data Exploration

library("plotly")

# Receives a vector labels... [joy, joy, fear, joy, anger, ...]
visualize_data <- function(test){
  test_frame <- subset(as.data.frame(table(test)), Freq != 0)
  p <- plot_ly(test_frame, x = test_frame$test, y = test_frame$Freq, type = 'bar') %>%
    layout(title = "Distribución de Sentimientos", 
           xaxis = list(title = "Frecuencia de Aparición"), 
           yaxis = list(title = "Sentimientos"))
  print(p)
}