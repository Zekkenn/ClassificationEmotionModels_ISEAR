# Data Exploration

library("plotly")

# Receives a vector labels... [joy, joy, fear, joy, anger, ...]
visualize_data <- function(data){
  test_frame <- subset(as.data.frame(table(test)), Freq != 0)
  plot_ly(test_frame, x = test_frame$test, y = test_frame$Freq, type = 'bar') %>%
    layout(title = "Distribución de Sentimientos", 
           xaxis = list(title = "Frecuencia de Aparición"), 
           yaxis = list(title = "Sentimientos"))
}