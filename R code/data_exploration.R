library(tidyverse)
library(tokenizers)
library(plotly)

# =============================== EXPLORATORY ANALYSIS ========================================

# GENERAL DATA EXPLORATION
#   sentences: chr vector with speeches
#   emotions: factor with emotion label for every sentence
explore.data <- function(sentences,emotions){
  print(str(sentences))
  print(str(emotions))
  
  # ANALYSIS OF WORDS ON SENTENCES
  write("WORDS ON SENTENCES",stdout())
  words <- tokenize_words(sentences)
  visualize.nwords(words,emotions,"words")
  
  # ANALYSIS OF PHRASES ON SENTENCES
  write("PHRASES ON SENTENCES",stdout())
  phrases <- tokenize_sentences(sentences)
  visualize.nwords(phrases,emotions,"phrases")
  
  # EMOTION FRECUENCIES
  visualize_data(emotions)
  
  # WORDCLOUD
  generate.wordcloud(sentences)
}


# HISTOGRAM OF EMOTIONS FRECUENCIES 
#   Receives a vector labels... [joy, joy, fear, joy, anger, ...]
visualize_data <- function(test){
  test_frame <- subset(as.data.frame(table(test)), Freq != 0)
  pal <- c("red", "blue", "green")
  p <- plot_ly(test_frame, x = test_frame$test, y = test_frame$Freq, type = 'bar', color = ~test_frame$test) %>%
    layout(title = "Distribución de Emociones", 
           xaxis = list(title = "Frecuencia de Apariciones"), 
           yaxis = list(title = "Emociones"))
  print(p)
}


# SCATTER OF WORDS OR PHRASES IN PHRASES
visualize.nwords <- function(words,emots,part.of.whole = "words"){
  total.words <- plot_ly(x = rep(1:length(words)), y = sapply(words, length), name = "Word counts", type = "scatter", mode = "lines") %>%
    add_trace(y = ~mean(sapply(words, length)), name = paste('Mean: ', mean(sapply(words, length))), line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) %>%
    add_trace(y = ~min(sapply(words, length)), name = paste('Min: ', min(sapply(words, length))), line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dot')) %>%
    add_trace(y = ~max(sapply(words, length)), name = paste('Max: ', max(sapply(words, length))), line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
    layout(title = paste(part.of.whole,'in Sentences'),
           xaxis = list(title = 'sentence'),
           yaxis = list (title = paste('number of',part.of.whole)))
  print(total.words)
  
  nwords.byEmot <- plot_ly(ggplot2::diamonds, y = sapply(words, length), color = emots, type = "box")
  print(tapply(sapply(words, length), emots, summary))
  print(nwords.byEmot)
}


# WORDCLOUD : MOST FREC WORDS
generate.wordcloud <- function(sentences, data.title = "Most Used Words"){
  
  # FRECUENCY MATRIX OF WORDS
  data.docs <- Corpus(VectorSource(sentences))
  dtm <- TermDocumentMatrix(data.docs)
  data.freq <- as.matrix(dtm)
  data.freq <- sort(rowSums(data.freq),decreasing=TRUE)
  data.freq <- data.frame(word = names(data.freq),freq=data.freq)
  
  # GENERATE WORDCLOUD
  
  par(bg="grey30")
  png(file="WordCloud.png",width=1000,height=700, bg="grey30")
  write("Generating wordCloud .....", stdout())
  wordcloud(data.freq$word, data.freq$freq, col=terrain.colors(length(data.freq$word), alpha=0.9), random.order=FALSE, rot.per=0.3 )
  write("Done, Saved on Working Directory", stdout())
  title(main = data.title, font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
  dev.off()
}


# COMPARATIVE ANALYSIS: DATA VS PRE.PROC.DATA
compare.data <- function(dataX, dataY, Pre.dataX, Pre.dataY){
  
  isear.data <- isear.data[-pos,]
  # WORD IN SENTENCES
  words.data <- tokenize_words(dataX)
  words.Pre.data <- tokenize_words(Pre.dataX)
  
  pos <- which(sapply(words.data, length) == 1)
  
  words.comp <- plot_ly(x = rep(1:length(words.data[-pos])), y = sapply(words.data[-pos], length), type = "scatter", mode = "lines", name = 'Normal Data') %>%
    add_trace(y = sapply(words.Pre.data, length), name = 'PreProc Data') %>%
    add_trace(y = ~mean(sapply(words.data[-pos], length)), name = paste('Normal Mean: ', mean(sapply(words.data[-pos], length))), line = list(color = 'rgb(12, 105, 24)', width = 2, dash = 'dash')) %>%
    add_trace(y = ~min(sapply(words.data[-pos], length)), name = paste('Normal Min: ', min(sapply(words.data[-pos], length))), line = list(color = 'rgb(12, 105, 24)', width = 2, dash = 'dot')) %>%
    add_trace(y = ~max(sapply(words.data[-pos], length)), name = paste('Normal Max: ', max(sapply(words.data[-pos], length))), line = list(color = 'rgb(12, 105, 24)', width = 2)) %>%
    add_trace(y = ~mean(sapply(words.Pre.data, length)), name = paste('PreProc Mean: ', mean(sapply(words.Pre.data, length))), line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dash')) %>%
    add_trace(y = ~min(sapply(words.Pre.data, length)), name = paste('PreProc Min: ', min(sapply(words.Pre.data, length))), line = list(color = 'rgb(205, 12, 24)', width = 2, dash = 'dot')) %>%
    add_trace(y = ~max(sapply(words.Pre.data, length)), name = paste('PreProc Max: ', max(sapply(words.Pre.data, length))), line = list(color = 'rgb(205, 12, 24)', width = 2)) %>%
    layout(yaxis = list(title = 'Number of Words'), barmode = 'group')
  print(words.comp)
  
  # WORDS BY EMOTIONS
  words.comp.byEmot <- plot_ly(ggplot2::diamonds, x = Pre.dataY, y = sapply(words.data[-pos], length), type = "box", name = "Normal") %>%
    add_trace(ggplot2::diamonds, x = Pre.dataY, y = sapply(words.Pre.data, length), type = "box", name = "PreProc") %>%
    layout(boxmode = "group")
  print(words.comp.byEmot)
  
}
