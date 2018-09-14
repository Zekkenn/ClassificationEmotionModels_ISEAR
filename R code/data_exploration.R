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
  p <- plot_ly(test_frame, x = test_frame$test, y = test_frame$Freq, type = 'bar') %>%
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

