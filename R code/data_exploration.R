library(tidyverse)
library(tokenizers)
library(plotly)
library(plyr)
library(ggplot2)
library(wordcloud)
library(gridExtra)
library(xtable)

# ==========================================================================================
# ============================= EXPLORATORY ANALYSIS =======================================
# ==========================================================================================
emot.colors <- function(amount){
  cols <- c("#93CEBA","#E89B7D","#ADB5CC","#F0A1CE","#B6D077","#FEE571","#E0CBAE","#442220","#31493C","#00CC66")
  return(cols[1:amount])
}

emot.colors.semeval <- function(){
  return(c("#93CEBA","#E89B7D","#ADB5CC","#F0A1CE","#B6D077","#E2792D"))
}

# GENERAL DATA EXPLORATION
#   sentences: chr vector with speeches
#   emotions: factor with emotion label for every sentence
explore.data <- function(sentences,emotions){
  
  #COMMON PALETTE
  emot.palette <- emot.colors(length(levels(emotions)))
  if (length(levels(emotions))){emot.palette <- emot.colors.semeval()}
  
  # EMOTION FRECUENCIES
  visualize_data(emotions,emot.palette)
  
  # ANALYSIS OF WORDS ON SENTENCES
  write("WORDS ON SENTENCES",stdout())
  words <- tokenize_words(sentences)
  visualize.nwords(words,emotions,emot.palette,"words")
  
  # ANALYSIS OF PHRASES ON SENTENCES
  write("PHRASES ON SENTENCES",stdout())
  phrases <- tokenize_sentences(sentences)
  visualize.nwords(phrases,emotions,emot.palette,"phrases")
  
  # WORDCLOUD
  generate.wordcloud(sentences,emotions,emot.palette)
}


# HISTOGRAM OF EMOTIONS FRECUENCIES 
#   Receives a vector labels... [joy, joy, fear, joy, anger, ...]
visualize_data <- function(emots,emot.palette){
  ## Saving Parameters

  freq <- round(as.vector(summary(emots)) / sum(as.vector(summary(emots))), 3)
  cum_freq <- cumsum(freq)
  
  # New margins
  def_par <- par()
  par(mar=c(5,5,4,5)) 
  
  # plot bars, pc will hold x values for bars
  pc = barplot(as.vector(summary(emots)),
               width = 1, space = 0.2, border = NA, axes = F,
               ylim = c(0, 1.05 * max(as.vector(summary(emots)), na.rm = T)), 
               ylab = "Counts" , cex.names = 0.7, 
               names.arg = levels(emots),
               main = "Distribucción de Emociones",
               col = emot.palette)
  
  # anotate left axis
  axis(side = 2, at = c(0, as.vector(summary(emots))), las = 1, col.axis = "grey62", col = "grey62", tick = T, cex.axis = 0.8)
  
  # frame plot
  box( col = "grey62")
  
  # Cumulative Frequency Lines 
  px <- cum_freq * max(as.vector(summary(emots)), na.rm = T)
  lines(pc, px, type = "b", cex = 0.7, pch = 19, col="cyan4")
  
  # Annotate Right Axis
  axis(side = 4, at = c(0, px), labels = paste(c(0, round(cum_freq * 100)) ,"%",sep=""), 
       las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.8, col.axis = "cyan4")
  
  # restoring default paramenter
  par(def_par) 
}


# SCATTER OF WORDS OR PHRASES IN PHRASES
visualize.nwords <- function(words,emots,emot.palette,part.of.whole = "words"){
  data.words <- data.frame(n.words = sapply(words, length), emot = emots)

  # Plot 1: Density plot with transparency (using the alpha argument)
  dens.words <- ggplot(data=data.words,aes(x=n.words, group=emot, fill=emot)) + 
    geom_density(adjust=1.5 , alpha=0.5) +
    scale_fill_manual(values=emot.palette) +
    xlab(part.of.whole)
  total.dens.words <- ggplot(data=data.words,aes(x=n.words, group=emot, fill=emot)) + 
    geom_density(adjust=1.5 , alpha=0.5) +
    scale_fill_manual(values=emot.palette) +
    xlim(0,summary(data.words$n.words)[[5]]) +
    xlab(part.of.whole) +
    theme(legend.position="none")
  
  print(grid.arrange(dens.words, total.dens.words, ncol=2))

  # Plot 2: General Frecuency plot
  total.words <- plot_ly(x = rep(1:length(words)), y = data.words$n.words, name = "Word counts", type = "scatter", mode = "lines", line = list(color = 'rgb(45,178,115)')) %>%
    add_trace(y = ~mean(sapply(words, length)), name = paste('Mean: ', mean(sapply(words, length))), line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash')) %>%
    add_trace(y = ~min(sapply(words, length)), name = paste('Min: ', min(sapply(words, length))), line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dot')) %>%
    add_trace(y = ~max(sapply(words, length)), name = paste('Max: ', max(sapply(words, length))), line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%
    layout(title = paste(part.of.whole,'in Sentences'),
           xaxis = list(title = 'sentence'),
           yaxis = list (title = paste('number of',part.of.whole)))
  print(total.words)
  
  MLBRPGplot <- ggplot(data.words, aes(x=rep(1:length(words)), y=data.words$n.words)) +
    geom_point(color='#2DB273') +
    theme_bw() +
    theme(panel.grid.minor = element_line(colour="gray95")) +
    xlab("sentence") +
    ylab(paste('number of',part.of.whole)) +
    geom_smooth(span = 0.25, color = '#343432') +
    ggtitle(paste(paste('Number of',part.of.whole), "per Sentence")) +
    theme(plot.title = element_text(hjust=0, size=16))
  print(MLBRPGplot)
  
  # Plot 3: Box plot
  sum.words <- factor(data.words$emot)
  sum.words <- mapvalues(sum.words, from = levels(sum.words), to = rep("Stack",length(levels(sum.words))))

  sum.total <- data.frame(n.words = data.words$n.words, emot = sum.words)
  words.stacked <- rbind(data.words, sum.total)
  nwords.byEmot <- plot_ly(y = words.stacked$n.words, color = words.stacked$emot, type = "box", colors = c(emot.palette,"#6D6C74"))
  print(nwords.byEmot)
}


# WORDCLOUD : MOST FREC WORDS
generate.wordcloud <- function(sentences, emots, emot.palette, data.title = "Most Used Words"){
  data.sent <- data.frame(sentences = sentences, emot = emots)
  
  # FRECUENCY MATRIX OF WORDS
  data.docs <- Corpus(VectorSource(sentences))
  dtm <- TermDocumentMatrix(data.docs)
  data.freq <- as.matrix(dtm)
  colnames(data.freq) <- emots
  
  combine.byEmot <- function(e){apply(data.freq[,which(emots == e)], 1, sum)}
  
  combine.data.freq <- sapply(levels(emots), combine.byEmot) %>% as.matrix()
  
  #data.freq <- sort(rowSums(data.freq),decreasing=TRUE)
  #data.freq <- data.frame(word = names(data.freq),freq=data.freq)
  
  # GENERATE WORDCLOUD
  
  png("comparison_cloud_top_500_words.png", width = 480, height = 480)
  comparison.cloud(combine.data.freq,max.words=500,random.order=FALSE,title.size = 1.5, colors = emot.palette)
  dev.off()
  
  #par(bg="grey30")
  #png(file="WordCloud.png",width=1000,height=700, bg="grey30")
  #write("Generating wordCloud .....", stdout())
  #wordcloud(data.freq$word, data.freq$freq, col=terrain.colors(length(data.freq$word), alpha=0.9), random.order=FALSE, rot.per=0.3 )
  #write("Done, Saved on Working Directory", stdout())
  #title(main = data.title, font.main = 1, col.main = "cornsilk3", cex.main = 1.5)
  #dev.off()
}


# COMPARATIVE ANALYSIS: DATA VS PRE.PROC.DATA
compare.data <- function(dataX, dataY, Pre.dataX, Pre.dataY){
  
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
