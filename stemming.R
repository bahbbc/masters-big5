library(tm)
library(ggplot2)

personalities <- read.csv('final_normalized_file.csv')

generate_word_martrix <- function(files){
  all_words <- data.frame()
  for (file_name in files){
    corpus <- read.csv(file_name, header = TRUE, encoding="latin1", stringsAsFactors=FALSE, sep = ';')
    corpus <- rep(corpus$word, corpus$frequency)
    
    collapsed_text = paste(unlist(corpus), collapse=' ')
    
    file_name <- gsub('/home/barbara/workspace/masters/post/', '', file_name)
    file_name <- gsub('-vocabulary.csv', '', file_name)
    
    concatened_files <- cbind(collapsed_text, file_name)
    
    all_words <- rbind(all_words, concatened_files)
  }
  all_words
}

generate_frequency_matrix <- function(word_matrix){
  c <- Corpus(VectorSource(word_matrix))
  
  c <- tm_map(c, stemDocument, language = "portuguese")
  
  docs <- tm_map(c, PlainTextDocument)
  
  dtm <- DocumentTermMatrix(docs)
  
  freq <- colSums(as.matrix(dtm))
  
  ord <- order(freq)
  
  term_freq <- subset((freq), freq >= 1)
  
  df <- data.frame(term = names(term_freq), freq = term_freq)
  df <- df[order(term_freq),]
}

folders <- list.dirs('~/workspace/masters/post')

file_names <- list.files(folders, full.names=TRUE, pattern='[0-9]')

final_word_matrix <- generate_word_martrix(file_names)

ggplot(df, aes(x = df$term, y = df$freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()