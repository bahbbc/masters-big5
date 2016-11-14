library(tm)
library(ggplot2)


corpus <- read.csv('~/workspace/masters/post/17-vocabulary.csv', header = TRUE, encoding="latin1", stringsAsFactors=FALSE, sep = ';')

corpus <- rep(corpus$word, corpus$frequency)

c <- Corpus(VectorSource(corpus))

c <- tm_map(c, stemDocument, language = "portuguese")

docs <- tm_map(c, PlainTextDocument)

dtm <- DocumentTermMatrix(docs)

freq <- colSums(as.matrix(dtm))
length(freq)

ord <- order(freq)

term_freq <- subset((freq), freq >= 2)

df <- data.frame(term = names(term_freq), freq = term_freq)
df <- df[order(term_freq),]

ggplot(df, aes(x = df$term, y = df$freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()