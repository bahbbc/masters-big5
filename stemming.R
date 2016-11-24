library(tm)
library(ggplot2)

personalities <- read.csv('~/workspace/masters/final_normalized_file.csv')

# verificar como esse codigo funciona para importar todos os arquivos de uma pasta
c <- Corpus(DirSource('~/workspace/masters/post', encoding="latin1"), readerControl = list(language="portuguese"))
  
c <- tm_map(c, stemDocument, language = "portuguese")
c <- tm_map(c, removeNumbers)
  
#dtm <- DocumentTermMatrix(c)
  
tdm <- DocumentTermMatrix(c)
  
tm_matrix <- as.matrix(tdm)
  
tm_data <- as.data.frame(tm_matrix)
  
file_name <- gsub('/home/barbara/workspace/masters/post/', '', rownames(tm_data))
file_name <- gsub('-vocabulary.csv', '', file_name)
tm_data <- cbind(tm_data, file_name)
  
personalities_tmp <- data.frame(id = personalities$id, extraversion = personalities$extraversion, agreeableness = personalities$agreeableness, conscientiousness = personalities$conscientiousness, neuroticism = personalities$neuroticism, openness = personalities$openness)
  
personality_terms <- merge(personalities_tmp, tm_data, by.x= 'id', by.y = 'file_name')
  
write.csv(personality_terms, 'personality_terms.csv')

ggplot(df, aes(x = df$term, y = df$freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()