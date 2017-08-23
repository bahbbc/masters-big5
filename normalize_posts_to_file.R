# Normalize all texts from files to one single file with the texts and the personalities

tokenizing <- function(original_files, concatenated_files){
  # if the merged dataset does exist, append to it
  for (file in original_files){
    text <-readLines(file, encoding="utf-8")
    # Save file_name to match personality
    file_name <- gsub("/home/bahbbc/Documents/post-utf8/normalised-v2/post-speaker-", "", file)
    #file_name <- gsub("/home/bahbbc/Documents/post-utf8/lemmatised/lemma-post-speaker-", "", file)
    #file_name <- gsub("/home/bahbbc/Documents/post-utf8/normalised-v2/post-speaker-", "", file)
    file_name <- gsub("-normalised.txt", "", file_name)
    
    # generate one whole string with the text
    formatted_text = paste(unlist(text), collapse=' ')
    # remove numbers
    formatted_text = gsub("[0-9]", " ", formatted_text)
    # Remove '"'
    formatted_text = gsub("\"", " ", formatted_text)
    
    if (is.na(file_name)){
      print('--')
      next
    }
    
    final_text <- cbind(file_name, formatted_text)
    
    concatenated_files <- rbind(concatenated_files, final_text)
  }
  concatenated_files
}

folders = list.dirs('/home/bahbbc/Documents/post-utf8/normalised-v2/')

file_names = list.files(folders, full.names=TRUE, pattern='[0-9]')

tokenized_text <- tokenizing(file_names, data.frame())

personalities <- read.csv('~/workspace/masters-big5/personality-normalized.csv')
  
text_personality_matrix <- merge(tokenized_text, personalities, by.x = 'file_name', by.y ='id')

write.csv(text_personality_matrix, file = 'personality-normalized-v2-word2vec.csv')