# Normalize all texts from files to one single file with the texts and the personalities

tokenizing <- function(original_files, concatenated_files){
  # if the merged dataset does exist, append to it
  for (file in original_files){
    text <-readLines(file, encoding="latin1")
    # Save file_name to match personality
    file_name <- as.integer(text[1])
    
    # generate one whole string with the text
    formatted_text = paste(unlist(text), collapse=' ')
    # remove numbers
    formatted_text = gsub("[0-9]", " ", formatted_text)
    # Remove '"'
    formatted_text = gsub("\"", " ", formatted_text)
    
    # if there is no facebook data, there will be no file name, skip to the next
    if (is.na(file_name)){
      next
    }
    final_text <- cbind(file_name, formatted_text)
    
    concatenated_files <- rbind(concatenated_files, final_text)
  }
  concatenated_files
}

folders = list.dirs('/home/bahbbc/Documents/b5-post(confidencial)/normalised')

file_names = list.files(folders, full.names=TRUE, pattern='[0-9]')

tokenized_text <- tokenizing(file_names, data.frame())

personalities <- read.csv('~/workspace/masters-big5/personality-normalized.csv')

text_personality_matrix <- merge(tokenized_text, personalities, by.x = 'file_name', by.y ='id')

write.csv(text_personality_matrix, file = 'personality-normalized-word2vec-norm.csv')