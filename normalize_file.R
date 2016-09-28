# read personality CSV
personality = read.csv('Desktop/Coursera/fb-stats-2.csv', header = TRUE, sep = ';')

#word data
word_len = personality$items

# divides an array per word number

per_word <- function(x) x / word_len

#applies to the whole dataset

per_word_data <- as.data.frame(apply(personality, FUN = per_word, MARGIN = 2))

#remove NaN?
not_null <- which(!is.nan(per_word_data$items))
user_ids <- c(187, 358, 439, 481, 524, 621, 635, 639, 815, 90)

#leave only numbers
personality_data <- per_word_data[not_null,]
words <- personality$items[not_null]
id <- personality$ID[not_null]
personality_data_frame <- cbind(personality_data, words, id)


write.csv(personality_data_frame, file = 'personality-normalized.csv')


#traits normalization

traits = read.csv('Desktop/Coursera/personality-normalized.csv', header = TRUE, sep = ',')

divide_by_mean <- function(personality) {
  m <- mean(personality)
  personality_1 <- which(personality > m)
  personality_norm <- rep(0, length(personality))
  personality_norm[personality_1] = 1
  personality_norm
}

extraversion_m <- divide_by_mean(traits$extraversion)
agreeabeness_m <- divide_by_mean(traits$agreableness)
conscientiousness_m <- divide_by_mean(traits$conscientiousness)
neuroticism_m <- divide_by_mean(traits$neuroticism)
openness_m <- divide_by_mean(traits$openness)

divide_by_3 <- function(personality) {
  personality_0 <-which(personality < 3)
  personality_1 <-which(personality >= 3 & personality < 4)
  personality_2 <-which(personality >= 4)
  personality_norm <- rep(0, length(personality))
  personality_norm[personality_1] = 1
  personality_norm[personality_2] = 2
  personality_norm
}

extraversion_3 <- divide_by_3(traits$extraversion)
agreeableness_3 <- divide_by_3(traits$agreableness)
conscientiousness_3 <- divide_by_3(traits$conscientiousness)
neuroticism_3 <- divide_by_3(traits$neuroticism)
openness_3 <- divide_by_3(traits$openness)

divide_by_binary_sd <- function(personality){
  sd_per <- sd(personality)
  mean_per <- mean(personality)
  personality_1 <-which(personality < mean_per-sd_per)
  personality_2 <-which(personality > mean_per+sd_per)
  personality_norm <- rep(0, length(personality))
  personality_norm[personality_1] = -1
  personality_norm[personality_2] = 1
  personality_norm
}

extraversion_ober_2 <- divide_by_binary_sd(traits$extraversion)
agreeableness_ober_2 <- divide_by_binary_sd(traits$agreableness)
conscientiousness_ober_2 <- divide_by_binary_sd(traits$conscientiousness)
neuroticism_ober_2 <- divide_by_binary_sd(traits$neuroticism)
openness_ober_2 <- divide_by_binary_sd(traits$openness)


personality_data_frame <- cbind(traits, extraversion_m, agreeabeness_m, conscientiousness_m,
  neuroticism_m, openness_m, extraversion_3, agreeableness_3, conscientiousness_3, neuroticism_3, openness_3,
  extraversion_ober_2, agreeableness_ober_2, conscientiousness_ober_2, neuroticism_ober_2, openness_ober_2)

write.csv(personality_data_frame, file = 'personality-normalized-3.csv')

#concat all col_names with per_word
paste(names(personality[1]), 'per_word', sep='_')

#:) Bah fofinha s2
