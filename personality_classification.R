library(caret)
library(e1071)
library(gbm)
set.seed(78)

personalities <- read.csv('personality-normalized.csv')
personalities <- sapply(personalities, function(x) as.numeric(as.matrix(x)))
personalities <- as.data.frame(personalities)

# remove texts with few words

new_personalities <- personalities[personalities$items > 780,]

create_partition <- function(personality){
  training_data_rows <- createDataPartition(new_personalities[,personality], p = .7, list = FALSE)
  train_data <- new_personalities[training_data_rows,]
  test_data <- new_personalities[-training_data_rows,]
  list(train_data, test_data)
}

create_ober_partition <- function(personality){
  op_1 <- new_personalities[new_personalities[personality] == 1,]
  op_2 <- new_personalities[new_personalities[personality] == 0,]
  
  new_personalities <- rbind(op_1, op_2)
  new_personalities <- new_personalities[sample(1:nrow(new_personalities)),]
  
  training_data_rows <- createDataPartition(new_personalities[,personality], p = .7, list = FALSE)
  train_data <- new_personalities[training_data_rows,]
  test_data <- new_personalities[-training_data_rows,]
  list(train_data, test_data)
}

train_gbm <- function(personality, attributes, train_data, test_data){
  gbm_model <- train(pre_proc_func_gbm, data = train_data, method = "gbm", trControl = trainControl(method = "repeatedcv", number = 4, repeats = 4), verbose = FALSE)
  truth_table <- table(gbm_model, test_data[personality])
  
  precision <- truth_table[4]/(truth_table[4] + truth_table[3])
  recall <- truth_table[4]/(truth_table[4] + truth_table[2])
  accuracy <- (truth_table[1] + truth_table[4])/(truth_table[1] + truth_table[2] + truth_table[3] + truth_table[4])
  f_measure <- (precision * recall)/(precision + recall)
  print('-----GBM-------')
  print(truth_table)
  print(paste("precision", precision, 'recall', recall, 'accuracy', accuracy, 'f-measure', f_measure))
  print('--------------')
}

train_svm <- function(personality, attributes, train_data, test_data){
  svm_model <- svm(attributes, data = train_data)
  truth_table <- table(predict(svm_model, test_data), test_data[,personality])
  
  precision <- truth_table[4]/(truth_table[4] + truth_table[3])
  recall <- truth_table[4]/(truth_table[4] + truth_table[2])
  accuracy <- (truth_table[1] + truth_table[4])/(truth_table[1] + truth_table[2] + truth_table[3] + truth_table[4])
  f_measure <- (precision * recall)/(precision + recall)
  print('-----SVM-------')
  print(truth_table)
  print(paste("precision", precision, 'recall', recall, 'accuracy', accuracy, 'f-measure', f_measure))
  print('--------------')
}

### EXTRAVERSION

data_set <- create_partition('extraversion_m')

### PRE PROC
print('PRE PROC')
attributes <- (as.factor(extraversion_m) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('extraversion_m', attributes, data_set[[1]], data_set[[2]])

### GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(extraversion_m) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
train_svm('extraversion_m', attributes, data_set[[1]], data_set[[2]])

### LIWC
print('LIWC')
attributes <- as.factor(extraversion_m) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
train_svm('extraversion_m', attributes, data_set[[1]], data_set[[2]])

### ALL
print('ALL')
attributes <- (as.factor(extraversion_m) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('extraversion_m', attributes, data_set[[1]], data_set[[2]])

##############################################

### AGREEABLENESS

data_set <- create_partition('agreeabeness_m')

### PRE PROC
print('PRE PROC')
attributes <- (as.factor(agreeabeness_m) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('agreeabeness_m', attributes, data_set[[1]], data_set[[2]])

### GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(agreeabeness_m) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
train_svm('agreeabeness_m', attributes, data_set[[1]], data_set[[2]])

### LIWC
print('LIWC')
attributes <- as.factor(agreeabeness_m) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
train_svm('agreeabeness_m', attributes, data_set[[1]], data_set[[2]])

### ALL
print('ALL')
attributes <- (as.factor(agreeabeness_m) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('agreeabeness_m', attributes, data_set[[1]], data_set[[2]])

############################

#CONSCIENTIOUSNESS

data_set <- create_partition('conscientiousness_m')

#SOCIAL
attributes <- (as.factor(conscientiousness_m) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('conscientiousness_m', attributes, data_set[[1]], data_set[[2]])

### GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(conscientiousness_m) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
train_svm('conscientiousness_m', attributes, data_set[[1]], data_set[[2]])

### LIWC
print('LIWC')
attributes <- as.factor(conscientiousness_m) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
train_svm('conscientiousness_m', attributes, data_set[[1]], data_set[[2]])

#ALL
print('ALL')
attributes <- (as.factor(conscientiousness_m) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('conscientiousness_m', attributes, data_set[[1]], data_set[[2]])

############################################

data_set <- create_partition('neuroticism_m')

# NEUROTICISM

#SOCIAL
attributes <- (as.factor(neuroticism_m) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('neuroticism_m', attributes, data_set[[1]], data_set[[2]])

#GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(neuroticism_m) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
train_svm('neuroticism_m', attributes, data_set[[1]], data_set[[2]])

# LIWC
print('LIWC')
attributes <- as.factor(neuroticism_m) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
train_svm('neuroticism_m', attributes, data_set[[1]], data_set[[2]])

#ALL
print('ALL')
attributes <- (as.factor(neuroticism_m) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('neuroticism_m', attributes, data_set[[1]], data_set[[2]])

################

# OPENESS

data_set <- create_partition('openness_m')

#SOCIAL
attributes <- (as.factor(openness_m) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('openness_m', attributes, data_set[[1]], data_set[[2]])

#GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(openness_m) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
train_svm('openness_m', attributes, data_set[[1]], data_set[[2]])

### LIWC
print('LIWC')
attributes <- as.factor(openness_m) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
train_svm('openness_m', attributes, data_set[[1]], data_set[[2]])

#ALL
print('ALL')
attributes <- (as.factor(neuroticism_m) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('neuroticism_m', attributes, data_set[[1]], data_set[[2]])

########################################
########################################
########################################

### EXTRAVERSION

data_set <- create_ober_partition('extraversion_ober_2')

### PRE PROC
print('PRE PROC')
attributes <- (as.factor(extraversion_ober_2) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('extraversion_ober_2', attributes, data_set[[1]], data_set[[2]])

### GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(extraversion_ober_2) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
train_svm('extraversion_ober_2', attributes, data_set[[1]], data_set[[2]])

### LIWC
print('LIWC')
attributes <- as.factor(extraversion_ober_2) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
train_svm('extraversion_ober_2', attributes, data_set[[1]], data_set[[2]])

### ALL
print('ALL')
attributes <- (as.factor(extraversion_ober_2) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('extraversion_ober_2', attributes, data_set[[1]], data_set[[2]])

##############################################

### AGREEABLENESS

data_set <- create_ober_partition('agreeableness_ober_2')

### PRE PROC
print('PRE PROC')
attributes <- (as.factor(agreeableness_ober_2) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('agreeableness_ober_2', attributes, data_set[[1]], data_set[[2]])

### GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(agreeableness_ober_2) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
train_svm('agreeableness_ober_2', attributes, data_set[[1]], data_set[[2]])

### LIWC
print('LIWC')
attributes <- as.factor(agreeableness_ober_2) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
train_svm('agreeableness_ober_2', attributes, data_set[[1]], data_set[[2]])

### ALL
print('ALL')
attributes <- (as.factor(agreeableness_ober_2) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('agreeableness_ober_2', attributes, data_set[[1]], data_set[[2]])

############################

#CONSCIENTIOUSNESS

data_set <- create_ober_partition('conscientiousness_ober_2')

### PRE PROC
print('PRE PROC')
attributes <- (as.factor(conscientiousness_ober_2) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('conscientiousness_ober_2', attributes, data_set[[1]], data_set[[2]])

### GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(conscientiousness_ober_2) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
train_svm('conscientiousness_ober_2', attributes, data_set[[1]], data_set[[2]])

### LIWC
print('LIWC')
attributes <- as.factor(conscientiousness_ober_2) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
train_svm('conscientiousness_ober_2', attributes, data_set[[1]], data_set[[2]])

### ALL
print('ALL')
attributes <- (as.factor(conscientiousness_ober_2) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('conscientiousness_ober_2', attributes, data_set[[1]], data_set[[2]])

############################################

# NEUROTICISM

data_set <- create_ober_partition('neuroticism_ober_2')

### PRE PROC
print('PRE PROC')
attributes <- (as.factor(neuroticism_ober_2) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('neuroticism_ober_2', attributes, data_set[[1]], data_set[[2]])

### GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(neuroticism_ober_2) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
train_svm('neuroticism_ober_2', attributes, data_set[[1]], data_set[[2]])

### LIWC
print('LIWC')
attributes <- as.factor(neuroticism_ober_2) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
train_svm('neuroticism_ober_2', attributes, data_set[[1]], data_set[[2]])

### ALL
print('ALL')
attributes <- (as.factor(neuroticism_ober_2) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('neuroticism_ober_2', attributes, data_set[[1]], data_set[[2]])

################

# OPENESS

data_set <- create_ober_partition('openness_ober_2')

### PRE PROC
print('PRE PROC')
attributes <- (as.factor(openness_ober_2) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('openness_ober_2', attributes, data_set[[1]], data_set[[2]])

### GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(openness_ober_2) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
train_svm('openness_ober_2', attributes, data_set[[1]], data_set[[2]])

### LIWC
print('LIWC')
attributes <- as.factor(openness_ober_2) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
train_svm('openness_ober_2', attributes, data_set[[1]], data_set[[2]])

### ALL
print('ALL')
attributes <- (as.factor(openness_ober_2) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
train_svm('openness_ober_2', attributes, data_set[[1]], data_set[[2]])

############################