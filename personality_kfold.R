library(caret)
library(e1071)
library(gbm)
set.seed(78)

# necessary functions

fold_measures <- function(dataset, attributes, class){
  set.seed(78)
  data_folds <- createFolds(dataset, 3)
  measure_mean <- data.frame()
  for(i in 1:length(data_folds)){
    fold_test <- unlist(data_folds[-i], use.names = FALSE)
    
    svm_model <- svm(attributes, data = dataset[fold_test,])
    pred <- predict(svm_model, dataset[data_folds[[i]],])
    tb <- table(pred, dataset[data_folds[[i]],][class][,1])
    
    precision <- tb[4]/(tb[4] + tb[3])
    recall <- tb[4]/(tb[4] + tb[2])
    fmeasure <- 2 * ((precision * recall)/(precision + recall))
    accuracy <- (tb[1] + tb[4])/(tb[1] + tb[2] + tb[3] + tb[4])
    metrics <- c('fmeasure' = fmeasure, 'precision' = precision, 'recall' = recall, 'accuracy' = accuracy)
    measure_mean <- rbind(measure_mean, metrics)
  }
  fmeasure <-  mean(measure_mean[,1], na.rm= TRUE)
  precision <- mean(measure_mean[,2])
  recall <- mean(measure_mean[,3])
  accuracy <- mean(measure_mean[,4])
  c(fmeasure = fmeasure, recall = recall, precision = precision, accuracy = accuracy)
}

create_ober_partition <- function(personality){
  op_1 <- new_personalities[new_personalities[personality] == 1,]
  op_2 <- new_personalities[new_personalities[personality] == 0,]
  
  new_personalities <- rbind(op_1, op_2)
  new_personalities <- new_personalities[sample(1:nrow(new_personalities)),]
  new_personalities
}

personalities <- read.csv('personality-normalized.csv')
personalities <- sapply(personalities, function(x) as.numeric(as.matrix(x)))
personalities <- as.data.frame(personalities)


# remove people with less than 1st quartile words
new_personalities <- personalities[personalities$items > 780,]

# remove NAs

# new_personalities <- personalities[!is.na(personalities$exclam),]

### EXTRAVERSION

### PRE PROC
print('PRE PROC')
attributes <- (as.factor(extraversion_m) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(new_personalities, attributes, 'extraversion_m')

### GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(extraversion_m) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
fold_measures(new_personalities, attributes, 'extraversion_m')

### LIWC
print('LIWC')
attributes <- as.factor(extraversion_m) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
fold_measures(new_personalities, attributes, 'extraversion_m')

### ALL
print('ALL')
attributes <- (as.factor(extraversion_m) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(new_personalities, attributes, 'extraversion_m')

##############################################

### AGREEABLENESS

### PRE PROC
print('PRE PROC')
attributes <- (as.factor(agreeabeness_m) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(new_personalities, attributes, 'agreeabeness_m')

### GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(agreeabeness_m) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
fold_measures(new_personalities, attributes, 'agreeabeness_m')

### LIWC
print('LIWC')
attributes <- as.factor(agreeabeness_m) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
fold_measures(new_personalities, attributes, 'agreeabeness_m')

### ALL
print('ALL')
attributes <- (as.factor(agreeabeness_m) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(new_personalities, attributes, 'agreeabeness_m')

############################

#CONSCIENTIOUSNESS

#SOCIAL
attributes <- (as.factor(conscientiousness_m) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(new_personalities, attributes, 'conscientiousness_m')

### GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(conscientiousness_m) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
fold_measures(new_personalities, attributes, 'conscientiousness_m')

### LIWC
print('LIWC')
attributes <- as.factor(conscientiousness_m) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
fold_measures(new_personalities, attributes, 'conscientiousness_m')

#ALL
print('ALL')
attributes <- (as.factor(conscientiousness_m) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(new_personalities, attributes, 'conscientiousness_m')

############################################

# NEUROTICISM

#SOCIAL
attributes <- (as.factor(neuroticism_m) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(new_personalities, attributes, 'neuroticism_m')

#GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(neuroticism_m) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
fold_measures(new_personalities, attributes, 'neuroticism_m')

# LIWC
print('LIWC')
attributes <- as.factor(neuroticism_m) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
fold_measures(new_personalities, attributes, 'neuroticism_m')

#ALL
print('ALL')
attributes <- (as.factor(neuroticism_m) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(new_personalities, attributes, 'neuroticism_m')

################

# OPENESS

#SOCIAL
attributes <- (as.factor(openness_m) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(new_personalities, attributes, 'openness_m')

#GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(openness_m) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
fold_measures(new_personalities, attributes, 'openness_m')

### LIWC
print('LIWC')
attributes <- as.factor(openness_m) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
fold_measures(new_personalities, attributes, 'openness_m')

#ALL
print('ALL')
attributes <- (as.factor(neuroticism_m) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(new_personalities, attributes, 'openness_m')

########################################
########################################
########################################

### EXTRAVERSION

data_set <- create_ober_partition('extraversion_ober_2')

### PRE PROC
print('PRE PROC')
attributes <- (as.factor(extraversion_ober_2) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(data_set, attributes, 'extraversion_ober_2')

### GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(extraversion_ober_2) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
fold_measures(data_set, attributes, 'extraversion_ober_2')

### LIWC
print('LIWC')
attributes <- as.factor(extraversion_ober_2) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
fold_measures(data_set, attributes, 'extraversion_ober_2')

### ALL
print('ALL')
attributes <- (as.factor(extraversion_ober_2) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(data_set, attributes, 'extraversion_ober_2')

##############################################

### AGREEABLENESS

data_set <- create_ober_partition('agreeableness_ober_2')

### PRE PROC
print('PRE PROC')
attributes <- (as.factor(agreeableness_ober_2) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(data_set, attributes, 'agreeableness_ober_2')

### GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(agreeableness_ober_2) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
fold_measures(data_set, attributes, 'agreeableness_ober_2')

### LIWC
print('LIWC')
attributes <- as.factor(agreeableness_ober_2) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
fold_measures(data_set, attributes, 'agreeableness_ober_2')

### ALL
print('ALL')
attributes <- (as.factor(agreeableness_ober_2) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(data_set, attributes, 'agreeableness_ober_2')

############################

#CONSCIENTIOUSNESS

data_set <- create_ober_partition('conscientiousness_ober_2')

### PRE PROC
print('PRE PROC')
attributes <- (as.factor(conscientiousness_ober_2) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(data_set, attributes, 'conscientiousness_ober_2')

### GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(conscientiousness_ober_2) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
fold_measures(data_set, attributes, 'conscientiousness_ober_2')

### LIWC
print('LIWC')
attributes <- as.factor(conscientiousness_ober_2) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
fold_measures(data_set, attributes, 'conscientiousness_ober_2')

### ALL
print('ALL')
attributes <- (as.factor(conscientiousness_ober_2) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(data_set, attributes, 'conscientiousness_ober_2')

############################################

# NEUROTICISM

data_set <- create_ober_partition('neuroticism_ober_2')

### PRE PROC
print('PRE PROC')
attributes <- (as.factor(neuroticism_ober_2) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(data_set, attributes, 'neuroticism_ober_2')

### GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(neuroticism_ober_2) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
fold_measures(data_set, attributes, 'neuroticism_ober_2')

### LIWC
print('LIWC')
attributes <- as.factor(neuroticism_ober_2) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
fold_measures(data_set, attributes, 'neuroticism_ober_2')

### ALL
print('ALL')
attributes <- (as.factor(neuroticism_ober_2) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(data_set, attributes, 'neuroticism_ober_2')

################

# OPENESS

data_set <- create_ober_partition('openness_ober_2')

### PRE PROC
print('PRE PROC')
attributes <- (as.factor(openness_ober_2) ~ skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(data_set, attributes, 'openness_ober_2')

### GRAMMAR
print('GRAMMAR')
attributes <- (as.factor(openness_ober_2) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)
fold_measures(data_set, attributes, 'openness_ober_2')

### LIWC
print('LIWC')
attributes <- as.factor(openness_ober_2) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler 
fold_measures(data_set, attributes, 'openness_ober_2')

### ALL
print('ALL')
attributes <- (as.factor(openness_ober_2) ~ sentences + items + chars + allTokens + wordTokens +  m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
fold_measures(data_set, attributes, 'openness_ober_2')

############################
