library(caret)
library(e1071)
library(gbm)

personalities <- read.csv('personality-normalized.csv')

new_personalities <- personalities[personalities$items > 780,]

#EXTRAVERSION

# divide personalities by  (mean + sd) or (mean - sd)

op_2_1 <- new_personalities[new_personalities$extraversion_ober_2 == 0,]
op_2_2 = new_personalities[new_personalities$extraversion_ober_2 == 1,]

extra_sorted_o <- rbind(op_2_1, op_2_2)
extra_sorted_o <- extra_sorted_o[sample(1:nrow(extra_sorted_o)),]

extra_sorted_o <- sapply(extra_sorted_o, function(x) as.numeric(as.matrix(x)))
extra_sorted_o <- as.data.frame(extra_sorted_o)

pre_proc_attr <- extra_sorted_o[3:33]

correlationMatrix <- cor(pre_proc_attr)
print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
print(highlyCorrelated)

###########

op_1 <- new_personalities[new_personalities$conscientiousness_m == 1,]
op_2 <- new_personalities[new_personalities$conscientiousness_m == 0,]

# divide personalities by  (mean + sd) or (mean - sd)

op_2_1 <- new_personalities[new_personalities$conscientiousness_ober_2 == -1,]
op_2_2 = new_personalities[new_personalities$conscientiousness_ober_2 == 1,]

#fazer um shuffle com esses dados

extra_sorted_m <- rbind(op_1, op_2)
extra_sorted_o <- rbind(op_2_1, op_2_2)

set.seed(78)
training_data_rows <- createDataPartition(extra_sorted_m$conscientiousness_m, p = .8, list = FALSE)
train_data <- extra_sorted_m[training_data_rows,]
test_data <- extra_sorted_m[-training_data_rows,]

############



# separate train and test sets

set.seed(78)
training_data_rows <- createDataPartition(extra_sorted_o$extraversion_ober_2, p = .7, list = FALSE)
train_data <- extra_sorted_o[training_data_rows,]
test_data <- extra_sorted_o[-training_data_rows,]

control <- rfeControl(functions=rfFuncs, method="cv", number=10)
results <- rfe(extra_sorted_o[3:157], extra_sorted_o$extraversion_ober_2, sizes=c(1:154), rfeControl=control)
print(results)
predictors(results)
plot(results, type=c("g", "o"))

names(extra_sorted_o[3:157])

# SVM 

#PREPROC
pre_proc_func <- (~ extraversion_ober_2 + sentences + items + chars + allTokens + wordTokens + skip+ compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
pre_proc_func_gbm <- (as.factor(extraversion_ober_2) ~ sentences + items + chars + allTokens + wordTokens + skip+ compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)
cor_func <- (~ extraversion_ober_2 + sentences + skip+ compound+ links+ questions+ exclam+ numbers+ upcase+ verb.pro + rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + emph+ echars+ unkn)
rfe_func <- ( ~ extraversion_ober_2 + numbers + links + chars + wordTokens + allTokens + emph + items + exclam + pt.lexicon + en.lexicon + echars + unkn + sentences + rewrite + skip + questions + mispell + firstup + emo. + emo..2 + laugh + verb.pro + emo..1 + lowcase + punct + hashtags + upcase + added + names)
all_filtered <- (as.factor(extraversion_ober_2) ~ X28posemo+X24family+X27affect+proPoss  +numbers  +X8they   +X26humans+chars    +V1p       +links    +X48health+X38certain +p1       +X60relig +X46bio   +f        +wordTokens +X5we      +X4i      +numF     +items    +A        +X23social+VG       +dimin    +dativa   +unkn      +SIGLA    +X25friend+obliqua  +X55work  +X61death +allTokens+X49sexual+PREFIX   +VP        +exclam   +X42percept +X20quant +X56achieve +VK       +upcase   +N        +emph     +X53space  +proPess  +numO     +questions+indef    +NUM      +X10article +skip     +X45feel  +s         +emo.     +X64filler+X52motion+VJ       +X16adverb+X32sad   +X6you    +X19negate+X50ingest +pt.lexicon +X22swear +V2s      +acusativa+X21number+VC       +X2pronoun+X37tentat+X14present+numC     +X11verb  +m        +echars   +DET      +aument   +X18conj  +X59money +X58home   +proTrat  +PRO      +X29negemo+X62assent+proIndef +foreign  +VQ       +VY       +p         +proDem   +X51relativ +X44hear  +def      +X40incl  +emo..1   +X3ppron  +emo..2   +mispell   +reflexa  +VS       +X1funct  +en.lexicon +X33cogmech +sentences+X9ipron  +V3p      +PREP      +V        +X31anger +firstup  +X13past  +numM     +X17preps +p3       +X34insight +hashtags  +lowcase  +nominativa +VI       +X47body  +VF       +X63nonfl +X35cause +X15future+X57leisure+V1s      +p2       +ABREV    +CONJ     +V3s      +superlat +punct    +X43see   +INTERJ    +V2p      +VW       +ADV      +X36discrep +rewrite)





pred_svm_extra_ma_social <- predict(svm_extra_ma_social, test_data)
tb_svm_extra_ma_social <- table(as.numeric(pred_svm_extra_ma_social), test_data$extraversion_ober_2)
F_meas(tb_svm_extra_ma_social)
precision(tb_svm_extra_ma_social)
recall(tb_svm_extra_ma_social)
(tb_svm_extra_ma_social[1] + tb_svm_extra_ma_social[4])/(tb_svm_extra_ma_social[1] + tb_svm_extra_ma_social[2] + tb_svm_extra_ma_social[1] + tb_svm_extra_ma_social[3] + tb_svm_extra_ma_social[1] + tb_svm_extra_ma_social[4])

#GRAMMAR



svm_extra_ma_gram <- svm(grammar_func, data = train_data)
nb_extra_ma_gram <- naiveBayes(grammar_func, data = train_data)
pred_svm_extra_ma_gram <- predict(svm_extra_ma_gram, test_data)
tb_svm_extra_ma_gram <- table(as.numeric(pred_svm_extra_ma_gram), test_data$extraversion_ober_2)
F_meas(tb_svm_extra_ma_gram)
precision(tb_svm_extra_ma_gram)
recall(tb_svm_extra_ma_gram)
(tb_svm_extra_ma_gram[1] + tb_svm_extra_ma_gram[4])/(tb_svm_extra_ma_gram[1] + tb_svm_extra_ma_gram[2] + tb_svm_extra_ma_gram[1] + tb_svm_extra_ma_gram[3] + tb_svm_extra_ma_gram[1] + tb_svm_extra_ma_gram[4])

#LIWC


svm_extra_ma_liwc <- svm(liwc_func, data = train_data)
pred_svm_extra_ma_liwc <- predict(svm_extra_ma_liwc, test_data)
tb_svm_extra_ma_liwc <- table(as.numeric(pred_svm_extra_ma_liwc), test_data$extraversion_ober_2)
F_meas(tb_svm_extra_ma_liwc)
precision(tb_svm_extra_ma_liwc)
recall(tb_svm_extra_ma_liwc)
(tb_svm_extra_ma_liwc[1] + tb_svm_extra_ma_liwc[4])/(tb_svm_extra_ma_liwc[1] + tb_svm_extra_ma_liwc[2] + tb_svm_extra_ma_liwc[1] + tb_svm_extra_ma_liwc[3] + tb_svm_extra_ma_liwc[1] + tb_svm_extra_ma_liwc[4])

#ALL


svm_extra_ma_all <- svm(all_func, data = train_data)
pred_svm_extra_ma_all <- predict(svm_extra_ma_all, test_data)
tb_svm_extra_ma_all <- table(as.numeric(pred_svm_extra_ma_all), test_data$extraversion_ober_2)
F_meas(tb_svm_extra_ma_all)
precision(tb_svm_extra_ma_all)
recall(tb_svm_extra_ma_all)
(tb_svm_extra_ma_all[1] + tb_svm_extra_ma_all[4])/(tb_svm_extra_ma_all[1] + tb_svm_extra_ma_all[2] + tb_svm_extra_ma_all[1] + tb_svm_extra_ma_all[3] + tb_svm_extra_ma_all[1] + tb_svm_extra_ma_all[4])

##############################################

# divide personalities by  (mean + sd) or (mean - sd)

op_2_1 <- new_personalities[new_personalities$agreeableness_ober_2 == 0,]
op_2_2 = new_personalities[new_personalities$agreeableness_ober_2 == 1,]

agrea_sorted_o <- rbind(op_2_1, op_2_2)

# separate train and test sets

training_data_rows <- createDataPartition(agrea_sorted_o$agreeableness_ober_2, p = .8, list = FALSE)
train_data <- agrea_sorted_o[training_data_rows,]
test_data <- agrea_sorted_o[-training_data_rows,]

# AGREABLENESS

#SVM

#PREPROC
pre_proc_func <- (~ agreeableness_ober_2 + skip+ compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)

svm_agrea_ma_social <- svm(pre_proc_func, data = train_data)
pred_svm_agrea_ma_social <- predict(svm_agrea_ma_social, test_data)
tb_svm_agrea_ma_social <- table(as.numeric(pred_svm_agrea_ma_social), test_data$agreeableness_ober_2)
F_meas(tb_svm_agrea_ma_social)
precision(tb_svm_agrea_ma_social)
recall(tb_svm_agrea_ma_social)
(tb_svm_agrea_ma_social[1] + tb_svm_agrea_ma_social[4])/(tb_svm_agrea_ma_social[1] + tb_svm_agrea_ma_social[2] + tb_svm_agrea_ma_social[1] + tb_svm_agrea_ma_social[3] + tb_svm_agrea_ma_social[1] + tb_svm_agrea_ma_social[4])

#GRAMMAR
grammar_func <- (~ agreeableness_ober_2 + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)

svm_agrea_ma_gram <- svm(grammar_func, data = train_data)
pred_svm_agrea_ma_gram <- predict(svm_agrea_ma_gram, test_data)
tb_svm_agrea_ma_gram <- table(as.numeric(pred_svm_agrea_ma_gram), test_data$agreeableness_ober_2)
F_meas(tb_svm_agrea_ma_gram)
precision(tb_svm_agrea_ma_gram)
recall(tb_svm_agrea_ma_gram)
(tb_svm_agrea_ma_gram[1] + tb_svm_agrea_ma_gram[4])/(tb_svm_agrea_ma_gram[1] + tb_svm_agrea_ma_gram[2] + tb_svm_agrea_ma_gram[1] + tb_svm_agrea_ma_gram[3] + tb_svm_agrea_ma_gram[1] + tb_svm_agrea_ma_gram[4])

#LIWC
liwc_func <- (~ agreeableness_ober_2 + X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler)

svm_agrea_ma_liwc <- svm(liwc_func, data = train_data)
pred_svm_agrea_ma_liwc <- predict(svm_agrea_ma_liwc, test_data)
tb_svm_agrea_ma_liwc <- table(as.numeric(pred_svm_agrea_ma_liwc), test_data$agreeableness_ober_2)
F_meas(tb_svm_agrea_ma_liwc)
precision(tb_svm_agrea_ma_liwc)
recall(tb_svm_agrea_ma_liwc)

#ALL
all_func <- (~ agreeableness_ober_2 + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)

svm_agrea_ma_all <- svm(all_func, data = train_data)
pred_svm_agrea_ma_all <- predict(svm_agrea_ma_all, test_data)
tb_svm_agrea_ma_all <- table(as.numeric(pred_svm_agrea_ma_all), test_data$agreeableness_ober_2)
F_meas(tb_svm_agrea_ma_all)
precision(tb_svm_agrea_ma_all)
recall(tb_svm_agrea_ma_all)
(tb_svm_agrea_ma_all[1] + tb_svm_agrea_ma_all[4])/(tb_svm_agrea_ma_all[1] + tb_svm_agrea_ma_all[2] + tb_svm_agrea_ma_all[1] + tb_svm_agrea_ma_all[3] + tb_svm_agrea_ma_all[1] + tb_svm_agrea_ma_all[4])

############################

# divide personalities by  (mean + sd) or (mean - sd)

op_2_1 <- new_personalities[new_personalities$conscientiousness_ober_2 == 0,]
op_2_2 = new_personalities[new_personalities$conscientiousness_ober_2 == 1,]

cons_sorted_o <- rbind(op_2_1, op_2_2)
cons_sorted_o$conscientiousness_ober_2[cons_sorted_o$conscientiousness_ober_2 == -1] <- 0

# separate train and test sets

set.seed(78)
training_data_rows <- createDataPartition(cons_sorted_o$conscientiousness_ober_2, p = .8, list = FALSE)
train_data <- cons_sorted_o[training_data_rows,]
test_data <- cons_sorted_o[-training_data_rows,]

#CONSCIENTIOUSNESS

#PREPROC
pre_proc_func <- (~ conscientiousness_ober_2 + skip+ compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)

svm_cons_ma_social <- svm(pre_proc_func, data = train_data)
pred_svm_cons_ma_social <- predict(svm_cons_ma_social, test_data)
tb_svm_cons_ma_social <- table(as.numeric(pred_svm_cons_ma_social), test_data$conscientiousness_ober_2)
F_meas(tb_svm_cons_ma_social)
precision(tb_svm_cons_ma_social)
recall(tb_svm_cons_ma_social)

#GRAMMAR
grammar_func <- (~ conscientiousness_ober_2 + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)

svm_gram_ma_gram <- svm(grammar_func, data = train_data)
pred_svm_gram_ma_gram <- predict(svm_gram_ma_gram, test_data)
tb_svm_gram_ma_gram <- table(as.numeric(pred_svm_gram_ma_gram), test_data$conscientiousness_ober_2)
F_meas(tb_svm_gram_ma_gram)
precision(tb_svm_gram_ma_gram)
recall(tb_svm_gram_ma_gram)

#LIWC
liwc_func <- (~ conscientiousness_ober_2 + X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler)

svm_cons_ma_liwc <- svm(liwc_func, data = train_data)
pred_svm_cons_ma_liwc <- predict(svm_cons_ma_liwc, test_data)
tb_svm_cons_ma_liwc <- table(as.numeric(pred_svm_cons_ma_liwc), test_data$conscientiousness_ober_2)
F_meas(tb_svm_cons_ma_liwc)
precision(tb_svm_cons_ma_liwc)
recall(tb_svm_cons_ma_liwc)

#ALL
all_func <- (~ conscientiousness_ober_2 + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)

svm_cons_ma_all <- svm(all_func, data = train_data)
pred_svm_cons_ma_all <- predict(svm_cons_ma_all, test_data)
tb_svm_cons_ma_all <- table(as.numeric(pred_svm_cons_ma_all), test_data$conscientiousness_ober_2)
F_meas(tb_svm_cons_ma_all)
precision(tb_svm_cons_ma_all)
recall(tb_svm_cons_ma_all)


##################################

# NEUROTICISM

# divide personalities by  (mean + sd) or (mean - sd)

op_2_1 <- new_personalities[new_personalities$neuroticism_ober_2 == 0,]
op_2_2 = new_personalities[new_personalities$neuroticism_ober_2 == 1,]

neu_sorted_o <- rbind(op_2_1, op_2_2)

# separate train and test sets

set.seed(78)
training_data_rows <- createDataPartition(neu_sorted_o$neuroticism_ober_2, p = .8, list = FALSE)
train_data <- neu_sorted_o[training_data_rows,]
test_data <- neu_sorted_o[-training_data_rows,]


#PREPROC
pre_proc_func <- (~ neuroticism_ober_2 + skip+ compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)

svm_neu_ma_social <- svm(pre_proc_func, data = train_data)
pred_svm_neu_ma_social <- predict(svm_neu_ma_social, test_data)
tb_svm_neu_ma_social <- table(as.numeric(pred_svm_neu_ma_social), test_data$neuroticism_ober_2)
F_meas(tb_svm_neu_ma_social)
precision(tb_svm_neu_ma_social)
recall(tb_svm_neu_ma_social)

#GRAMMAR
grammar_func <- (~ neuroticism_ober_2 + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)

svm_neu_ma_gram <- svm(grammar_func, data = train_data)
pred_svm_neu_ma_gram <- predict(svm_neu_ma_gram, test_data)
tb_svm_neu_ma_gram <- table(as.numeric(pred_svm_neu_ma_gram), test_data$neuroticism_ober_2)
F_meas(tb_svm_neu_ma_gram)
precision(tb_svm_neu_ma_gram)
recall(tb_svm_neu_ma_gram)

#LIWC
liwc_func <- (~ neuroticism_ober_2 + X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler)

svm_neu_ma_liwc <- svm(liwc_func, data = train_data)
pred_svm_neu_ma_liwc <- predict(svm_neu_ma_liwc, test_data)
tb_svm_neu_ma_liwc <-  table(as.numeric(pred_svm_neu_ma_liwc), test_data$neuroticism_ober_2)
F_meas(tb_svm_neu_ma_liwc)
precision(tb_svm_neu_ma_liwc)
recall(tb_svm_neu_ma_liwc)

#ALL
all_func <- (~ neuroticism_ober_2 + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)

svm_neu_ma_all <- svm(all_func, data = train_data)
pred_svm_neu_ma_all <- predict(svm_neu_ma_all, test_data)
tb_svm_neu_ma_all <- table(as.numeric(pred_svm_neu_ma_all), test_data$neuroticism_ober_2)
F_meas(tb_svm_neu_ma_all)
precision(tb_svm_neu_ma_all)
recall(tb_svm_neu_ma_all)
################

# OPENESS

# divide personalities by  (mean + sd) or (mean - sd)

op_2_1 <- new_personalities[new_personalities$openness_ober_2 == 0,]
op_2_2 = new_personalities[new_personalities$openness_ober_2 == 1,]

ober_sorted_o <- rbind(op_2_1, op_2_2)

# separate train and test sets

set.seed(78)
training_data_rows <- createDataPartition(ober_sorted_o$openness_ober_2, p = .8, list = FALSE)
train_data <- ober_sorted_o[training_data_rows,]
test_data <- ober_sorted_o[-training_data_rows,]

#PREPROC
pre_proc_func <- (~ openness_ober_2 + skip+ compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)

svm_open_ma_social <- svm(pre_proc_func, data = train_data)
pred_svm_open_ma_social <- predict(svm_open_ma_social, test_data)
tb_svm_open_ma_social <- table(as.numeric(pred_svm_open_ma_social), test_data$openness_ober_2)
F_meas(tb_svm_open_ma_social)
precision(tb_svm_open_ma_social)
recall(tb_svm_open_ma_social)

#GRAMMAR
grammar_func <- (~ openness_ober_2 + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p)

svm_open_ma_gram <- svm(grammar_func, data = train_data)
pred_svm_open_ma_gram <- predict(svm_open_ma_gram, test_data)
tb_svm_open_ma_gram <- table(as.numeric(pred_svm_open_ma_gram), test_data$openness_ober_2)
F_meas(tb_svm_open_ma_gram)
precision(tb_svm_open_ma_gram)
recall(tb_svm_open_ma_gram)

#LIWC
liwc_func <- (~ openness_ober_2 + X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler)

svm_open_ma_liwc <- svm(liwc_func, data = train_data)
pred_svm_open_ma_liwc <- predict(svm_open_ma_liwc, test_data)
tb_svm_open_ma_liwc <- table(as.numeric(pred_svm_open_ma_liwc), test_data$openness_ober_2)
F_meas(tb_svm_open_ma_liwc)
precision(tb_svm_open_ma_liwc)
recall(tb_svm_open_ma_liwc)

#ALL
all_func <- (~ openness_ober_2 + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p+X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler + skip + compound+ hashtags+ links+ punct + questions+ exclam+ numbers+ upcase+ lowcase+ firstup+ pt.lexicon + added+ verb.pro+ names+ en.lexicon+ rewrite+ mispell+ foreign+ emo. + emo..1 + emo..2 + laugh+ emph+ echars+ unkn)

svm_open_ma_all <- svm(all_func, data = train_data)
pred_svm_open_ma_all <- predict(svm_open_ma_all, test_data)
tb_svm_open_ma_all <- table(as.numeric(pred_svm_open_ma_all), test_data$openness_ober_2)
F_meas(tb_svm_open_ma_all)
precision(tb_svm_open_ma_all)
recall(tb_svm_open_ma_all)