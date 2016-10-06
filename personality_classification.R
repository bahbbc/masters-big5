library(caret)
library(e1071)
library(gbm)

personalities <- read.csv('personality-normalized-3.csv')

my_features <- c('skip','compound','sentences','chars','allTokens','wordTokens','hashtags','links','punct','questions','exclam','numbers','upcase','lowcase','firstup','pt.lexicon','added','verb.pro','names','en.lexicon','rewrite','mispell','homograph','foreign','emo.','emo..1','emo..','laugh','emph','echars','unkn','X1funct','X2pronoun','X3ppron','X4i','X5we','X6you','X7shehe','X8they','X9ipron','X10article','X11verb','X12auxverb','X13past','X14present','X15future','X16adverb','X17preps','X18conj','X19negate','X20quant','X21number','X22swear','X23social','X24family','X25friend','X26humans','X27affect','X28posemo','X29negemo','X30anx','X31anger','X32sad','X33cogmech','X34insight','X35cause','X36discrep','X37tentat','X38certain','X39inhib','X40incl','X41excl','X42percept','X43see','X44hear','X45feel','X46bio','X47body','X48health','X49sexual','X50ingest','X51relativ','X52motion','X53space','X54time','X55work','X56achieve','X57leisure','X58home','X59money','X60relig','X61death','X62assent','X63nonfl','X64filler','m','f','s','p','aument','dimin','superlat','N','A','PREP','CONJ','ADV','PREFIX','SIGLA','ABREV','INTERJ','DET','def','indef','NUM','numC','numO','numM','numF','PRO','proDem','proIndef','proRel','proInterr','proTrat','proPoss','proPess','acusativa','dativa','nominativa','obliqua','reflexa','p1','p2','p3','V','VW','VG','VK','VP','VI','VJ','VF','VQ','VS','VT','VU','VY','VC','V1s','V2s','V3s','V1p','V2p','V3p')

my_improved_features <- c('X1funct','X2pronoun','X3ppron','X4i','X5we','X6you','X7shehe','X8they','X9ipron','X10article','X11verb','X12auxverb','X13past','X14present','X15future','X16adverb','X17preps','X18conj','X19negate','X20quant','X21number','X22swear','X23social','X24family','X25friend','X26humans','X27affect','X28posemo','X29negemo','X30anx','X31anger','X32sad','X33cogmech','X34insight','X35cause','X36discrep','X37tentat','X38certain','X39inhib','X40incl','X41excl','X42percept','X43see','X44hear','X45feel','X46bio','X47body','X48health','X49sexual','X50ingest','X51relativ','X52motion','X53space','X54time','X55work','X56achieve','X57leisure','X58home','X59money','X60relig','X61death','X62assent','X63nonfl','X64filler')

social_features <- c('skip','compound','sentences','chars','allTokens','wordTokens','hashtags','links','punct','questions','exclam','numbers','upcase','lowcase','firstup','pt.lexicon','added','verb.pro','names','en.lexicon','rewrite','mispell','homograph','foreign','emo.','emo..1','emo..','laugh','emph','echars','unkn')

gramatical_features <- c('m','f','s','p','aument','dimin','superlat','N','A','PREP','CONJ','ADV','PREFIX','SIGLA','ABREV','INTERJ','DET','def','indef','NUM','numC','numO','numM','numF','PRO','proDem','proIndef','proRel','proInterr','proTrat','proPoss','proPess','acusativa','dativa','nominativa','obliqua','reflexa','p1','p2','p3','V','VW','VG','VK','VP','VI','VJ','VF','VQ','VS','VT','VU','VY','VC','V1s','V2s','V3s','V1p','V2p','V3p')

all_features <- c('X1funct','X2pronoun','X3ppron','X4i','X5we','X6you','X7shehe','X8they','X9ipron','X10article','X11verb','X12auxverb','X13past','X14present','X15future','X16adverb','X17preps','X18conj','X19negate','X20quant','X21number','X22swear','X23social','X24family','X25friend','X26humans','X27affect','X28posemo','X29negemo','X30anx','X31anger','X32sad','X33cogmech','X34insight','X35cause','X36discrep','X37tentat','X38certain','X39inhib','X40incl','X41excl','X42percept','X43see','X44hear','X45feel','X46bio','X47body','X48health','X49sexual','X50ingest','X51relativ','X52motion','X53space','X54time','X55work','X56achieve','X57leisure','X58home','X59money','X60relig','X61death','X62assent','X63nonfl','X64filler', 'skip','compound','sentences','chars','allTokens','wordTokens','hashtags','links','punct','questions','exclam','numbers','upcase','lowcase','firstup','pt.lexicon','added','verb.pro','names','en.lexicon','rewrite','mispell','homograph','foreign','emo.','emo..1','emo..','laugh','emph','echars','unkn', 'm','f','s','p','aument','dimin','superlat','N','A','PREP','CONJ','ADV','PREFIX','SIGLA','ABREV','INTERJ','DET','def','indef','NUM','numC','numO','numM','numF','PRO','proDem','proIndef','proRel','proInterr','proTrat','proPoss','proPess','acusativa','dativa','nominativa','obliqua','reflexa','p1','p2','p3','V','VW','VG','VK','VP','VI','VJ','VF','VQ','VS','VT','VU','VY','VC','V1s','V2s','V3s','V1p','V2p','V3p')

# remove texts with few words

new_personalities <- personalities[personalities$words > 500,]

# divide personalities in half (using mairesse division)

op_1 <- new_personalities[new_personalities$extraversion_m == 1,]
op_2 <- new_personalities[new_personalities$extraversion_m == 0,]

# divide personalities by  (mean + sd) or (mean - sd)

op_2_1 <- new_personalities[new_personalities$extraversion_ober_2 == -1,]
op_2_2 = new_personalities[new_personalities$extraversion_ober_2 == 1,]

#fazer um shuffle com esses dados

extra_sorted_m <- rbind(op_1, op_2)
extra_sorted_o <- rbind(op_2_1, op_2_2)

paste(dim(extra_sorted_o)[1], dim(extra_sorted_m)[1])

# separate train and test sets

set.seed(78)
training_data_rows <- createDataPartition(extra_sorted_m$extraversion_m, p = .8, list = FALSE)
train_data <- extra_sorted_m[training_data_rows,]
test_data <- extra_sorted_m[-training_data_rows,]

#EXTRAVERSION

# SVM 

#LIWC
svm_extra_ma_liwc <- svm(~ extraversion_m + X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, gamma = 0.1, cost = 1000)
pred_svm_extra_ma_liwc <- predict(svm_extra_ma_liwc, test_data)
tb_svm_extra_ma_liwc <- table(as.numeric(pred_svm_extra_ma_liwc), test_data$extraversion_m)
precision(tb_svm_extra_ma_liwc)
recall(tb_svm_extra_ma_liwc)
F_meas(tb_svm_extra_ma_liwc)


#SOCIAL
svm_extra_ma_social <- svm(~ extraversion_m +   skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, gamma = 0.05)
pred_svm_extra_ma_social <- predict(svm_extra_ma_social, test_data)
tb_svm_extra_ma_social <- table(as.numeric(pred_svm_extra_ma_social), test_data$extraversion_m)
precision(tb_svm_extra_ma_social)
recall(tb_svm_extra_ma_social)
F_meas(tb_svm_extra_ma_social)


#GRAMMAR
svm_extra_ma_gram <- svm(~ extraversion_m + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, gamma = 0.09, cost = 1000)
pred_svm_extra_ma_gram <- predict(svm_extra_ma_gram, test_data)
tb_svm_extra_ma_gram <- table(as.numeric(pred_svm_extra_ma_gram), test_data$extraversion_m)
precision(tb_svm_extra_ma_gram)
recall(tb_svm_extra_ma_gram)
F_meas(tb_svm_extra_ma_gram)


# BOOSTED TREES

#LIWC
gbm_extra_ma_liwc <- train(as.factor(extraversion_m) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_extra_ma_liwc <- predict(gbm_extra_ma_liwc, test_data)
tb_gbm_extra_ma_liwc <- table(pred_gbm_extra_ma_liwc, test_data$extraversion_m)
precision(tb_gbm_extra_ma_liwc)
recall(tb_gbm_extra_ma_liwc)
F_meas(tb_gbm_extra_ma_liwc)


#SOCIAL
gbm_extra_ma_social <- train(as.factor(extraversion_m) ~   skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_extra_ma_social <- predict(gbm_extra_ma_social, test_data)
tb_gbm_extra_ma_social <- table(pred_gbm_extra_ma_social, test_data$extraversion_m)
precision(tb_gbm_extra_ma_social)
recall(tb_gbm_extra_ma_social)
F_meas(tb_gbm_extra_ma_social)


#GRAMMAR
gbm_extra_ma_gram <- train(as.factor(extraversion_m) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_extra_ma_gram <- predict(gbm_extra_ma_gram, test_data)
tb_gbm_extra_ma_gram <- table(pred_gbm_extra_ma_gram, test_data$extraversion_m)
precision(tb_gbm_extra_ma_gram)
recall(tb_gbm_extra_ma_gram)
F_meas(tb_gbm_extra_ma_gram)


##############################################

# AGREABLENESS

#SVM

#LIWC
svm_agrea_ma_liwc <- svm(~ agreeabeness_m + X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, gamma = 0.09, cost = 1000)
pred_svm_agrea_ma_liwc <- predict(svm_agrea_ma_liwc, test_data)
tb_svm_agrea_ma_liwc <- table(as.numeric(pred_svm_agrea_ma_liwc), test_data$agreeabeness_m)
precision(tb_svm_agrea_ma_liwc)
recall(tb_svm_agrea_ma_liwc)
F_meas(tb_svm_agrea_ma_liwc)


#SOCIAL
svm_agrea_ma_social <- svm(~ agreeabeness_m + skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, gamma = 0.09, cost = 1000)
pred_svm_agrea_ma_social <- predict(svm_agrea_ma_social, test_data)
tb_svm_agrea_ma_social <- table(as.numeric(pred_svm_agrea_ma_social), test_data$agreeabeness_m)
precision(tb_svm_agrea_ma_social)
recall(tb_svm_agrea_ma_social)
F_meas(tb_svm_agrea_ma_social)


#GRAMMAR
svm_agrea_ma_gram <- svm(~ agreeabeness_m + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, gamma = 0.09, cost = 1000)
pred_svm_agrea_ma_gram <- predict(svm_agrea_ma_gram, test_data)
tb_svm_agrea_ma_gram <- table(as.numeric(pred_svm_agrea_ma_gram), test_data$agreeabeness_m)
precision(tb_svm_agrea_ma_gram)
recall(tb_svm_agrea_ma_gram)
F_meas(tb_svm_agrea_ma_gram)


# BOOSTED TREES

#LIWC
gbm_agre_ma_liwc <- train(as.factor(agreeabeness_m) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_agre_ma_liwc <- predict(gbm_agre_ma_liwc, test_data)
tb_gbm_agre_ma_liwc <- table(pred_gbm_agre_ma_liwc, test_data$agreeabeness_m)
precision(tb_gbm_agre_ma_liwc)
recall(tb_gbm_agre_ma_liwc)
F_meas(tb_gbm_agre_ma_liwc)


#SOCIAL
gbm_agre_ma_social <- train(as.factor(agreeabeness_m) ~   skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_agre_ma_social <- predict(gbm_agre_ma_social, test_data)
tb_gbm_agre_ma_social <- table(pred_gbm_agre_ma_social, test_data$agreeabeness_m)
precision(tb_gbm_agre_ma_social)
recall(tb_gbm_agre_ma_social)
F_meas(tb_gbm_agre_ma_social)


#GRAMMAR
gbm_agre_ma_gram <- train(as.factor(agreeabeness_m) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_agre_ma_gram <- predict(gbm_agre_ma_gram, test_data)
tb_gbm_agre_ma_gram <- table(pred_gbm_agre_ma_gram, test_data$agreeabeness_m)
precision(tb_gbm_agre_ma_gram)
recall(tb_gbm_agre_ma_gram)
F_meas(tb_gbm_agre_ma_gram)


############################

#CONSCIENTIOUSNESS

#LIWC
svm_cons_ma_liwc <- svm(~ conscientiousness_m + X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, gamma = 0.09, cost = 1000)
pred_svm_cons_ma_liwc <- predict(svm_cons_ma_liwc, test_data)
tb_svm_cons_ma_liwc <- table(as.numeric(pred_svm_cons_ma_liwc), test_data$conscientiousness_m)
precision(tb_svm_cons_ma_liwc)
recall(tb_svm_cons_ma_liwc)
F_meas(tb_svm_cons_ma_liwc)


#SOCIAL
svm_cons_ma_social <- svm(~ conscientiousness_m + skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, gamma = 0.09, cost = 1000)
pred_svm_cons_ma_social <- predict(svm_cons_ma_social, test_data)
tb_svm_cons_ma_social <- table(as.numeric(pred_svm_cons_ma_social), test_data$conscientiousness_m)
precision(tb_svm_cons_ma_social)
recall(tb_svm_cons_ma_social)
F_meas(tb_svm_cons_ma_social)

#GRAMMAR
svm_gram_ma_gram <- svm(~ conscientiousness_m + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, gamma = 0.09, cost = 1000)
pred_svm_gram_ma_gram <- predict(svm_gram_ma_gram, test_data)
tb_svm_gram_ma_gram <- table(as.numeric(pred_svm_gram_ma_gram), test_data$conscientiousness_m)
precision(tb_svm_gram_ma_gram)
recall(tb_svm_gram_ma_gram)
F_meas(tb_svm_gram_ma_gram)


# BOOSTED TREES

#LIWC
gbm_cons_ma_liwc <- train(as.factor(conscientiousness_m) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_cons_ma_liwc <- predict(gbm_cons_ma_liwc, test_data)
tb_gbm_cons_ma_liwc <-  table(pred_gbm_cons_ma_liwc, test_data$conscientiousness_m)
precision(tb_gbm_cons_ma_liwc)
recall(tb_gbm_cons_ma_liwc)
F_meas(tb_gbm_cons_ma_liwc)


#SOCIAL
gbm_cons_ma_social <- train(as.factor(conscientiousness_m) ~   skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_cons_ma_social <- predict(gbm_cons_ma_social, test_data)
tb_gbm_cons_ma_social <- table(pred_gbm_cons_ma_social, test_data$conscientiousness_m)
precision(tb_gbm_cons_ma_social)
recall(tb_gbm_cons_ma_social)
F_meas(tb_gbm_cons_ma_social)

#GRAMMAR
gbm_cons_ma_gram <- train(as.factor(conscientiousness_m) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_cons_ma_gram <- predict(gbm_cons_ma_gram, test_data)
tb_gbm_cons_ma_gram <- table(pred_gbm_cons_ma_gram, test_data$conscientiousness_m)
precision(tb_gbm_cons_ma_gram)
recall(tb_gbm_cons_ma_gram)
F_meas(tb_gbm_cons_ma_gram)

# NEUROTICISM

#LIWC
svm_neu_ma_liwc <- svm(~ neuroticism_m + X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, gamma = 0.09, cost = 1000)
pred_svm_neu_ma_liwc <- predict(svm_neu_ma_liwc, test_data)
tb_svm_neu_ma_liwc <-  table(as.numeric(pred_svm_neu_ma_liwc), test_data$neuroticism_m)
precision(tb_svm_neu_ma_liwc)
recall(tb_svm_neu_ma_liwc)
F_meas(tb_svm_neu_ma_liwc)


#SOCIAL
svm_neu_ma_social <- svm(~ neuroticism_m + skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, gamma = 0.09, cost = 1000)
pred_svm_neu_ma_social <- predict(svm_neu_ma_social, test_data)
tb_svm_neu_ma_social <- table(as.numeric(pred_svm_neu_ma_social), test_data$neuroticismsm_m)
precision(tb_svm_neu_ma_social)
recall(tb_svm_neu_ma_social)
F_meas(tb_svm_neu_ma_social)

#GRAMMAR
svm_neu_ma_gram <- svm(~ neuroticism_m + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, gamma = 0.09, cost = 1000)
pred_svm_neu_ma_gram <- predict(svm_neu_ma_gram, test_data)
tb_svm_neu_ma_gram <- table(as.numeric(pred_svm_neu_ma_gram), test_data$neuroticism_m)
precision(tb_svm_neu_ma_gram)
recall(tb_svm_neu_ma_gram)
F_meas(tb_svm_neu_ma_gram)


# BOOSTED TREES

#LIWC
gbm_neu_ma_liwc <- train(as.factor(neuroticism_m) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_neu_ma_liwc <- predict(gbm_neu_ma_liwc, test_data)
tb_gbm_neu_ma_liwc <- table(pred_gbm_neu_ma_liwc, test_data$neuroticism_m)
precision(tb_gbm_neu_ma_liwc)
recall(tb_gbm_neu_ma_liwc)
F_meas(tb_gbm_neu_ma_liwc)

#SOCIAL
gbm_neu_ma_social <- train(as.factor(neuroticism_m) ~   skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_neu_ma_social <- predict(gbm_neu_ma_social, test_data)
tb_gbm_neu_ma_social <- table(pred_gbm_neu_ma_social, test_data$neuroticism_m)
precision(tb_gbm_neu_ma_social)
recall(tb_gbm_neu_ma_social)
F_meas(tb_gbm_neu_ma_social)


#GRAMMAR
gbm_neu_ma_gram <- train(as.factor(neuroticism_m) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_neu_ma_gram <- predict(gbm_neu_ma_gram, test_data)
tb_gbm_neu_ma_gram <- table(pred_gbm_neu_ma_gram, test_data$neuroticism_m)
precision(tb_gbm_neu_ma_gram)
recall(tb_gbm_neu_ma_gram)
F_meas(tb_gbm_neu_ma_gram)

################

# OPENESS

#LIWC
svm_open_ma_liwc <- svm(~ openness_m + X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, gamma = 0.09, cost = 1000)
pred_svm_open_ma_liwc <- predict(svm_open_ma_liwc, test_data)
tb_svm_open_ma_liwc <- table(as.numeric(pred_svm_open_ma_liwc), test_data$openness_m)
precision(tb_svm_open_ma_liwc)
recall(tb_svm_open_ma_liwc)
F_meas(tb_svm_open_ma_liwc)

#SOCIAL
svm_open_ma_social <- svm(~ openness_m + skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, gamma = 0.09, cost = 1000)
pred_svm_open_ma_social <- predict(svm_open_ma_social, test_data)
tb_svm_open_ma_social <- table(as.numeric(pred_svm_open_ma_social), test_data$openness_m)
precision(tb_svm_open_ma_social)
recall(tb_svm_open_ma_social)
F_meas(tb_svm_open_ma_social)


#GRAMMAR
svm_open_ma_gram <- svm(~ openness_m + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, gamma = 0.09, cost = 1000)
pred_svm_open_ma_gram <- predict(svm_open_ma_gram, test_data)
tb_svm_open_ma_gram <- table(as.numeric(pred_svm_open_ma_gram), test_data$openness_m)
precision(tb_svm_open_ma_gram)
recall(tb_svm_open_ma_gram)
F_meas(tb_svm_open_ma_gram)

# BOOSTED TREES

#LIWC
gbm_neu_ma_liwc <- train(as.factor(neuroticism_m) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_neu_ma_liwc <- predict(gbm_neu_ma_liwc, test_data)
tb_gbm_neu_ma_liwc <- table(pred_gbm_neu_ma_liwc, test_data$neuroticism_m)
precision(tb_gbm_neu_ma_liwc)
recall(tb_gbm_neu_ma_liwc)
F_meas(tb_gbm_neu_ma_liwc)

#SOCIAL
gbm_neu_ma_social <- train(as.factor(neuroticism_m) ~   skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_neu_ma_social <- predict(gbm_neu_ma_social, test_data)
tb_gbm_neu_so_ma <- table(pred_gbm_neu_ma_social, test_data$neuroticism_m)
precision(tb_gbm_neu_so_ma)
recall(tb_gbm_neu_so_ma)
F_meas(tb_gbm_neu_so_ma)

#GRAMMAR
gbm_neu_ma_gram <- train(as.factor(neuroticism_m) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_neu_ma_gram <- predict(gbm_neu_ma_gram, test_data)
tb_gbm_neu_gra_ma <- table(pred_gbm_neu_ma_gram, test_data$neuroticism_m)
precision(tb_gbm_neu_gra_ma)
recall(tb_gbm_neu_gra_ma)
F_meas(tb_gbm_neu_gra_ma)

