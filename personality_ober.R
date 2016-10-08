library(caret)
library(e1071)
library(gbm)

personalities <- read.csv('personality-normalized-3.csv')

new_personalities <- personalities[personalities$words > 500,]

#EXTRAVERSION

# divide personalities by  (mean + sd) or (mean - sd)

op_2_1 <- new_personalities[new_personalities$extraversion_ober_2 == -1,]
op_2_2 = new_personalities[new_personalities$extraversion_ober_2 == 1,]

extra_sorted_o <- rbind(op_2_1, op_2_2)

# separate train and test sets

set.seed(78)
training_data_rows <- createDataPartition(extra_sorted_m$extraversion_ober_2, p = .8, list = FALSE)
train_data <- extra_sorted_m[training_data_rows,]
test_data <- extra_sorted_m[-training_data_rows,]


# SVM 

#PREPROC
svm_extra_ma_social <- svm(~ extraversion_ober_2 +   skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, gamma = 0.1)
pred_svm_extra_ma_social <- predict(svm_extra_ma_social, test_data)
tb_svm_extra_ma_social <- table(as.numeric(pred_svm_extra_ma_social), test_data$extraversion_ober_2)
F_meas(tb_svm_extra_ma_social)
precision(tb_svm_extra_ma_social)
recall(tb_svm_extra_ma_social)

#GRAMMAR
svm_extra_ma_gram <- svm(~ extraversion_ober_2 + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, gamma = 0.1)
pred_svm_extra_ma_gram <- predict(svm_extra_ma_gram, test_data)
tb_svm_extra_ma_gram <- table(as.numeric(pred_svm_extra_ma_gram), test_data$extraversion_ober_2)
F_meas(tb_svm_extra_ma_gram)
precision(tb_svm_extra_ma_gram)
recall(tb_svm_extra_ma_gram)

#LIWC
svm_extra_ma_liwc <- svm(~ extraversion_ober_2 + X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, gamma = 0.1)
pred_svm_extra_ma_liwc <- predict(svm_extra_ma_liwc, test_data)
tb_svm_extra_ma_liwc <- table(as.numeric(pred_svm_extra_ma_liwc), test_data$extraversion_ober_2)
F_meas(tb_svm_extra_ma_liwc)
precision(tb_svm_extra_ma_liwc)
recall(tb_svm_extra_ma_liwc)

#ALL
svm_extra_ma_all <- svm(~ extraversion_ober_2 +  X1funct + X2pronoun + X3ppron + X4i + X5we + X6you + X7shehe + X8they + X9ipron + X10article + X11verb + X12auxverb + X13past + X14present + X15future + X16adverb + X17preps + X18conj + X19negate + X20quant + X21number + X22swear + X23social + X24family + X25friend + X26humans + X27affect + X28posemo + X29negemo + X30anx + X31anger + X32sad + X33cogmech + X34insight + X35cause + X36discrep + X37tentat + X38certain + X39inhib + X40incl + X41excl + X42percept + X43see + X44hear + X45feel + X46bio + X47body + X48health + X49sexual + X50ingest + X51relativ + X52motion + X53space + X54time + X55work + X56achieve + X57leisure + X58home + X59money + X60relig + X61death + X62assent + X63nonfl + X64filler +  skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn +  m + f + s + p + aument + dimin + superlat + N + A + PREP + CONJ + ADV + PREFIX + SIGLA + ABREV + INTERJ + DET + def + indef + NUM + numC + numO + numM + numF + PRO + proDem + proIndef + proRel + proInterr + proTrat + proPoss + proPess + acusativa + dativa + nominativa + obliqua + reflexa + p1 + p2 + p3 + V + VW + VG + VK + VP + VI + VJ + VF + VQ + VS + VT + VU + VY + VC + V1s + V2s + V3s + V1p + V2p + V3p, data = train_data, gamma = 0.1)
pred_svm_extra_ma_all <- predict(svm_extra_ma_all, test_data)
tb_svm_extra_ma_all <- table(as.numeric(pred_svm_extra_ma_all), test_data$extraversion_ober_2)
F_meas(tb_svm_extra_ma_all)
precision(tb_svm_extra_ma_all)
recall(tb_svm_extra_ma_all)

# BOOSTED TREES

#PREPROC
gbm_extra_ma_social <- train(as.factor(extraversion_ober_2) ~   skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_extra_ma_social <- predict(gbm_extra_ma_social, test_data)
tb_gbm_extra_ma_social <- table(pred_gbm_extra_ma_social, test_data$extraversion_ober_2)
F_meas(tb_gbm_extra_ma_social)
precision(tb_gbm_extra_ma_social)
recall(tb_gbm_extra_ma_social)

#GRAMMAR
gbm_extra_ma_gram <- train(as.factor(extraversion_ober_2) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_extra_ma_gram <- predict(gbm_extra_ma_gram, test_data)
tb_gbm_extra_ma_gram <- table(pred_gbm_extra_ma_gram, test_data$extraversion_ober_2)
F_meas(tb_gbm_extra_ma_gram)
precision(tb_gbm_extra_ma_gram)
recall(tb_gbm_extra_ma_gram)

#LIWC
gbm_extra_ma_liwc <- train(as.factor(extraversion_ober_2) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_extra_ma_liwc <- predict(gbm_extra_ma_liwc, test_data)
tb_gbm_extra_ma_liwc <- table(pred_gbm_extra_ma_liwc, test_data$extraversion_ober_2)
F_meas(tb_gbm_extra_ma_liwc)
precision(tb_gbm_extra_ma_liwc)
recall(tb_gbm_extra_ma_liwc)

#ALL
gbl_extra_ma_all <- train(as.factor(extraversion_ober_2) ~  X1funct + X2pronoun + X3ppron + X4i + X5we + X6you + X7shehe + X8they + X9ipron + X10article + X11verb + X12auxverb + X13past + X14present + X15future + X16adverb + X17preps + X18conj + X19negate + X20quant + X21number + X22swear + X23social + X24family + X25friend + X26humans + X27affect + X28posemo + X29negemo + X30anx + X31anger + X32sad + X33cogmech + X34insight + X35cause + X36discrep + X37tentat + X38certain + X39inhib + X40incl + X41excl + X42percept + X43see + X44hear + X45feel + X46bio + X47body + X48health + X49sexual + X50ingest + X51relativ + X52motion + X53space + X54time + X55work + X56achieve + X57leisure + X58home + X59money + X60relig + X61death + X62assent + X63nonfl + X64filler +  skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn +  m + f + s + p + aument + dimin + superlat + N + A + PREP + CONJ + ADV + PREFIX + SIGLA + ABREV + INTERJ + DET + def + indef + NUM + numC + numO + numM + numF + PRO + proDem + proIndef + proRel + proInterr + proTrat + proPoss + proPess + acusativa + dativa + nominativa + obliqua + reflexa + p1 + p2 + p3 + V + VW + VG + VK + VP + VI + VJ + VF + VQ + VS + VT + VU + VY + VC + V1s + V2s + V3s + V1p + V2p + V3p, data = train_data, method="gbm", verbose = FALSE)
pred_gbl_extra_ma_all <- predict(gbl_extra_ma_all, test_data)
tb_gbl_extra_ma_all <- table(pred_gbl_extra_ma_all, test_data$extraversion_ober_2)
F_meas(tb_gbl_extra_ma_all)
precision(tb_gbl_extra_ma_all)
recall(tb_gbl_extra_ma_all)

##############################################

# divide personalities by  (mean + sd) or (mean - sd)

op_2_1 <- new_personalities[new_personalities$agreeableness_ober_2 == -1,]
op_2_2 = new_personalities[new_personalities$agreeableness_ober_2 == 1,]

agrea_sorted_o <- rbind(op_2_1, op_2_2)

# separate train and test sets

set.seed(78)
training_data_rows <- createDataPartition(agrea_sorted_o$agreeableness_ober_2, p = .8, list = FALSE)
train_data <- agrea_sorted_o[training_data_rows,]
test_data <- agrea_sorted_o[-training_data_rows,]


# AGREABLENESS

#SVM

#PREPROC
svm_agrea_ma_social <- svm(~ agreeableness_ober_2 + skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, gamma = 0.1)
pred_svm_agrea_ma_social <- predict(svm_agrea_ma_social, test_data)
tb_svm_agrea_ma_social <- table(as.numeric(pred_svm_agrea_ma_social), test_data$agreeableness_ober_2)
F_meas(tb_svm_agrea_ma_social)
precision(tb_svm_agrea_ma_social)
recall(tb_svm_agrea_ma_social)

#GRAMMAR
svm_agrea_ma_gram <- svm(~ agreeableness_ober_2 + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, gamma = 0.1)
pred_svm_agrea_ma_gram <- predict(svm_agrea_ma_gram, test_data)
tb_svm_agrea_ma_gram <- table(as.numeric(pred_svm_agrea_ma_gram), test_data$agreeableness_ober_2)
F_meas(tb_svm_agrea_ma_gram)
precision(tb_svm_agrea_ma_gram)
recall(tb_svm_agrea_ma_gram)

#LIWC
svm_agrea_ma_liwc <- svm(~ agreeableness_ober_2 + X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, gamma = 0.1)
pred_svm_agrea_ma_liwc <- predict(svm_agrea_ma_liwc, test_data)
tb_svm_agrea_ma_liwc <- table(as.numeric(pred_svm_agrea_ma_liwc), test_data$agreeableness_ober_2)
F_meas(tb_svm_agrea_ma_liwc)
precision(tb_svm_agrea_ma_liwc)
recall(tb_svm_agrea_ma_liwc)

#ALL
svm_agrea_ma_all <- svm(~ agreeableness_ober_2 +  X1funct + X2pronoun + X3ppron + X4i + X5we + X6you + X7shehe + X8they + X9ipron + X10article + X11verb + X12auxverb + X13past + X14present + X15future + X16adverb + X17preps + X18conj + X19negate + X20quant + X21number + X22swear + X23social + X24family + X25friend + X26humans + X27affect + X28posemo + X29negemo + X30anx + X31anger + X32sad + X33cogmech + X34insight + X35cause + X36discrep + X37tentat + X38certain + X39inhib + X40incl + X41excl + X42percept + X43see + X44hear + X45feel + X46bio + X47body + X48health + X49sexual + X50ingest + X51relativ + X52motion + X53space + X54time + X55work + X56achieve + X57leisure + X58home + X59money + X60relig + X61death + X62assent + X63nonfl + X64filler +  skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn +  m + f + s + p + aument + dimin + superlat + N + A + PREP + CONJ + ADV + PREFIX + SIGLA + ABREV + INTERJ + DET + def + indef + NUM + numC + numO + numM + numF + PRO + proDem + proIndef + proRel + proInterr + proTrat + proPoss + proPess + acusativa + dativa + nominativa + obliqua + reflexa + p1 + p2 + p3 + V + VW + VG + VK + VP + VI + VJ + VF + VQ + VS + VT + VU + VY + VC + V1s + V2s + V3s + V1p + V2p + V3p, data = train_data, gamma = 0.1)
pred_svm_agrea_ma_all <- predict(svm_agrea_ma_all, test_data)
tb_svm_agrea_ma_all <- table(as.numeric(pred_svm_agrea_ma_all), test_data$agreeableness_ober_2)
F_meas(tb_svm_agrea_ma_all)
precision(tb_svm_agrea_ma_all)
recall(tb_svm_agrea_ma_all)

# BOOSTED TREES


#PREPROC
gbm_agre_ma_social <- train(as.factor(agreeableness_ober_2) ~   skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_agre_ma_social <- predict(agreeableness_ober_2, test_data)
tb_gbm_agre_ma_social <- table(pred_gbm_agre_ma_social, test_data$agreeableness_ober_2)
F_meas(tb_gbm_agre_ma_social)
precision(tb_gbm_agre_ma_social)
recall(tb_gbm_agre_ma_social)

#GRAMMAR
gbm_agre_ma_gram <- train(as.factor(agreeableness_ober_2) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_agre_ma_gram <- predict(gbm_agre_ma_gram, test_data)
tb_gbm_agre_ma_gram <- table(pred_gbm_agre_ma_gram, test_data$agreeableness_ober_2)
F_meas(tb_gbm_agre_ma_gram)
precision(tb_gbm_agre_ma_gram)
recall(tb_gbm_agre_ma_gram)

#LIWC
gbm_agre_ma_liwc <- train(as.factor(agreeableness_ober_2) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_agre_ma_liwc <- predict(gbm_agre_ma_liwc, test_data)
tb_gbm_agre_ma_liwc <- table(pred_gbm_agre_ma_liwc, test_data$agreeableness_ober_2)
F_meas(tb_gbm_agre_ma_liwc)
precision(tb_gbm_agre_ma_liwc)
recall(tb_gbm_agre_ma_liwc)

#ALL
gbl_agre_ma_all <- train(as.factor(agreeableness_ober_2) ~  X1funct + X2pronoun + X3ppron + X4i + X5we + X6you + X7shehe + X8they + X9ipron + X10article + X11verb + X12auxverb + X13past + X14present + X15future + X16adverb + X17preps + X18conj + X19negate + X20quant + X21number + X22swear + X23social + X24family + X25friend + X26humans + X27affect + X28posemo + X29negemo + X30anx + X31anger + X32sad + X33cogmech + X34insight + X35cause + X36discrep + X37tentat + X38certain + X39inhib + X40incl + X41excl + X42percept + X43see + X44hear + X45feel + X46bio + X47body + X48health + X49sexual + X50ingest + X51relativ + X52motion + X53space + X54time + X55work + X56achieve + X57leisure + X58home + X59money + X60relig + X61death + X62assent + X63nonfl + X64filler +  skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn +  m + f + s + p + aument + dimin + superlat + N + A + PREP + CONJ + ADV + PREFIX + SIGLA + ABREV + INTERJ + DET + def + indef + NUM + numC + numO + numM + numF + PRO + proDem + proIndef + proRel + proInterr + proTrat + proPoss + proPess + acusativa + dativa + nominativa + obliqua + reflexa + p1 + p2 + p3 + V + VW + VG + VK + VP + VI + VJ + VF + VQ + VS + VT + VU + VY + VC + V1s + V2s + V3s + V1p + V2p + V3p, data = train_data, method="gbm", verbose = FALSE)
pred_gbl_agre_ma_all <- predict(gbl_agre_ma_all, test_data)
tb_gbl_agre_ma_all <- table(pred_gbl_agre_ma_all, test_data$agreeableness_ober_2)
F_meas(tb_gbl_agre_ma_all)
precision(tb_gbl_agre_ma_all)
recall(tb_gbl_agre_ma_all)

############################

# divide personalities by  (mean + sd) or (mean - sd)

op_2_1 <- new_personalities[new_personalities$conscientiousness_ober_2 == -1,]
op_2_2 = new_personalities[new_personalities$conscientiousness_ober_2 == 1,]

cons_sorted_o <- rbind(op_2_1, op_2_2)

# separate train and test sets

set.seed(78)
training_data_rows <- createDataPartition(cons_sorted_o$conscientiousness_ober_2, p = .8, list = FALSE)
train_data <- cons_sorted_o[training_data_rows,]
test_data <- cons_sorted_o[-training_data_rows,]

#CONSCIENTIOUSNESS


#PREPROC
svm_cons_ma_social <- svm(~ conscientiousness_ober_2 + skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, gamma = 0.1)
pred_svm_cons_ma_social <- predict(svm_cons_ma_social, test_data)
tb_svm_cons_ma_social <- table(as.numeric(pred_svm_cons_ma_social), test_data$conscientiousness_ober_2)
F_meas(tb_svm_cons_ma_social)
precision(tb_svm_cons_ma_social)
recall(tb_svm_cons_ma_social)

#GRAMMAR
svm_gram_ma_gram <- svm(~ conscientiousness_ober_2 + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, gamma = 0.1)
pred_svm_gram_ma_gram <- predict(svm_gram_ma_gram, test_data)
tb_svm_gram_ma_gram <- table(as.numeric(pred_svm_gram_ma_gram), test_data$conscientiousness_ober_2)
F_meas(tb_svm_gram_ma_gram)
precision(tb_svm_gram_ma_gram)
recall(tb_svm_gram_ma_gram)

#LIWC
svm_cons_ma_liwc <- svm(~ conscientiousness_ober_2 + X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, gamma = 0.1)
pred_svm_cons_ma_liwc <- predict(svm_cons_ma_liwc, test_data)
tb_svm_cons_ma_liwc <- table(as.numeric(pred_svm_cons_ma_liwc), test_data$conscientiousness_ober_2)
F_meas(tb_svm_cons_ma_liwc)
precision(tb_svm_cons_ma_liwc)
recall(tb_svm_cons_ma_liwc)

#ALL
svm_cons_ma_all <- svm(~ conscientiousness_ober_2 +  X1funct + X2pronoun + X3ppron + X4i + X5we + X6you + X7shehe + X8they + X9ipron + X10article + X11verb + X12auxverb + X13past + X14present + X15future + X16adverb + X17preps + X18conj + X19negate + X20quant + X21number + X22swear + X23social + X24family + X25friend + X26humans + X27affect + X28posemo + X29negemo + X30anx + X31anger + X32sad + X33cogmech + X34insight + X35cause + X36discrep + X37tentat + X38certain + X39inhib + X40incl + X41excl + X42percept + X43see + X44hear + X45feel + X46bio + X47body + X48health + X49sexual + X50ingest + X51relativ + X52motion + X53space + X54time + X55work + X56achieve + X57leisure + X58home + X59money + X60relig + X61death + X62assent + X63nonfl + X64filler +  skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn +  m + f + s + p + aument + dimin + superlat + N + A + PREP + CONJ + ADV + PREFIX + SIGLA + ABREV + INTERJ + DET + def + indef + NUM + numC + numO + numM + numF + PRO + proDem + proIndef + proRel + proInterr + proTrat + proPoss + proPess + acusativa + dativa + nominativa + obliqua + reflexa + p1 + p2 + p3 + V + VW + VG + VK + VP + VI + VJ + VF + VQ + VS + VT + VU + VY + VC + V1s + V2s + V3s + V1p + V2p + V3p, data = train_data, gamma = 0.1)
pred_svm_cons_ma_all <- predict(svm_cons_ma_all, test_data)
tb_svm_cons_ma_all <- table(as.numeric(pred_svm_cons_ma_all), test_data$conscientiousness_ober_2)
F_meas(tb_svm_cons_ma_all)
precision(tb_svm_cons_ma_all)
recall(tb_svm_cons_ma_all)


# BOOSTED TREES

#PREPROC
gbm_cons_ma_social <- train(as.factor(conscientiousness_ober_2) ~   skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_cons_ma_social <- predict(gbm_cons_ma_social, test_data)
tb_gbm_cons_ma_social <- table(pred_gbm_cons_ma_social, test_data$conscientiousness_ober_2)
F_meas(tb_gbm_cons_ma_social)
precision(tb_gbm_cons_ma_social)
recall(tb_gbm_cons_ma_social)

#GRAMMAR
gbm_cons_ma_gram <- train(as.factor(conscientiousness_ober_2) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_cons_ma_gram <- predict(gbm_cons_ma_gram, test_data)
tb_gbm_cons_ma_gram <- table(pred_gbm_cons_ma_gram, test_data$conscientiousness_ober_2)
F_meas(tb_gbm_cons_ma_gram)
precision(tb_gbm_cons_ma_gram)
recall(tb_gbm_cons_ma_gram)

#LIWC
gbm_cons_ma_liwc <- train(as.factor(conscientiousness_ober_2) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_cons_ma_liwc <- predict(gbm_cons_ma_liwc, test_data)
tb_gbm_cons_ma_liwc <-  table(pred_gbm_cons_ma_liwc, test_data$conscientiousness_ober_2)
F_meas(tb_gbm_cons_ma_liwc)
precision(tb_gbm_cons_ma_liwc)
recall(tb_gbm_cons_ma_liwc)

#ALL
gbl_cons_ma_all <- train(as.factor(conscientiousness_ober_2) ~  X1funct + X2pronoun + X3ppron + X4i + X5we + X6you + X7shehe + X8they + X9ipron + X10article + X11verb + X12auxverb + X13past + X14present + X15future + X16adverb + X17preps + X18conj + X19negate + X20quant + X21number + X22swear + X23social + X24family + X25friend + X26humans + X27affect + X28posemo + X29negemo + X30anx + X31anger + X32sad + X33cogmech + X34insight + X35cause + X36discrep + X37tentat + X38certain + X39inhib + X40incl + X41excl + X42percept + X43see + X44hear + X45feel + X46bio + X47body + X48health + X49sexual + X50ingest + X51relativ + X52motion + X53space + X54time + X55work + X56achieve + X57leisure + X58home + X59money + X60relig + X61death + X62assent + X63nonfl + X64filler +  skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn +  m + f + s + p + aument + dimin + superlat + N + A + PREP + CONJ + ADV + PREFIX + SIGLA + ABREV + INTERJ + DET + def + indef + NUM + numC + numO + numM + numF + PRO + proDem + proIndef + proRel + proInterr + proTrat + proPoss + proPess + acusativa + dativa + nominativa + obliqua + reflexa + p1 + p2 + p3 + V + VW + VG + VK + VP + VI + VJ + VF + VQ + VS + VT + VU + VY + VC + V1s + V2s + V3s + V1p + V2p + V3p, data = train_data, method="gbm", verbose = FALSE)
pred_gbl_cons_ma_all <- predict(gbl_cons_ma_all, test_data)
tb_gbl_cons_ma_all <- table(pred_gbl_cons_ma_all, test_data$conscientiousness_ober_2)
F_meas(tb_gbl_cons_ma_all)
precision(tb_gbl_cons_ma_all)
recall(tb_gbl_cons_ma_all)

##################################

# NEUROTICISM

# divide personalities by  (mean + sd) or (mean - sd)

op_2_1 <- new_personalities[new_personalities$neuroticism_ober_2 == -1,]
op_2_2 = new_personalities[new_personalities$neuroticism_ober_2 == 1,]

neu_sorted_o <- rbind(op_2_1, op_2_2)

# separate train and test sets

set.seed(78)
training_data_rows <- createDataPartition(neu_sorted_o$neuroticism_ober_2, p = .8, list = FALSE)
train_data <- neu_sorted_o[training_data_rows,]
test_data <- neu_sorted_o[-training_data_rows,]


#PREPROC
svm_neu_ma_social <- svm(~ neuroticism_ober_2 + skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, gamma = 0.1)
pred_svm_neu_ma_social <- predict(svm_neu_ma_social, test_data)
tb_svm_neu_ma_social <- table(as.numeric(pred_svm_neu_ma_social), test_data$neuroticism_ober_2)
F_meas(tb_svm_neu_ma_social)
precision(tb_svm_neu_ma_social)
recall(tb_svm_neu_ma_social)

#GRAMMAR
svm_neu_ma_gram <- svm(~ neuroticism_ober_2 + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, gamma = 0.1)
pred_svm_neu_ma_gram <- predict(svm_neu_ma_gram, test_data)
tb_svm_neu_ma_gram <- table(as.numeric(pred_svm_neu_ma_gram), test_data$neuroticism_ober_2)
F_meas(tb_svm_neu_ma_gram)
precision(tb_svm_neu_ma_gram)
recall(tb_svm_neu_ma_gram)

#LIWC
svm_neu_ma_liwc <- svm(~ neuroticism_ober_2 + X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, gamma = 0.1)
pred_svm_neu_ma_liwc <- predict(svm_neu_ma_liwc, test_data)
tb_svm_neu_ma_liwc <-  table(as.numeric(pred_svm_neu_ma_liwc), test_data$neuroticism_ober_2)
F_meas(tb_svm_neu_ma_liwc)
precision(tb_svm_neu_ma_liwc)
recall(tb_svm_neu_ma_liwc)

#ALL
svm_neu_ma_all <- svm(~ neuroticism_ober_2 +  X1funct + X2pronoun + X3ppron + X4i + X5we + X6you + X7shehe + X8they + X9ipron + X10article + X11verb + X12auxverb + X13past + X14present + X15future + X16adverb + X17preps + X18conj + X19negate + X20quant + X21number + X22swear + X23social + X24family + X25friend + X26humans + X27affect + X28posemo + X29negemo + X30anx + X31anger + X32sad + X33cogmech + X34insight + X35cause + X36discrep + X37tentat + X38certain + X39inhib + X40incl + X41excl + X42percept + X43see + X44hear + X45feel + X46bio + X47body + X48health + X49sexual + X50ingest + X51relativ + X52motion + X53space + X54time + X55work + X56achieve + X57leisure + X58home + X59money + X60relig + X61death + X62assent + X63nonfl + X64filler +  skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn +  m + f + s + p + aument + dimin + superlat + N + A + PREP + CONJ + ADV + PREFIX + SIGLA + ABREV + INTERJ + DET + def + indef + NUM + numC + numO + numM + numF + PRO + proDem + proIndef + proRel + proInterr + proTrat + proPoss + proPess + acusativa + dativa + nominativa + obliqua + reflexa + p1 + p2 + p3 + V + VW + VG + VK + VP + VI + VJ + VF + VQ + VS + VT + VU + VY + VC + V1s + V2s + V3s + V1p + V2p + V3p, data = train_data, gamma = 0.1)
pred_svm_neu_ma_all <- predict(svm_neu_ma_all, test_data)
tb_svm_neu_ma_all <- table(as.numeric(pred_svm_neu_ma_all), test_data$neuroticism_ober_2)
F_meas(tb_svm_neu_ma_all)
precision(tb_svm_neu_ma_all)
recall(tb_svm_neu_ma_all)

# BOOSTED TREES

#PREPROC
gbm_neu_ma_social <- train(as.factor(neuroticism_ober_2) ~   skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_neu_ma_social <- predict(gbm_neu_ma_social, test_data)
tb_gbm_neu_ma_social <- table(pred_gbm_neu_ma_social, test_data$neuroticism_ober_2)
F_meas(tb_gbm_neu_ma_social)
precision(tb_gbm_neu_ma_social)
recall(tb_gbm_neu_ma_social)

#GRAMMAR
gbm_neu_ma_gram <- train(as.factor(neuroticism_ober_2) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_neu_ma_gram <- predict(gbm_neu_ma_gram, test_data)
tb_gbm_neu_ma_gram <- table(pred_gbm_neu_ma_gram, test_data$neuroticism_ober_2)
F_meas(tb_gbm_neu_ma_gram)
precision(tb_gbm_neu_ma_gram)
recall(tb_gbm_neu_ma_gram)

#LIWC
gbm_neu_ma_liwc <- train(as.factor(neuroticism_ober_2) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_neu_ma_liwc <- predict(gbm_neu_ma_liwc, test_data)
tb_gbm_neu_ma_liwc <- table(pred_gbm_neu_ma_liwc, test_data$neuroticism_ober_2)
F_meas(tb_gbm_neu_ma_liwc)
precision(tb_gbm_neu_ma_liwc)
recall(tb_gbm_neu_ma_liwc)

#ALL
gbl_neu_ma_all <- train(as.factor(neuroticism_ober_2) ~  X1funct + X2pronoun + X3ppron + X4i + X5we + X6you + X7shehe + X8they + X9ipron + X10article + X11verb + X12auxverb + X13past + X14present + X15future + X16adverb + X17preps + X18conj + X19negate + X20quant + X21number + X22swear + X23social + X24family + X25friend + X26humans + X27affect + X28posemo + X29negemo + X30anx + X31anger + X32sad + X33cogmech + X34insight + X35cause + X36discrep + X37tentat + X38certain + X39inhib + X40incl + X41excl + X42percept + X43see + X44hear + X45feel + X46bio + X47body + X48health + X49sexual + X50ingest + X51relativ + X52motion + X53space + X54time + X55work + X56achieve + X57leisure + X58home + X59money + X60relig + X61death + X62assent + X63nonfl + X64filler +  skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn +  m + f + s + p + aument + dimin + superlat + N + A + PREP + CONJ + ADV + PREFIX + SIGLA + ABREV + INTERJ + DET + def + indef + NUM + numC + numO + numM + numF + PRO + proDem + proIndef + proRel + proInterr + proTrat + proPoss + proPess + acusativa + dativa + nominativa + obliqua + reflexa + p1 + p2 + p3 + V + VW + VG + VK + VP + VI + VJ + VF + VQ + VS + VT + VU + VY + VC + V1s + V2s + V3s + V1p + V2p + V3p, data = train_data, method="gbm", verbose = FALSE)
pred_gbl_neu_ma_all <- predict(gbl_neu_ma_all, test_data)
tb_gbl_neu_ma_all <- table(pred_gbl_neu_ma_all, test_data$neuroticism_ober_2)
F_meas(tb_gbl_neu_ma_all)
precision(tb_gbl_neu_ma_all)
recall(tb_gbl_neu_ma_all)

################

# OPENESS

# divide personalities by  (mean + sd) or (mean - sd)

op_2_1 <- new_personalities[new_personalities$openness_ober_2 == -1,]
op_2_2 = new_personalities[new_personalities$openness_ober_2 == 1,]

ober_sorted_o <- rbind(op_2_1, op_2_2)

# separate train and test sets

set.seed(78)
training_data_rows <- createDataPartition(ober_sorted_o$openness_ober_2, p = .8, list = FALSE)
train_data <- ober_sorted_o[training_data_rows,]
test_data <- ober_sorted_o[-training_data_rows,]


#PREPROC
svm_open_ma_social <- svm(~ openness_ober_2 + skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, gamma = 0.1)
pred_svm_open_ma_social <- predict(svm_open_ma_social, test_data)
tb_svm_open_ma_social <- table(as.numeric(pred_svm_open_ma_social), test_data$openness_ober_2)
F_meas(tb_svm_open_ma_social)
precision(tb_svm_open_ma_social)
recall(tb_svm_open_ma_social)

#GRAMMAR
svm_open_ma_gram <- svm(~ openness_ober_2 + m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, gamma = 0.1)
pred_svm_open_ma_gram <- predict(svm_open_ma_gram, test_data)
tb_svm_open_ma_gram <- table(as.numeric(pred_svm_open_ma_gram), test_data$openness_ober_2)
F_meas(tb_svm_open_ma_gram)
precision(tb_svm_open_ma_gram)
recall(tb_svm_open_ma_gram)

#LIWC
svm_open_ma_liwc <- svm(~ openness_ober_2 + X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, gamma = 0.1)
pred_svm_open_ma_liwc <- predict(svm_open_ma_liwc, test_data)
tb_svm_open_ma_liwc <- table(as.numeric(pred_svm_open_ma_liwc), test_data$openness_ober_2)
F_meas(tb_svm_open_ma_liwc)
precision(tb_svm_open_ma_liwc)
recall(tb_svm_open_ma_liwc)

#ALL
svm_open_ma_all <- svm(~ openness_ober_2 +  X1funct + X2pronoun + X3ppron + X4i + X5we + X6you + X7shehe + X8they + X9ipron + X10article + X11verb + X12auxverb + X13past + X14present + X15future + X16adverb + X17preps + X18conj + X19negate + X20quant + X21number + X22swear + X23social + X24family + X25friend + X26humans + X27affect + X28posemo + X29negemo + X30anx + X31anger + X32sad + X33cogmech + X34insight + X35cause + X36discrep + X37tentat + X38certain + X39inhib + X40incl + X41excl + X42percept + X43see + X44hear + X45feel + X46bio + X47body + X48health + X49sexual + X50ingest + X51relativ + X52motion + X53space + X54time + X55work + X56achieve + X57leisure + X58home + X59money + X60relig + X61death + X62assent + X63nonfl + X64filler +  skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn +  m + f + s + p + aument + dimin + superlat + N + A + PREP + CONJ + ADV + PREFIX + SIGLA + ABREV + INTERJ + DET + def + indef + NUM + numC + numO + numM + numF + PRO + proDem + proIndef + proRel + proInterr + proTrat + proPoss + proPess + acusativa + dativa + nominativa + obliqua + reflexa + p1 + p2 + p3 + V + VW + VG + VK + VP + VI + VJ + VF + VQ + VS + VT + VU + VY + VC + V1s + V2s + V3s + V1p + V2p + V3p, data = train_data, gamma = 0.1)
pred_svm_open_ma_all <- predict(svm_open_ma_all, test_data)
tb_svm_open_ma_all <- table(as.numeric(pred_svm_open_ma_all), test_data$openness_ober_2)
F_meas(tb_svm_open_ma_all)
precision(tb_svm_open_ma_all)
recall(tb_svm_open_ma_all)

# BOOSTED TREES

#PREPROC
gbm_open_ma_social <- train(as.factor(openness_ober_2) ~   skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn, data = train_data, method="gbm", verbose = FALSE)
pred_open_neu_ma_social <- predict(gbm_open_ma_social, test_data)
tb_gbm_open_so_ma <- table(pred_open_neu_ma_social, test_data$openness_ober_2)
F_meas(tb_gbm_open_so_ma)
precision(tb_gbm_open_so_ma)
recall(tb_gbm_open_so_ma)

#GRAMMAR
gbm_open_ma_gram <- train(as.factor(openness_ober_2) ~ m+f+s+p+aument+dimin+superlat+N+A+PREP+CONJ+ADV+PREFIX+SIGLA+ABREV+INTERJ+DET+def+indef+NUM+numC+numO+numM+numF+PRO+proDem+proIndef+proRel+proInterr+proTrat+proPoss+proPess+acusativa+dativa+nominativa+obliqua+reflexa+p1+p2+p3+V+VW+VG+VK+VP+VI+VJ+VF+VQ+VS+VT+VU+VY+VC+V1s+V2s+V3s+V1p+V2p+V3p, data = train_data, method="gbm", verbose = FALSE)
pred_open_neu_ma_gram <- predict(gbm_open_ma_gram, test_data)
tb_gbm_open_gra_ma <- table(pred_open_neu_ma_gram, test_data$openness_ober_2)
F_meas(tb_gbm_open_gra_ma)
precision(tb_gbm_open_gra_ma)
recall(tb_gbm_open_gra_ma)

#LIWC
gbm_open_ma_liwc <- train(as.factor(openness_ober_2) ~ X1funct + X2pronoun + X3ppron +  X4i +  X5we +  X6you +  X7shehe +  X8they +  X9ipron +  X10article +  X11verb +  X12auxverb +  X13past +  X14present +  X15future +  X16adverb +  X17preps +  X18conj +  X19negate +  X20quant +  X21number +  X22swear +  X23social +  X24family +  X25friend +  X26humans +  X27affect +  X28posemo +  X29negemo +  X30anx +  X31anger +  X32sad +  X33cogmech +  X34insight +  X35cause +  X36discrep +  X37tentat +  X38certain +  X39inhib +  X40incl +  X41excl +  X42percept +  X43see +  X44hear +  X45feel +  X46bio +  X47body +  X48health +  X49sexual +  X50ingest +  X51relativ +  X52motion +  X53space +  X54time +  X55work +  X56achieve +  X57leisure +  X58home +  X59money +  X60relig +  X61death +  X62assent +  X63nonfl +  X64filler, data = train_data, method="gbm", verbose = FALSE)
pred_gbm_open_ma_liwc <- predict(gbm_open_ma_liwc, test_data)
tb_gbm_open_ma_liwc <- table(pred_gbm_open_ma_liwc, test_data$openness_ober_2)
F_meas(tb_gbm_open_ma_liwc)
precision(tb_gbm_open_ma_liwc)
recall(tb_gbm_open_ma_liwc)

#ALL
gbl_open_ma_all <- train(as.factor(openness_ober_2) ~  X1funct + X2pronoun + X3ppron + X4i + X5we + X6you + X7shehe + X8they + X9ipron + X10article + X11verb + X12auxverb + X13past + X14present + X15future + X16adverb + X17preps + X18conj + X19negate + X20quant + X21number + X22swear + X23social + X24family + X25friend + X26humans + X27affect + X28posemo + X29negemo + X30anx + X31anger + X32sad + X33cogmech + X34insight + X35cause + X36discrep + X37tentat + X38certain + X39inhib + X40incl + X41excl + X42percept + X43see + X44hear + X45feel + X46bio + X47body + X48health + X49sexual + X50ingest + X51relativ + X52motion + X53space + X54time + X55work + X56achieve + X57leisure + X58home + X59money + X60relig + X61death + X62assent + X63nonfl + X64filler +  skip + compound + sentences + chars + allTokens + wordTokens + hashtags + links + punct + questions + exclam + numbers + upcase + lowcase + firstup + pt.lexicon + added + verb.pro + names + en.lexicon + rewrite + mispell + homograph + foreign + emo. + emo..1 + emo.. + laugh + emph + echars + unkn +  m + f + s + p + aument + dimin + superlat + N + A + PREP + CONJ + ADV + PREFIX + SIGLA + ABREV + INTERJ + DET + def + indef + NUM + numC + numO + numM + numF + PRO + proDem + proIndef + proRel + proInterr + proTrat + proPoss + proPess + acusativa + dativa + nominativa + obliqua + reflexa + p1 + p2 + p3 + V + VW + VG + VK + VP + VI + VJ + VF + VQ + VS + VT + VU + VY + VC + V1s + V2s + V3s + V1p + V2p + V3p, data = train_data, method="gbm", verbose = FALSE)
pred_gbl_open_ma_all <- predict(gbl_open_ma_all, test_data)
tb_gbl_open_ma_all <- table(pred_gbl_open_ma_all, test_data$openness_ober_2)
F_meas(tb_gbl_open_ma_all)
precision(tb_gbl_open_ma_all)
recall(tb_gbl_open_ma_all)
