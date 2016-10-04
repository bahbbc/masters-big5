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

extra_sorted_m <- rbind(op_1, op_2)
extra_sorted_o <- rbind(op_2_1, op_2_2)

paste(dim(extra_sorted_o)[1], dim(extra_sorted_m)[1])