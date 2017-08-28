
# coding: utf-8

# In[3]:

import gensim
import logging
logging.root.handlers = []  # Jupyter messes up logging so needs a reset
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)
from smart_open import smart_open
import pandas as pd
import numpy as np
from numpy import random
import gensim
import nltk
from sklearn.cross_validation import train_test_split
from sklearn import linear_model
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer
from sklearn.metrics import accuracy_score, confusion_matrix
from gensim.models import Word2Vec
from sklearn.neighbors import KNeighborsClassifier
from sklearn import linear_model
from nltk.corpus import stopwords
import os, codecs
from bs4 import BeautifulSoup
import re
from nltk.corpus import stopwords
from sklearn import metrics as skmetrics
from sklearn.model_selection import cross_val_score
from sklearn.ensemble import RandomForestClassifier


# In[4]:

model = gensim.models.Word2Vec.load("/home/bahbbc/workspace/masters-big5/wiki.pt-br.word2vec.model")
model.init_sims(replace=True)


# In[5]:

num_features = 400


# ### Verify model with personality

# In[6]:

df = pd.read_csv('~/personality-normalized-single-file-word2vec.csv', encoding='utf-8')
df.shape


# In[7]:

df.extraversion_m.value_counts()


# ### Word2Vec model training

# In[8]:

def word_averaging(wv, words):
    all_words, mean = set(), []
    #print words.shape
    for word in words:
        if isinstance(word, np.ndarray):
            mean.append(word)
        elif word in wv.wv.vocab:
            mean.append(wv.wv.syn0norm[wv.wv.vocab[word].index])
            all_words.add(wv.wv.vocab[word].index)
        #print mean

    if not mean:
        logging.warning("cannot compute similarity with no input %s", words)
        # FIXME: remove these examples in pre-processing
        return np.zeros(num_features,)

    mean = gensim.matutils.unitvec(np.array(mean).mean(axis=0)).astype(np.float32)
    return mean

def  word_averaging_list(wv, text_list):
    return np.vstack([word_averaging(wv, review) for review in text_list ])


# In[9]:

def w2v_tokenize_text(text):
    tokens = []
    if text is np.nan:
        return []
    for sent in nltk.sent_tokenize(text, language='portuguese'):
        for word in nltk.word_tokenize(sent, language='portuguese'):
            if len(word) < 2:
                continue
            tokens.append(word)
    return tokens


# In[10]:

data = df.apply(lambda r: w2v_tokenize_text(r['formatted_text']), axis=1).values


# In[ ]:

data_word_average = word_averaging_list(model, data)


# In[ ]:

clf = RandomForestClassifier(max_depth=2, random_state=42, criterion= "gini", class_weight="balanced")


# ## Extraversion

# In[ ]:

scores = cross_val_score(clf, data_word_average, df['extraversion_m'], cv=10, scoring='f1_macro')


# In[ ]:

print("F1-score: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))


# ## Agreableness

# In[ ]:

scores = cross_val_score(clf, data_word_average, df['agreeabeness_m'], cv=10, scoring='f1_macro')


# In[ ]:

print("F1-score: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))


# ## Consciousness

# In[ ]:

scores = cross_val_score(clf, data_word_average, df['conscientiousness_m'], cv=10, scoring='f1_macro')


# In[ ]:

print("F1-score: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))


# ## Neuroticism

# In[ ]:

scores = cross_val_score(clf, data_word_average, df['neuroticism_m'], cv=10, scoring='f1_macro')


# In[ ]:

print("F1-score: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))


# ## Openess

# In[ ]:

scores = cross_val_score(clf, data_word_average, df['openness_m'], cv=10, scoring='f1_macro')


# In[ ]:

print("F1-score: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))

