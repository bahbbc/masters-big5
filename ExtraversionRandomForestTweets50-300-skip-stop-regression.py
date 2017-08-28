
# coding: utf-8

# In[163]:

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
import scipy.stats


# In[164]:

model = gensim.models.Word2Vec.load('/home/bahbbc/workspace/masters-big5/models/tweet50-300-skip.model')
model.init_sims(replace=True)


# In[165]:

num_features= 300


# In[166]:

df = pd.read_csv('~/personality-normalized-single-file-word2vec.csv', encoding='utf-8')
df.shape


# In[167]:

train_w2v_data, test_w2v_data = train_test_split(df, test_size=0.3, random_state=42)


# In[168]:

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


# In[169]:

def w2v_tokenize_text(text):
    tokens = []
    if text is np.nan:
        return []
    for sent in nltk.sent_tokenize(text):
        for word in nltk.word_tokenize(sent):
            if len(word) < 2:
                continue
            if word in stopwords.words('portuguese'):
                continue
            tokens.append(word)
    return tokens


# In[170]:

test_tokenized = test_w2v_data.apply(lambda r: w2v_tokenize_text(r['formatted_text']), axis=1).values
train_tokenized = train_w2v_data.apply(lambda r: w2v_tokenize_text(r['formatted_text']), axis=1).values


# In[171]:

X_train_word_average = word_averaging_list(model,train_tokenized)
X_test_word_average = word_averaging_list(model,test_tokenized)


# In[172]:

del model


# In[173]:

X_train_word_average.shape


# In[174]:

from sklearn.model_selection import RandomizedSearchCV
from sklearn.linear_model import LinearRegression
from sklearn.tree import DecisionTreeRegressor
from sklearn.externals import joblib

force = False
model_trainer = RandomizedSearchCV(
    n_iter=1,
    estimator=DecisionTreeRegressor(),
    param_distributions={
        "max_features": ["log2"],
        "random_state": [42],
        "criterion":['mse']
    },
    verbose=True,
    refit=True,
    cv=10,
    n_jobs=-1
)


# ## Extraversion evaluation

# In[175]:

df.extraversion.astype('int').value_counts()


# In[176]:


# In[178]:

model_trainer.fit(X_train_word_average, train_w2v_data['extraversion'])
model = model_trainer.best_estimator_


# In[179]:

yp = model.predict(X_test_word_average)
yt = test_w2v_data['extraversion']


# In[180]:

(yt > 5).sum()


# In[181]:

yp[:10]


# In[182]:

yt[:10]
print 'extraversion'

# In[183]:
print 'mean_squared_error'
print skmetrics.mean_squared_error(yt, yp)


# In[184]:
print 'r2_score'
print skmetrics.r2_score(yt, yp)


# In[185]:
print 'pearsonr'
print scipy.stats.pearsonr(yt,yp)


# ## Agreableness evaluation

# In[186]:


# In[188]:

model_trainer.fit(X_train_word_average, train_w2v_data['agreeableness'])
model = model_trainer.best_estimator_


# In[189]:

yp = model.predict(X_test_word_average)
yt = test_w2v_data['agreeableness']


# In[190]:

(yt > 5).sum()

print 'agreeableness'
# In[191]:
print 'mean_squared_error'
print skmetrics.mean_squared_error(yt, yp)


# In[192]:
print 'r2_score'
print skmetrics.r2_score(yt, yp)


# In[193]:
print 'pearsonr'
print scipy.stats.pearsonr(yt,yp)


# ## Conscientiousness evaluation

# In[194]:


# In[196]:

model_trainer.fit(X_train_word_average, train_w2v_data['conscientiousness'])
model = model_trainer.best_estimator_


# In[197]:

yp = model.predict(X_test_word_average)
yt = test_w2v_data['conscientiousness']

print 'Conscientiousness'

# In[198]:
print 'mean_squared_error'
print skmetrics.mean_squared_error(yt, yp)


# In[199]:
print 'r2_score'
print skmetrics.r2_score(yt, yp)


# In[200]:
print 'pearsonr'
print scipy.stats.pearsonr(yt,yp)


# ## Neuroticism evaluation

# In[201]:


# In[203]:

model_trainer.fit(X_train_word_average, train_w2v_data['neuroticism'])
model = model_trainer.best_estimator_


# In[204]:

yp = model.predict(X_test_word_average)
yt = test_w2v_data['neuroticism']


# In[205]:

(yt > 5).sum()

print 'Neuroticism'

# In[206]:
print 'mean_squared_error'
print skmetrics.mean_squared_error(yt, yp)


# In[207]:
print 'r2_score'
print skmetrics.r2_score(yt, yp)


# In[208]:
print 'pearsonr'
print scipy.stats.pearsonr(yt,yp)


# ## Openess evaluation

# In[209]:


# In[211]:

model_trainer.fit(X_train_word_average, train_w2v_data['openness'])
model = model_trainer.best_estimator_


# In[212]:

yp = model.predict(X_test_word_average)
yt = test_w2v_data['openness']


# In[213]:

(yt > 5).sum()

print 'openess'

# In[214]:
print 'Mean squared error'
print skmetrics.mean_squared_error(yt, yp)


# In[215]:
print 'r2_score'
print skmetrics.r2_score(yt, yp)


# In[216]:
print 'pearsonr'
print scipy.stats.pearsonr(yt,yp)

