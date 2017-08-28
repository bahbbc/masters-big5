
# coding: utf-8

# In[1]:

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
#import matplotlib.pyplot as plt
from gensim.models import Word2Vec
from sklearn.neighbors import KNeighborsClassifier
from sklearn import linear_model
from nltk.corpus import stopwords
import os, codecs
#get_ipython().magic('matplotlib inline')
from bs4 import BeautifulSoup
import re
from nltk.corpus import stopwords
from sklearn import metrics as skmetrics


# ## Model 1 evaluation

# In[2]:

model = gensim.models.Word2Vec.load('/home/bahbbc/workspace/masters-big5/models/tweet50-600.model')
model.init_sims(replace=True)


# In[3]:

num_features= 600


# In[4]:

df = pd.read_csv('~/personality-normalized-single-file-word2vec.csv', encoding='utf-8')
df.shape


# In[5]:

df.agreeabeness_m.value_counts()


# In[6]:

train_w2v_data, test_w2v_data = train_test_split(df, test_size=0.5, random_state=42)


# In[8]:

def predict(vectorizer, classifier, data):
    data_features = vectorizer.transform(data['formatted_text'])
    predictions = classifier.predict(data_features)
    target = data['agreeabeness_m']
    evaluate_prediction(predictions, target)


# In[9]:

def word_averaging(wv, words):
    all_words, mean = set(), []

    for word in words:
        if isinstance(word, np.ndarray):
            mean.append(word)
        elif word in wv.wv.vocab:
            mean.append(wv.wv.syn0norm[wv.wv.vocab[word].index])
            all_words.add(wv.wv.vocab[word].index)

    if not mean:
        logging.warning("cannot compute similarity with no input %s", words)
        # FIXME: remove these examples in pre-processing
        return np.zeros(num_features,)

    mean = gensim.matutils.unitvec(np.array(mean).mean(axis=0)).astype(np.float32)
    return mean

def  word_averaging_list(wv, text_list):
    return np.vstack([word_averaging(wv, review) for review in text_list ])


# In[10]:

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


# In[11]:

test_tokenized = test_w2v_data.apply(lambda r: w2v_tokenize_text(r['formatted_text']), axis=1).values
train_tokenized = train_w2v_data.apply(lambda r: w2v_tokenize_text(r['formatted_text']), axis=1).values


# In[12]:

X_train_word_average = word_averaging_list(model,train_tokenized)
X_test_word_average = word_averaging_list(model,test_tokenized)


# In[13]:

del model


# In[14]:

from sklearn.model_selection import RandomizedSearchCV
from sklearn.ensemble import RandomForestClassifier
from sklearn.externals import joblib

force = False
model_trainer = RandomizedSearchCV(
    n_iter=1,
    estimator=RandomForestClassifier(),
    param_distributions={
        "criterion": ["gini"],
        #"n_estimators": [1000],
        "max_features": ["log2"],
        "max_depth": [None],
        "bootstrap": [True],
        #"oob_score": [True],
        "class_weight": ["balanced"],
    },
    scoring="f1",
    verbose=True,
    refit=True,
    cv=10,
    n_jobs=-1
)


# In[15]:

model_trainer.fit(X_train_word_average, train_w2v_data['agreeabeness_ober'])
model = model_trainer.best_estimator_


# In[16]:

yp = model.predict(X_test_word_average)
yt = test_w2v_data['agreeabeness_ober']


# In[17]:

pd.DataFrame(
    index=pd.Index([0, 1], name="y_true"),
    columns=pd.Index([0, 1], name="y_pred"),
    data=skmetrics.confusion_matrix(y_true=yt, y_pred=yp)
)


# In[18]:

print skmetrics.classification_report(y_true=yt, y_pred=yp)


# In[19]:

accuracy_score(yt, yp)


# In[20]:

print '--------- TRAIN -----------------'


# In[21]:

ytp = model.predict(X_train_word_average)
ytt = train_w2v_data['extraversion_m']


# In[22]:

print skmetrics.classification_report(y_true=ytt, y_pred=ytp)


# In[23]:

print accuracy_score(ytt, ytp)

