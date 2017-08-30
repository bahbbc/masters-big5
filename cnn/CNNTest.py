
# coding: utf-8

# In[1]:

import numpy
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers.embeddings import Embedding
from keras.preprocessing import sequence
# fix random seed for reproducibility
numpy.random.seed(42)
from sklearn.model_selection import cross_val_score
import pandas as pd
import gensim
from sklearn.cross_validation import train_test_split
import nltk
import logging
logging.root.handlers = []  # Jupyter messes up logging so needs a reset
logging.basicConfig(format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)


# In[2]:

from keras.datasets import imdb


# In[3]:

top_words = 5000
(X_train, y_train), (X_test, y_test) = imdb.load_data(num_words=top_words)


# In[4]:

df = pd.read_csv('~/personality-normalized-word2vec-norm.csv', encoding='utf-8')
df.shape


# In[5]:

index = pd.isnull(df.formatted_text)
df.loc[index, 'formatted_text'] = ''


# In[7]:

w2v = gensim.models.Word2Vec.load('/home/bahbbc/workspace/masters-big5/models/tweet50-600.model')
w2v.init_sims(replace=True)


# In[8]:

num_features= 600


# In[ ]:

train_w2v_data, test_w2v_data = train_test_split(df, test_size=0.3, random_state=42)


# In[ ]:

# create the model
embedding_vecor_length = 32
model = Sequential()
model.add(Embedding(w2v.wv.vocab.items(), input_length=w2v.wv.syn0.shape[0], output_dim=w2v.wv.syn0.shape[1]))
#model.add(Embedding(, , , weights=[weights]))
model.add(LSTM(100))
model.add(Dense(1, activation='sigmoid'))
model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
print(model.summary())
model.fit(train_w2v_data, train_w2v_data['extraversion_m'], validation_data=(test_w2v_data, test_w2v_data['extraversion_m']), epochs=3, batch_size=64)

