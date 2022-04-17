
############################################################
# 1. Text analytics for analyzing President Trump's tweets. 
############################################################

# Recall we want to predict if a tweet was posted from an
# Android (Trump) or an iPhone (Trump's staff). 

# Install/load the required packages. 
# install.packages("tm")
# install.packages("SnowballC")
library(tidyverse)
library(tm) # Will use to create corpus and modify text therein.
library(SnowballC) # Will use for "stemming." 
library(rpart) # Will use to construct a CART model.
library(rpart.plot) # Will use to plot CART tree.

# Load the dataset (trump tweets posted between 6/1/15 and 3/1/17).
tweets = read.csv("trump_tweets.csv", stringsAsFactors = FALSE, encoding="UTF-8")

# Let us recall the structure of the dataset.
str(tweets)
# Note that if TrumpWrote = 1 in an observation, then the 
# corresponding tweet was posted from an Android. 

# Let us also see how many tweets President Trump posted.   
table(tweets$TrumpWrote)

# Next we will manipulate the text in President Trump's tweets. To this end, 
# we will create a "corpus." 
corpus = Corpus(VectorSource(tweets$text)) # An array of document
corpus

# The tweets in the corpus are called "documents."
# corpus[1]     # subset
corpus[[1]]   # individual doc
strwrap(corpus[[1]])
str(corpus[[3]])

# Let us start processing the text in the corpus! Here is a 
# summary of how we shall process the text.  
# 1. Change all the text to lower case.  
# 2. Remove stop words and particular words. 
# 3. "Stem" the documents. 
# 4. Remove infrequent words. 
# 5. Create new data frame that contains word counts. 

# 1. Let's change all the text to lower case. 
corpus = tm_map(corpus, tolower)
strwrap(corpus[[1]])
# The function tm_map applies an operation to every document in the corpus. 
# In this case, the operation is 'tolower" (i.e. to lowercase). 

# We now transform a link of the form "https://link" into "http link" to make sure
# http appears as a word
# We first define a new function f from gsub (substitute text):
# f is essentially the same as gsub except for the order of arguments
f <- content_transformer(function(doc, oldtext, newtext) gsub(oldtext, newtext, doc)) 
corpus <- tm_map(corpus, f, "https://", "http ")  # f(each doc in corpus, "https://", "http ")
strwrap(corpus[[3]])

# Last, we remove punctuation from the document
corpus <- tm_map(corpus, removePunctuation)
strwrap(corpus[[1]])

# 2. Let us remove some words. First, we remove stop words:  
corpus <- tm_map(corpus, removeWords, stopwords("english"))  # removeWords(corpus,stopwords("english"))
# stopwords("english") is a dataframe that constains a list of 
# stop words. Let us look at the first ten stop words. 
stopwords("english")[1:10]

# Checking again:  
strwrap(corpus[[1]])

# Next, we remove the three particular words: realdonaldtrump, donaldtrump, donaldjtrumpjr.
# This list can be customized depending on the application context
strwrap(corpus[[4]])
corpus <- tm_map(corpus, removeWords, c("realdonaldtrump", "donaldtrump", "donaldjtrumpjr"))
strwrap(corpus[[4]])

# 3. Now we stem our documents. Recall that this corresponds toremoving the parts of words
# that are in some sense not necessary (e.g. 'ing' and 'ed'). 
corpus <- tm_map(corpus, stemDocument)

# We have: 
strwrap(corpus[[1]])

# 4. Let us "sparsify" the corpus and remove infrequent words. 
# First, we calculate the frequency of each words over all tweets. 
frequencies = DocumentTermMatrix(corpus)
frequencies 

# documents as the rows, terms as the columns
# Let us get a feel for what words occur the most. Words that appear at least 200 times: 
findFreqTerms(frequencies, lowfreq=200)
# Words that appear at least 100 times: 
findFreqTerms(frequencies, lowfreq=100)
# Let us only keep terms that appear in at least 1% of the tweets. We create a list of these words as follows. 
sparse = removeSparseTerms(frequencies, 0.99)  # 0.99: maximal allowed sparsity 
sparse # We now have 172 terms instead of 12,093

# 5. We first create a new data frame. Each variable corresponds to one of the 172 words, and each row corresponds to one of the tweets.
document_terms = as.data.frame(as.matrix(sparse))
str(document_terms)
# Lastly, we create a new column for the dependent variable: 
document_terms$TrumpWrote = tweets$TrumpWrote

# We have processed our data! Let us briefly construct a CART model. 

# Training and test set.
split1 = (tweets$created_at < "2016-06-01")
split2 = (tweets$created_at >= "2016-06-01")
train = document_terms[split1,]
test = document_terms[split2,]

# Constructing the logistic regression model
logreg = glm(TrumpWrote ~., data=train, family="binomial")
summary(logreg)

# Assessing the out-of-sample performance of the logistic regression model
predictions.logreg <- predict(logreg, newdata=test, type="response")
matrix.logreg = table(test$TrumpWrote, predictions.logreg > 0.5)   # threshold = 0.5
matrix.logreg    # confusion matrix
accuracy.logreg = (matrix.logreg[1,1]+matrix.logreg[2,2])/nrow(test)
TPR.logreg = (matrix.logreg[2,2])/sum(matrix.logreg[2,])
FPR.logreg = (matrix.logreg[1,2])/sum(matrix.logreg[1,])

# Constructing and plotting the CART model.
cart = rpart(TrumpWrote ~ ., data=train, method="class", cp = .003)  # classification
prp(cart, type =2, extra =104)

# Assessing the out-of-sample performance of the CART model
predictions.cart <- predict(cart, newdata=test, type="class")
matrix.cart = table(test$TrumpWrote, predictions.cart) # confusion matrix
accuracy.cart = (matrix.cart[1,1]+matrix.cart[2,2])/nrow(test)
TPR.cart = (matrix.cart[2,2])/sum(matrix.cart[2,])
FPR.cart = (matrix.cart[1,2])/sum(matrix.cart[1,])
