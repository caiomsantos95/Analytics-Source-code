##WORD CLOUD
library(wordcloud2)
# Positive reviews
matrix_pos = as.matrix(TermDocumentMatrix(
  corpus[reviews$review_scores_rating>3] ))
df_pos = data.frame(word = row.names(matrix_pos), freq=rowSums(matrix_pos))
wordcloud2(df_pos[order(df_pos$freq, decreasing=TRUE),])

# Negative reviews
matrix_neg = as.matrix(TermDocumentMatrix(
  corpus[reviews$review_scores_rating<=3] ))
df_neg = data.frame(word = row.names(matrix_neg), freq=rowSums(matrix_neg))
wordcloud2(df_neg[order(df_neg$freq, decreasing=TRUE),])

######################################################
##### SUPPORTING TRANSFORMATIONS FOR GETTING CORPUS###
######################################################

# import AirBnB data
reviews = read.csv("airbnbReviews.csv", stringsAsFactors = FALSE, encoding="UTF-8")

###1a
skim(reviews)
head(reviews)
score_na <- reviews %>% count(is.na(reviews$review_scores_rating))

#### C TRANSFORMS INTO VECTOR
x <- c(reviews$review_scores_rating)
tabulate(x)

###DOUBLE CHECK
five <- reviews %>% filter(review_scores_rating ==5)
skim(five)


###1b - LENGHT OF EACH STRING IN THE COMMENTS SECTION
# length of each string
nchar(reviews$comments)

###1b
# Add column - CBIND FUNCTION
reviews <- cbind(reviews, review_length = nchar(reviews$comments))     
mean_lenght <- aggregate(reviews$review_length, list(reviews$review_scores_rating), mean)
mean_lenght

###1c - CREATE A CORPUS FOR THE COMMENTS
corpus <- Corpus(VectorSource(reviews$comments))
corpus

#### CORPUS TRANSFORMATION
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, c("airbnb","apartment","location","place","room","host","stay"))
corpus <- tm_map(corpus, stemDocument) ### removes part of words that are necessary - maintains only the root
reviews[1,]
strwrap(corpus[[1]])