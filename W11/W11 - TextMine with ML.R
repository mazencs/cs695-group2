### Sentiment analysis of tweets with machine learning ####

install.packages('RTextTools')
install.packages('e1071')
install.packages('readr')
install.packages('tidyverse')
install.packages('text2vec')
install.packages('ggrepel')
install.packages('googleVis')
library(googleVis)
library(tidyverse)
library(text2vec)
library(ggrepel)
library(RTextTools)
library(e1071)
library(readr)

###################################################
### Section 1: Example of traning and testing data

# load Twitter data for training and testing
Twitterdata <- read_csv("Twitterdata.csv")
# load data for predicting sentiment
Trump <- read_csv("TrumpTweets.csv")

tweet_all = c(Twitterdata$tweet,Trump$tweet)

# label data
sentiment_all = as.factor(Twitterdata$sentiment)

# create matrix
mat= create_matrix(tweet_all, language="english", 
                   removeStopwords=FALSE, removeNumbers=TRUE, 
                   stemWords=FALSE, tm::weightTfIdf)

# create container for machine learning: trainning data size-400, testing data size-98
container = create_container(mat, as.numeric(sentiment_all),
                             trainSize=1:400, testSize=401:498,virgin=FALSE) 

# Train with several machine learning algorithms
# Be patient !!!
# Compare accuracy of different models
N=5
cross_validate(container,N,"SVM")
cross_validate(container,N,"TREE")
cross_validate(container,N,"RF")
cross_validate(container,N,"BAGGING")

#******************************************
### Section 2: Classify Twitter tweets by reusing a trained learning model

# loading the set of tweets for sentiment analysis
# Replace the data with your term project data

Trump <- read_csv("Trump.csv")
prep_fun <- tolower
tok_fun <- word_tokenizer
it_tweets <- itoken(Trump$MESSAGE_BODY,
                    preprocessor = prep_fun,
                    tokenizer = tok_fun,
                    #  ids = Trump$X1,
                    progressbar = TRUE)

# loading and reusig vocabulary and document-term matrix
vectorizer <- readRDS('TwSentiVectorizer.RDS')
dtm_tweets <- create_dtm(it_tweets, vectorizer)

# define tf-idf model
tfidf <- TfIdf$new()

# transforming data with tf-idf
dtm_tweets_tfidf <- fit_transform(dtm_tweets, tfidf)

# loading and reusing classification model
classifier <- readRDS('TwSentiClassifier.RDS')

# predict probabilities of positiveness
preds_tweets <- predict(classifier, dtm_tweets_tfidf, type = 'response')[ ,1]

# adding sentiment ratings to the dataset
Trump$sentiment <- preds_tweets

# define positive as sentiment value greater than 0.65, negative as the value less than 0.35
numPositive= nrow(subset(Trump, sentiment > 0.65))
numNegative= nrow(subset(Trump, sentiment < 0.35))
numNeutral = nrow(Trump) - numPositive-numNegative
dftemp=data.frame(topic=c("Positive", "Negative", "Neutral"), 
                  number=c(numPositive,numNegative, numNeutral))

Pie <- gvisPieChart(dftemp)
plot(Pie)

