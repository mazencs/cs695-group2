## global.R ##
# Put data loading and commonly used functions in this global script
# You need to install "shiny" and "shinydashboard" packages in your RStudio

library(shiny)
library(shinydashboard)
library(tm)
library(RColorBrewer)
library(wordcloud)
require(plyr)
require(stringr)

# Load data set
BKdata <- readRDS("BKdata.rds")
df <- BKdata
tweete <- df$MESSAGE_BODY

# Word cloud
corpus = Corpus(VectorSource(tweete))

# create term-document matrix
tdm = TermDocumentMatrix(
  corpus,
  control = list(
    wordLengths=c(3,20),
    removePunctuation = TRUE,
    stopwords = c("the", "a", stopwords("english")),
    removeNumbers = TRUE, tolower = TRUE) )

# convert as matrix
tdm = as.matrix(tdm)

# get word counts in decreasing order
word_freqs = sort(rowSums(tdm), decreasing=TRUE) 

#check top 50 most mentioned words
head(word_freqs, 50)

#remove the top words which don’t generate insights such as "the", "a", "and", etc.
word_freqs = word_freqs[-(1)]  #Here “1” is 1st word in the list we want to remove 

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# Sentiment analysis

pos.words = scan('positive-words.txt', what='character', comment.char=';')
neg.words = scan('negative-words.txt', what='character', comment.char=';')
neg.words = c(neg.words, 'wtf', 'fail')

#Implementing our sentiment scoring algorithm
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

sentiment.scores= score.sentiment(tweete, pos.words, neg.words, .progress='text')
