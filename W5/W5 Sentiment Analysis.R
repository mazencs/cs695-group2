#install the necessary packages
install.packages("readr")
install.packages("tm")
install.packages("RColorBrewer")
install.packages("wordcloud")
install.packages("plyr")
install.packages("stringr")
install.packages("googleVis")
install.packages("stringi")
install.packages("magrittr")
install.packages("dplyr")


library(readr)
gear <- read_delim("Samsung Tweets.csv", ";", escape_double = FALSE, trim_ws = TRUE)
geartweets <- gear$tweettext


#********************************************
#         Sentiment Analysis
#********************************************
#R's c() function (for "combine") allows us to add a few industry- and Twitter-specific terms to form our final pos.words and neg.words vectors:

pos.words = scan('positive-words.txt', what='character', comment.char=';')
neg.words = scan('negative-words.txt', what='character', comment.char=';')

neg.words = c(neg.words, 'wtf', 'fail')

#Implementing our sentiment scoring algorithm
require(plyr)
require(stringr)
require(stringi)

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

sentiment.scores= score.sentiment(geartweets, pos.words, neg.words, .progress='text')

# Create Histogram of sentiment scores
score= sentiment.scores$score
hist(score)
nonZeroscore = subset(score,score!=0)
sentiscore = data.frame(nonZeroscore)

library("googleVis")
Hist<- gvisHistogram(sentiscore, options=list(
  legend="{ position: 'top', maxLines:2 }",
  colors="['#5C3292', '#1A8763', '#871B47']",
  width=400, height=360))
plot(Hist)

# Create pie chart of sentiment scores

negScore = subset(score, score < 0)
posScore = subset(score, score > 0)
neuScore = subset(score, score == 0)

negNum= length(negScore)
posNum= length(posScore)
neuNum= length(neuScore)

dftemp=data.frame(topic=c("Negative", "Positive", "Neutral"), 
                  number=c(negNum,posNum,neuNum))

Pie <- gvisPieChart(dftemp)
plot(Pie)