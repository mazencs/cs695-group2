# R scripts for CS695 term project. 
# Every group member should contribute to this scripts
# I will track and grade student contribution through Git

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

# 1 - Load data



# 2 - Word Cloud

library('tm')
library('RColorBrewer')
library('wordcloud')

cmail <- readRDS("merola.RDS")

#********************************************
#         Word Cloud
#********************************************
corpus = Corpus(VectorSource(tweets))
corpus = Corpus(VectorSource(cmail))
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
head(word_freqs, 100)

#remove the top words which don’t generate insights such as "the", "a", "and", etc.
word_freqs = word_freqs[-(1)]  #Here “1” is 1st word in the list we want to remove 

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

#Plot corpus in a clored graph; need RColorBrewer package
wordcloud(head(dm$word, 50), head(dm$freq, 50), random.order=FALSE, colors=brewer.pal(8, "Dark2"))



# 3 - Topic Classification
sports.words = scan('merola.RDS', what='character', comment.char=';')

score.topic = function(sentences, dict, .progress='none')
{
  require(plyr)
  require(stringr)
  require(stringi)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, dict) {
    
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
    topic.matches = match(words, dict)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    topic.matches = !is.na(topic.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(topic.matches)
    
    return(score)
  }, dict, .progress=.progress )
  
  topicscores.df = data.frame(score=scores, text=sentences)
  return(topicscores.df)
}

topic.scores= score.topic(tweets, sports.words, .progress='text')
topic.mentioned = subset(topic.scores, score !=0)

N= nrow(topic.scores)
Nmentioned = nrow(topic.mentioned)

dftemp=data.frame(topic=c("Mentioned", "Not Mentioned"), 
                  number=c(Nmentioned,N-Nmentioned))


library("googleVis")

Pie <- gvisPieChart(dftemp)
plot(Pie)




# 4 - Sentiment Analysis

library(readr)
gear <- readRDS("merola.RDS",";", escape_double = FALSE, trim_ws = TRUE)
geartweets <- gear$tweettext


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
negNum= length(negScore)
posNum= length(posScore)

dftemp=data.frame(topic=c("Negative", "Positive"), 
                  number=c(negNum,posNum))

Pie <- gvisPieChart(dftemp)
plot(Pie)

# 5 - User Profile

mer <- readRDS("merola.RDS")

# Show the distribution of users by location (city) with Google Map
# !!! It takes time to show all the locations, so be patient !!!
mer$city <- tolower(mer$USER_CITY)
citySum = data.frame(table(mer$city))

GeoStates <- gvisGeoChart(citySum, "Var1", "Freq",
                          options=list(region="US", 
                                       #  displayMode="regions", 
                                       displayMode="markers",
                                       resolution="provinces",
                                       width=600, height=400))
plot(GeoStates)

# User gender distribution by city
# Get subset of the data to explore
subdf <- subset(mer, city =='new york city' | city =='brooklyn')
genderCity <-data.frame(table(subdf[,c("city", "USER_GENDER")]))
# xtabs(Freq~city+USER_GENDER,genderCity)
tab = reshape(genderCity,direction="wide",timevar="USER_GENDER",idvar="city")
Column <- gvisColumnChart(tab)
plot(Column)


# User posting time by gender
mer$days <- weekdays(as.POSIXlt(mer$MESSAGE_POSTED_TIME))
dfrm <-data.frame(table(mer[,c("USER_GENDER","days")]))
genderDays = reshape(dfrm,direction="wide",timevar="days",idvar="USER_GENDER")
Bar <- gvisBarChart(genderDays)
plot(Bar)





# 6 - Network Analysis
install.packages("mclust")
install.packages("igraph")
install.packages("sna")
source("http://bioconductor.org/biocLite.R")
biocLite("RBGL")
biocLite("graph")

library(mclust)
library("RColorBrewer")       
library(sna)
library(graph)
library(igraph)
library(readr)
library("plyr")
library("stringr")
library("stringi")
library("magrittr")
library("dplyr")


# Load data
mer <- readRDS("merola.RDS")
tweets = mer$MESSAGE_BODY
tweets = as.character(tweets)
screenname = mer$USER_SCREEN_NAME
screenname = as.character(screenname)
write.csv(cbind(screenname,tweets), "tweets.csv")

# Generate edge list from tweets
source("createList.R")
retweeterPoster <- createList("tweets.csv")

# Create graph
m <- ftM2adjM(ft = as.matrix(retweeterPoster[, 1:2]), W = retweeterPoster$weight, edgemode = "directed")
g1 <- as(m, "graphNEL")

# Plots initial graph
gplot(m, gmode = "graph", 
      label = nodes(g1),
      label.cex = 0,
      vertex.col = "#A63603",
      vertex.enclose = FALSE,
      edge.col = "#CCCCCC",
      vertex.cex = 2, 
      main = "Original graph",
      cex.main = 1)

# Prune graph 
# Original graph has too many small clusters, so we need to exclude small clusters
clust <- igraph::components(graph_from_adjacency_matrix(m), mode = "weak")
table(clust$csize)
# The largest cluster contains 891 nodes, so we create a graph of it
large_clusters <- which(clust$csize > 800)
selected_nodes <- names(clust$membership[clust$membership %in% large_clusters])
selected_nodes <- which(rownames(m) %in% selected_nodes)
m2 <- m[selected_nodes, selected_nodes]
gfrom2 <- graph_from_adjacency_matrix(m2)
g2 <- as(m2, "graphNEL")

# centrality measurements of new graph
require(sna) # to mask centrality functions
central <- data.frame(nodes(g2))
central$betweenness <-  sna::betweenness(m2)
central$degree <- sna::degree(m2)
sortlist <- central[order(-central$degree),]
head(sortlist, 10)


#**************************
# Plot Pruned Graph
# The largest cluster still has too many nodes, so we only label
# the most important ones based on centrality scores
#***************************

# Clasterize betweenness values to get groups of nodes 
central %<>%
  mutate(size = log(central$betweenness)) %>%
  mutate(size = ifelse(size == -Inf, 1, size))

# Number of groups for colors
N <- 9
# Colors for nodes
pal <- brewer.pal(N, "Oranges")

# Defines clusters for nodes in groups with different colors
central %<>%
  mutate(group = Mclust(size, G = N)$classification,
         color = pal[group])

# Removes labels for small nodes
# central$label = as.character(central$nodes.g2.)
# central$label[central$group < 7] = ''
central %<>%
  mutate(label = ifelse(group < 7, "", as.character(central$nodes.g2.)))

# Updates node sizes
central %<>%
  mutate(size = ifelse(group == N, 5, ifelse(group < 3, 1, 2)))

# Arranges vertexes by m2
indx <- plyr::laply(colnames(m2), function(i) {which(central$nodes.g2. == i)})
central <- central[indx, ]


# Plot function
PlotGraph <- function(m, colors, sizes, labels, filename, title = "") {
  m[m == Inf] <- 0
  png(filename = filename, width = 2048, height = 2048)
  gplot(m, gmode = "graph", 
        label = labels,
        label.cex = 2,
        vertex.col = colors,
        vertex.enclose = FALSE,
        edge.col = "#CCCCCC",
        vertex.cex = sizes, 
        main = title,
        cex.main = 4)
  dev.off()
}

# Plot graph with by centrality scores and save the image
set.seed(1)
PlotGraph(m2, 
          colors = central$color,
          sizes = central$size,
          labels = central$label,
          filename = "bybBetwenness.png",
          title = "Pruned Graph by Centrality")




