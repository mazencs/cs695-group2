# global.R ##
# Put data loading and commonly used functions in this global script
# You need to install "shiny" and "shinydashboard" packages in your RStudio

library(shiny)
library(shinydashboard)
library(tm)
library(RColorBrewer)
library(wordcloud)
require(plyr)
require(stringr)
require(stringi)
library("magrittr")
library("dplyr")
library(mclust)
library("RColorBrewer")       
library(sna)
library(graph)
library(igraph)
library(readr)

# Load data set
BKdata <- readRDS("BKdata.rds")
df <- BKdata
tweets <- df$MESSAGE_BODY
tweets = as.character(tweets)

#********************************************
#         Word Cloud
#********************************************
corpus = Corpus(VectorSource(tweets))

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

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

#********************************************
#         Sentiment analysis
#********************************************
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

sentiment.scores= score.sentiment(tweets, pos.words, neg.words, .progress='text')

#********************************************
#         Network Analysis
#********************************************

screenname = df$USER_SCREEN_NAME
screenname = as.character(screenname)

# Generate edge list from tweets
galaxy <- 
  cbind(1:length(tweets),  screenname,tweets) %>% 
  set_colnames(c("id", "screenname", "tweet")) %>%
  tbl_df()

# Extracts poster information
retweeterPoster <- 
  galaxy %>%
  mutate(is_retweeted = stri_detect_regex(tweet, "(RT|via)((?:\\b\\W*@\\w+)+)")) %>%
  filter(is_retweeted) %>%
  rowwise() %>%
  do({
    # Gets retwitter
    who_retweet <- 
      stri_extract_first_regex(.$tweet, "(RT|via)((?:\\b\\W*@\\w+)+)")[[1]] %>%
      stri_extract_first_regex("@[a-zA-Z0-9_]{1,}") %>%
      stri_replace_all_fixed("@", "")
    
    # Returns pair
    data_frame(who_post = .$screenname, who_retweet = who_retweet, 
               combi = stri_c(sort(c(.$screenname, who_retweet)), collapse = " "))
  }) %>%
  ungroup() %>%
  group_by(combi) %>%
  summarize(from = min(who_post, who_retweet), 
            to = max(who_post, who_retweet), 
            weight = n()) %>%
  ungroup() %>%
  select(-combi)


# Create graph
m <- ftM2adjM(ft = as.matrix(retweeterPoster[, 1:2]), W = retweeterPoster$weight, edgemode = "directed")
g1 <- as(m, "graphNEL")


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

central %<>%
  mutate(label = ifelse(group < 9, "", as.character(central$nodes.g2.)))

# Updates node sizes
central %<>%
  mutate(size = ifelse(group == N, 3, ifelse(group < 3, 1, 2)))

# Arranges vertexes by m2
indx <- plyr::laply(colnames(m2), function(i) {which(central$nodes.g2. == i)})
central <- central[indx, ]

# Plot function
PlotGraph <- function(m, colors, sizes, labels, filename, title = "") {
  m[m == Inf] <- 0
   gplot(m, gmode = "graph", 
        label = labels,
        label.cex = 2,
        vertex.col = colors,
        vertex.enclose = FALSE,
        edge.col = "#CCCCCC",
        vertex.cex = sizes, 
        main = title,
        cex.main = 4)
  }

#********************************************
#         Consumer Profile
#********************************************

# User posting time by gender
df$days <- weekdays(as.POSIXlt(df$MESSAGE_POSTED_TIME))
dfrm <-table(df[,c("USER_GENDER","days")])

#********************************************
#         Topic Analysis
#********************************************
sports.words = scan('Sports_Word.txt', what='character', comment.char=';')

score.topic = function(sentences, dict, .progress='none')
{
 
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


