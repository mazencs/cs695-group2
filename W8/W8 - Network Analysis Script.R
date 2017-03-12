#install the necessary packages
install.packages("mclust")
install.packages("plyr")
install.packages("stringr")
install.packages("igraph")
install.packages("stringi")
install.packages("magrittr")
install.packages("dplyr")
install.packages("sna")
install.packages("RColorBrewer")
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

#********************************************
#         Social Network Analysis
#********************************************

# Load data
Trump <- read_csv("Trump.csv")
tweets = Trump$MESSAGE_BODY
tweets = as.character(tweets)
screenname = Trump$USER_SCREEN_NAME
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

