# import Library
library(igraph)

# read the graph analytics project dataset
graphAnalyticsDataset <- read.delim("~/soc-Epinions1_adj(1).tsv", sep = "\t", header = FALSE)

#View the dataset
View(graphAnalyticsDataset)

# Make a matrix from the dataset
optab = as.matrix(graphAnalyticsDataset)

# Make Vectors V1 and V2

v1 <- optab[1:811480, 1]
v2 <- optab[1:811480, 1]

# Make DataFrames
relations = data.frame(from=v1, v2)
g <- graph_from_data_frame(relations, directed = TRUE)
plot(g)

# Cleaning up the blob graph using brute force

myGV <- igraph :: V(g)

myGV1 <- igraph :: V(g)[igraph::degree(g) < 1]

myGV1

myGV10 <- igraph :: V(g)[igraph::degree(g) < 10]

myGV10

newGraph <- igraph::delete_vertices(g, myGV10) 
newGraph

plot(newGraph)

newGV0 <- igraph::V(newGraph)[igraph:: degree(newGraph) < 1]

newGV0

newG2 <- igraph:: delete.vertices(newGraph, newGV0)
newG2

plot(newG2)

newGV50 <- v(newGraph)[igraph::degree(newGraph)<50]
newGV50

newGraph3 <- igraph::delete.vertices(newGraph, newGV50)
plot(newGraph3)


# Plot the graph with first 200 data points

df200 <- relations[1:200,]

df200

myGraph200 <- igraph:: graph_from_data_frame(df200)
plot(myGraph200)

plot.igraph(myGraph200)

# Str func 
str(myGraph200)

#install.packages("sna")
library(sna)

agraph <- rgraph(n=100, m=1, tprob=0.5, diag=FALSE)
agraph

agraph1 <- rgraph(n=100, m=1, tprob=0.15,diag=FALSE, mode = "undirected", return.as.edgelist=TRUE)
agraph1

agraph2 <- agraph1[, -3]

agraph3 <- graph_from_edgelist(agraph2, directed = FALSE)
agraph3

plot(agraph3)

V(agraph3)

E(agraph3)

agraph3 


