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

# Install package SNA (Social Network Analysis)
#install.packages("sna") 
library(sna)

# Note that it represents a graph as an adjacency matrix. An adjacency matrix is an alternate way of representing a graph
agraph <- rgraph(n=100, m=1, tprob=0.5, diag=FALSE)
agraph

# We can also return the results as an edgelist.
# Also, drop the tprob to 0.15 from 0.5 in agraph2,
# so we have less edges for illustrative purposes.
# “undirected” means no arrows on the edges
agraph1 <- rgraph(n=100, m=1, tprob=0.15,diag=FALSE, mode = "undirected", return.as.edgelist=TRUE)
agraph1

# Note: it has three columns.
# So, let’s delete the third column.
# agraph[,-3] means keep all the rows, and all columns except the third.
agraph2 <- agraph1[, -3]

# Let’s convert the edgelist to an igraph object.
# For this we will use graph_from_edgelist
agraph3 <- graph_from_edgelist(agraph2, directed = FALSE)
agraph3

# Plot the new agraph3 object
plot(agraph3)

#To get the vertices of a graph, we use the V function:
V(agraph3)

#To get the edges of a graph, we use the E function:
E(agraph3)

# Get the adjacency matrix: For this we will use the as_adjacency_matrix method
agraph3.adj = igraph:: as_adjacency_matrix(agraph3)
agraph3.adj

# Density = # existing edges/ # possible edges.
#The # possible edges is the number of edges computed by assuming every node is connected to every other node in the graph.
#gden takes an adjacency matrix as its argument. 
agraph.density = gden(agraph)
agraph.density

# Loops specifies whether to allow loops in the graph.
# The default is F, but loops=T yields a smalle value because there are different paths with loops
igraph:: edge_density(agraph3)
igraph::edge_density(agraph3, loops = T)

# An egocentric network of a vertex v is a subgraph consisting of v and its immediate neighbors.
# Vertices with lots of neighbors can serve in many roles, such as brokers of information passing through the network. 
agraph3.ego = ego.extract(agraph)
agraph3.ego[1]

# We can find the degree of each node in the graph:
# the function ‘degree’ might be defined in several packages.
# So, sometimes you need to preface the function name with the package name.
# Here “igraph” is the package name, so the function call is “igraph::degree”
igraph:: degree(agraph3)


# Some centrality metrics:
# betweenness centrality: is a measure of centrality in a graph based on shortest paths.
# For every pair of vertices in a connected graph, there exists at least one shortest path between the vertices such that either the number of 
# edges that the path passes through (for unweighted graphs) or the sum of the weights of the edges (for weighted graphs) is minimized.
#The betweenness centrality for each vertex is the number of these shortest paths that pass through the vertex.
agraph3.between = igraph::centr_betw(agraph3)
agraph3.between

# Closeness Centrality (CLC) is a measure defined for a given vertex
# In a connected graph, closeness centrality (or closeness) of a node is a measure of centrality in a network,
# calculated as the reciprocal of the sum of the length of the shortest paths between the node and all other nodes in the graph.
# Thus, the more central a node is, the closer it is to all other nodes.

# The higher the closeness, the closer the node is to other nodes in the graph.
agraph3.closeness = igraph::centr_clo(agraph3)
agraph3.closeness

# Finding the shortest path between two nodes:
# Shortest.paths provides the length of the shortest path between any two nodes in a graph g

agraph3.sp = igraph::distances(agraph3)
agraph3.sp

# A geodesic is the shortest path between any two nodes in the network.
# A node has high betweenness if the geodesics between many pairs of other nodes pass through that node.
# A node with high betweenness, when it fails or is removed, has greater influence on the connectivity of the network.

agraph.geos = geodist(agraph)
agraph.geos

# Suppose we want to find the number of paths between two nodes.
# We can multiply the adjacency matrix by itself. The cell numbers specify the number of paths.

agraph3.np = agraph3.adj%*%agraph3.adj
agraph3.np

# You can get a histogram of the degree of the nodes in agraph3: 
# Use hist(igraph::degree(agraph3)) 

hist(igraph:: degree(agraph3))

bgraph = sna:: rgraph(25,1,0.2,"graph",FALSE)
bgraph.adj <- igraph::graph_from_adjacency_matrix(bgraph, mode = "undirected")
bgraph.adj

plot(bgraph.adj)

# Notice the difference between density with and without loop consideration as the link factor decreases.

igraph::edge_density(bgraph.adj)
igraph::edge_density(bgraph.adj,loops = T)

# Find the diameter of bgraph
bgraph.d = igraph::diameter(bgraph.adj)
bgraph.d

# Find the max-cliques for node 13:
node <- c(13)
bgraph.13clique = igraph::max_cliques(bgraph.adj,min = NULL, max = NULL, subset = node)
bgraph.13clique

# Find the largest clique
bgraph.largestClique = igraph::clique_num(bgraph.adj)
bgraph.largestClique





