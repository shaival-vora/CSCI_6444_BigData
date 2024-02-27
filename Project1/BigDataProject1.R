# import Library
library(igraph)

# read the graph analytics project dataset
graphAnalyticsDataset <- read.delim("~/soc-Epinions1_adj(1).tsv", sep = "\t", header = FALSE)

#View the dataset
View(graphAnalyticsDataset)

# Make a matrix from the dataset
optab = as.matrix(graphAnalyticsDataset)

# Make Vectors V1, V2
v1 <- optab[1:811480, 1]
v2 <- optab[1:811480, 2]
v3 <- optab[1:811480, 3]

# Make DataFrames
relations = data.frame(from=v1, v2)
g <- graph_from_data_frame(relations, directed = TRUE)
plot(g)

# ---------------------------------------------------------------------------------------
# Cleaning up the blob graph using brute force
# ---------------------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------------------
# Cleaning up the dataset using fewer nodes
# ---------------------------------------------------------------------------------------

# Plot the graph with first 200 data points
df200 <- relations[1:200,]
df200
myGraph200 <- igraph:: graph_from_data_frame(df200)
plot(myGraph200)
plot.igraph(myGraph200)

# Plot the graph with first 500 data points
df500 <- relations[1:500,]
df500
myGraph500 <- igraph:: graph_from_data_frame(df500)
plot(myGraph500)

# Plot the graph with first 1000 data points
df1000 <- relations[1:1000,]
df1000
myGraph1000 <- igraph:: graph_from_data_frame(df1000)
plot(myGraph1000)

# Plot the graph with first 2500 data points
df2500 <- relations[1: 2500,]
df2500
myGraph2500 <- igraph:: graph_from_data_frame(df2500)
plot(myGraph2500)

# Plot the graph with first 5000 data points
df5000 <- relations[1: 5000,]
df5000
myGraph5000 <- igraph:: graph_from_data_frame(df5000)
plot(myGraph5000)

# ---------------------------------------------------------------------------------------
# Using Graph analytics functions from the graph analytics document 
# ---------------------------------------------------------------------------------------

# Str func 
# str() function is used to display the internal structure of R objects.
str(myGraph200)

# Install package SNA (Social Network Analysis)
#install.packages("sna") 
library(sna)

# Note that it represents a graph as an adjacency matrix. An adjacency matrix is an alternate way of representing a graph
# rgraph() is a function used to generate random graphs in R
# n: The number of nodes (vertices) in the graph
# m: The number of edges (ties) each node should have.
# tprob: The probability of forming an edge between any two nodes.
# diag: A logical value indicating whether to allow self-loops 
agraph <- rgraph(n=100, m=1, tprob=0.5, diag=FALSE)
agraph

# We can also return the results as an edgelist.
# Also, drop the tprob to 0.15 from 0.5 in agraph2,
# so we have less edges for illustrative purposes.
# “undirected” means no arrows on the edges
# edgeList:  It is essentially a list of pairs of nodes that are connected by edges.
agraph1 <- rgraph(n=100, m=1, tprob=0.15,diag=FALSE, mode = "undirected", return.as.edgelist=TRUE)
agraph1

# Note: it has three columns.
# So, let’s delete the third column.
# agraph[,-3] means keep all the rows, and all columns except the third.
agraph2 <- agraph1[, -3]
agraph2

# Let’s convert the edgelist to an igraph object.
# For this we will use graph_from_edgelist
# agraph3 will be a graph object created from the edge list agraph2, with edges indicating the connections between nodes.
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
# The default is F, but loops=T yields a smaller value because there are different paths with loops
igraph:: edge_density(agraph3)
igraph::edge_density(agraph3, loops = T)

# An egocentric network of a vertex v is a subgraph consisting of v and its immediate neighbors.
# Vertices with lots of neighbors can serve in many roles, such as brokers of information passing through the network. 
# agraph3.ego[1] to access the ego network centered around the first node
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
# edges that the path passes through (for un-weighted graphs) or the sum of the weights of the edges (for weighted graphs) is minimized.
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

# The shortest_paths() function from the igraph package calculates the shortest paths in a graph.
# If you want to find the shortest paths from node 5 to all other nodes in the graph agraph3
igraph:: shortest_paths(agraph3, from = 5)

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
# The degree of a node in a graph represents the number of connections that node has.
# Analyzing the degree distribution helps in understanding the overall structure of the network
hist(igraph:: degree(agraph3))

# By generating random graphs and converting them into adjacency matrices, you can gain insights into the structural properties of graphs.
# Making the dataset smaller to get some more visibility
bgraph = sna:: rgraph(25,1,0.2,"graph",FALSE)
bgraph.adj <- igraph::graph_from_adjacency_matrix(bgraph, mode = "undirected")
bgraph.adj
plot(bgraph.adj)

# Notice the difference between density with and without loop consideration as the link factor decreases.
# Edge density is defined as the ratio of the number of edges present in the graph to the total number of possible edges.
# It quantifies how densely the nodes in the graph are connected to each other.
# Calculating edge density with and without loops allows you to understand how self-edges affect the overall connectivity of the graph.
igraph::edge_density(bgraph.adj)
# This line calculates the edge density of the graph, considering loops (self-edges) as well.
# By setting loops = TRUE, the function includes self-edges when calculating edge density.
igraph::edge_density(bgraph.adj,loops = T)

# Find the diameter of bgraph
# The diameter of a graph is defined as the maximum shortest path length between any pair of vertices in the graph. 
bgraph.d = igraph::diameter(bgraph.adj)
bgraph.d

# Find the max-cliques for node 13(any random node):
# The max_cliques() function from the igraph package is used to find the maximal cliques in the graph.
# Maximal cliques are complete subgraphs (subsets of nodes) in which every pair of nodes is connected by an edge,
# and no additional node can be added to the subset while maintaining this property.
node <- c(13)
bgraph.13clique = igraph::max_cliques(bgraph.adj,min = NULL, max = NULL, subset = node)
bgraph.13clique

# Find the largest clique
# The largest clique in a graph represents the largest subset of nodes where every node is connected to every other node within the subset.
# Finding the size of the largest clique provides insights into the maximum level of connectivity within the graph
bgraph.largestClique = igraph::clique_num(bgraph.adj)
bgraph.largestClique

# Simplify function
sg <- simplify(g)
is_simple(sg)

# Detecting structures using walktrap.community()
wc <- cluster_walktrap(agraph3)
plot(wc,g, vertex.size = 5, vertex.label.cex = 0.2, edge.arrow.size = 0.1, layout = layout.fruchterman.reingold)

# Alpha Centrality
# Alpha centrality measures the extent to which a node's neighbors are influenced by the node itself.
acg <- alpha_centrality(agraph3)
sort(acg, decreasing = TRUE)



# ---------------------------------------------------------------------------------------
# Determine the (a) central nodes(s) in the graph, (b) longest path(s), (c) largest clique(s), (d) ego(s), and (e) power centrality. 
# ---------------------------------------------------------------------------------------

# (a) central nodes
# We can find the central node in two ways, either based on the sum of in and out degree or based on the node with the highest betweenness.
# Identifying the most central node in a graph can provide insights into its structure and functioning.
most_central <- which.max(igraph:: degree(g, mode = "all"))
most_central

between <- igraph:: betweenness(g)
most_central <- which.max(between)
most_central

# (b) longest path(s) 
sg = induced_subgraph(g, which(igraph::components(g) $membership == 1))
V(sg)$degree = igraph::degree(sg)
result = dfs(sg, root = 1, dist = TRUE)$dist
sort(result, decreasing = TRUE)

# (c) largest clique(s)
largest_cliques(agraph3)

# (d) ego(s)
ego.graph = igraph::ego(agraph3)
ego.graph

 # (e) power centrality. 
powerCentrality <- power_centrality(agraph3)
powerCentrality
