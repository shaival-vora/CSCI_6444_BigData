#install.packages("factoextra") 
library(igraph)
library(sna)
library(plotly)
library(factoextra)
library(psych)
library(ggplot2)
library(corrplot)
library(lattice)


clusteringDataset <- read.csv("/Users/shaivalvora/CSCI_6444_BigData/Project2/ObesityDataSet_raw_and_data_sinthetic.csv", stringsAsFactors = FALSE)
clusteringDataset[1:10,]

clusteringDataset.names = c("Gender",	"Age", "Height", "Weight","family_history_with_overweight","FAVC","FCVC","NCP","CAEC","SMOKE","CH2O","SCC","FAF","TUE","CALC","MTRANS","NObeyesdad")
names(clusteringDataset) <- clusteringDataset.names

str(clusteringDataset)

clusteringDataset[1:10,1:7]
clusteringDataset.df = clusteringDataset[,1:17]
clusteringDataset.df[1:10,1:17]

# Identifying non numeric colums 
non_numeric_columns = sapply(clusteringDataset, Negate(is.numeric))
non_numeric_columns

# Convert all the non numeric data into a numeric data 
clusteringDataset[non_numeric_columns] = lapply(clusteringDataset[non_numeric_columns], function(x) as.numeric(as.factor(x)))
clusteringDataset

# Get the summery and description of the data set and check the structure of the data
str(clusteringDataset)
summary(clusteringDataset)
describe(clusteringDataset)
head(clusteringDataset)

# Do a pair wise plotting to get the relation between all the types of data
plot(clusteringDataset)
pairs(clusteringDataset)

# 3d Plotting of the data using three parameters Height, Weight and NObeyesdad to get more information
plot_ly(clusteringDataset,
        x = ~Height,
        y = ~Weight,
        z = ~NObeyesdad, 
        type = "scatter3d",
        mode = "markers",
        colors = c('#BF382A', '#0C4B8E', '#000000')) %>%
  layout(scene = list(xaxis = list(title = 'X-axis'),
                      yaxis = list(title = 'Y-axis'),
                      zaxis = list(title = 'Z-axis')))

# Normalization function
normalize_function = function(x) {((x-min(x))/(max(x)-min(x)))}
normalize_function

# Scaling with normalize: The lapply() method applies to the entire data frame and all selected columns.
clusteringDataset.norms = as.data.frame(lapply(clusteringDataset, normalize_function))
clusteringDataset.norms

# A graphical display of a correlation matrix, confidence interval
corelation = cor(clusteringDataset.norms)
corelation
plot(corelation)
cor.plot(corelation)

# ########################################
# Creating clusters for K-Means clustering 
# #######################################

obesity.k5 = kmeans(clusteringDataset.norms, centers = 5)
str(obesity.k5)
obesity.k5
factoextra::fviz_cluster(obesity.k5, clusteringDataset.norms)


obesity.k7 = kmeans(clusteringDataset.norms, centers = 7)
str(obesity.k7)
obesity.k7
factoextra::fviz_cluster(obesity.k7, clusteringDataset.norms)

obesity.k9 = kmeans(clusteringDataset.norms, centers = 9)
str(obesity.k9)
obesity.k9
factoextra::fviz_cluster(obesity.k9, clusteringDataset.norms)

obesity.k11 = kmeans(clusteringDataset.norms, centers = 11)
str(obesity.k11)
obesity.k11
factoextra::fviz_cluster(obesity.k11, clusteringDataset.norms)

obesity.k13 = kmeans(clusteringDataset.norms, centers = 13)
str(obesity.k13)
obesity.k13
factoextra::fviz_cluster(obesity.k13, clusteringDataset.norms)

#Finding the optimal k values
factoextra::fviz_nbclust(clusteringDataset, FUNcluster = kmeans, method = "wss", k.max = 20, verbose = TRUE)

obesity.k4 = kmeans(clusteringDataset.norms, centers = 6)
str(obesity.k4)
obesity.k4
factoextra::fviz_cluster(obesity.k4, clusteringDataset.norms)


#Creating the train test split
clusteringDataset.norms.rows = nrow()
