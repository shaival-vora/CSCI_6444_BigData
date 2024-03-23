library(igraph)
library(sna)
library(plotly)
library(factoextra)
library(psych)
library(ggplot2)
library(corrplot)
library(lattice)

#install.packages("factoextra") 
#install.packages("psych")
#install.packages("")
clusteringDataset <- read.table("/Users/shaivalvora/CSCI_6444_BigData/Project2/ObesityDataSet_raw_and_data_sinthetic.csv", stringsAsFactors = FALSE)
clusteringDataset[1:10,]

clusteringDataset.names = c("Gender",	"Age", "Height", "Weight","family_history_with_overweight","FAVC","FCVC","NCP","CAEC","SMOKE","CH2O","SCC","FAF","TUE","CALC","MTRANS","NObeyesdad")

str(clusteringDataset)

names(clusteringDataset) <- clusteringDataset.names


summary(clusteringDataset)

describe(clusteringDataset)

plot(clusteringDataset)


