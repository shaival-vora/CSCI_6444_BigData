install.packages("glmnet") 
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr) 
library(caret) 
library(cluster) 
library(corrplot)
library(glmnet)
library(rstatix)
library(gmodels)
library(psych)
library(nnet)
library(plotly)

############################################################################################################################
# Task 1 
# Loading Data set  and exploration
# For this data set, plot the data using pairwise plotting to get a sense of the relationships between the attributes. 
# 
############################################################################################################################

# Read the Data set 
clusteringDataset <- read.csv("/Users/shaivalvora/CSCI_6444_BigData/Project2/ObesityDataSet_raw_and_data_sinthetic.csv")
clusteringDataset[1:10,]

clusteringDataset.names = c("Gender",	"Age", "Height", "Weight","family_history_with_overweight","FAVC","FCVC","NCP","CAEC","SMOKE","CH2O","SCC","FAF","TUE","CALC","MTRANS","NObeyesdad")
names(clusteringDataset) <- clusteringDataset.names

# Get the structure of the date set 
str(clusteringDataset)

# Identifying all the  non numeric columns 
non_numeric_columns = sapply(clusteringDataset, Negate(is.numeric))
non_numeric_columns

# Convert all the non numeric data into a numeric data 
clusteringDataset[non_numeric_columns] = lapply(clusteringDataset[non_numeric_columns], function(x) as.numeric(as.factor(x)))
clusteringDataset

# Do a pair wise plotting to get the relation between all the types of data
plot(clusteringDataset)
pairs(clusteringDataset)

# 3d Plotting of the data using three parameters Height, Weight and NObeyesdad to get more information

plot3D = plot_ly(clusteringDataset,
                 x = ~Weight,
                 y = ~Age,
                 z = ~Height,
                 type = "scatter3d",
                 mode = "markers")
plot3D = plot3D %>% add_markers()
plot3D = plot3D %>%  layout(scene = list(xaxis = list(title = 'Weight'),
                                         yaxis = list(title = 'Age'),
                                         zaxis = list(title = 'Height')))
plot3D

############################################################################################################################
# Task 2 
# Prepare the Data 
#
############################################################################################################################

# Get the summery and description of the data set and check the structure of the data
str(clusteringDataset)
summary(clusteringDataset)
describe(clusteringDataset)
head(clusteringDataset)

# Normalize features, we will keep the response variable "NObeyesdad" intact
features <- setdiff(names(clusteringDataset), "NObeyesdad")
clusteringDataset.features <- clusteringDataset[features]

# Scaling with normalize: The lapply() method applies to the entire data frame and all selected columns
# Normalization function.
normalize_function <- function(x) {((x-min(x))/(max(x)-min(x)))}
clusteringDataset.features.norm <- as.data.frame(lapply(clusteringDataset.features, normalize_function))
clusteringDataset.norm <- cbind(clusteringDataset.features.norm, NObeyesdad = clusteringDataset$NObeyesdad)

clusteringDataset.norm[1:10,]

# A graphical display of a correlation matrix, confidence interval
correlation = cor(clusteringDataset.features.norm)
corelation

# Plot the correlation
cor.plot(correlation)

# Split the data set into Training and Test Sets (70-30%, 60-40%, 50-50%)
set.seed(110)  # Ensures reproducibility

# For 50-50 Split in the data set
splitRatio50_50 <- 0.5

clusteringDataset.norm.rows.50_50 <- nrow(clusteringDataset.norm)
clusteringDataset.rows.50_50 <- round(splitRatio50_50 * clusteringDataset.norm.rows.50_50)

clusteringDataset.train.index.50_50 <- sample(clusteringDataset.norm.rows.50_50, clusteringDataset.rows.50_50)

clusteringDataset.tain.50_50 <- clusteringDataset.norm[clusteringDataset.train.index.50_50, ]
clusteringDataset.test.50_50 <- clusteringDataset.norm[-clusteringDataset.train.index.50_50, ]

cat("For the 50-50 Split Ratio: Training data set will have", nrow(clusteringDataset.tain.50_50), "rows. Testing data will have", nrow(clusteringDataset.test.50_50), "rows.\n")

# For 60-40 Split in the data set
splitRatio60_40 <- 0.6

clusteringDataset.norm.rows.60_40 <- nrow(clusteringDataset.norm)
clusteringDataset.rows.60_40 <- round(splitRatio60_40 * clusteringDataset.norm.rows.60_40)

clusteringDataset.train.index.60_40 <- sample(clusteringDataset.norm.rows.60_40, clusteringDataset.rows.60_40)

clusteringDataset.tain.60_40 <- clusteringDataset.norm[clusteringDataset.train.index.60_40, ]
clusteringDataset.test.60_40 <- clusteringDataset.norm[-clusteringDataset.train.index.60_40, ]

cat("For the 60-40 Split Ratio: Training data set will have", nrow(clusteringDataset.tain.60_40), "rows. Testing data will have", nrow(clusteringDataset.test.60_40), "rows.\n")

# For 70-30 Split in the data set
splitRatio70_30 <- 0.7

clusteringDataset.norm.rows.70_30 <- nrow(clusteringDataset.norm)
clusteringDataset.rows.70_30 <- round(splitRatio70_30 * clusteringDataset.norm.rows.70_30)

clusteringDataset.train.index.70_30 <- sample(clusteringDataset.norm.rows.70_30, clusteringDataset.rows.70_30)

clusteringDataset.tain.70_30 <- clusteringDataset.norm[clusteringDataset.train.index.70_30, ]
clusteringDataset.test.70_30 <- clusteringDataset.norm[-clusteringDataset.train.index.70_30, ]

cat("For the 70_30 Split Ratio: Training data set will have", nrow(clusteringDataset.tain.70_30), "rows. Testing data will have", nrow(clusteringDataset.test.70_30), "rows.\n")


############################################################################################################################
# Task 3 
# Creating clusters for K-Means clustering 
#
############################################################################################################################

#Create 3 clusters
obesity.k3 = kmeans(clusteringDataset.norm, centers = 3)
str(obesity.k3)
obesity.k3
factoextra::fviz_cluster(obesity.k3, clusteringDataset.norm)

#Create 5 clusters
obesity.k5 = kmeans(clusteringDataset.norm, centers = 5)
str(obesity.k5)
obesity.k5
factoextra::fviz_cluster(obesity.k5, clusteringDataset.norm)

#Create 7 clusters
obesity.k7 = kmeans(clusteringDataset.norm, centers = 7)
str(obesity.k7)
obesity.k7
factoextra::fviz_cluster(obesity.k7, clusteringDataset.norm)

#Create 9 clusters
obesity.k9 = kmeans(clusteringDataset.norm, centers = 9)
str(obesity.k9)
obesity.k9
factoextra::fviz_cluster(obesity.k9, clusteringDataset.norm)

#Create 11 clusters
obesity.k11 = kmeans(clusteringDataset.norm, centers = 11)
str(obesity.k11)
obesity.k11
factoextra::fviz_cluster(obesity.k11, clusteringDataset.norm)

#Create 13 clusters
obesity.k13 = kmeans(clusteringDataset.norm, centers = 13)
str(obesity.k13)
obesity.k13
factoextra::fviz_cluster(obesity.k13, clusteringDataset.norm)

#Finding the optimal k values
factoextra::fviz_nbclust(clusteringDataset,
                         FUNcluster = kmeans,
                         method = "wss",
                         k.max = 20,
                         verbose = TRUE)

#Create 4 clusters
obesity.k4 = kmeans(clusteringDataset.norms, centers = 6)
str(obesity.k4)
obesity.k4
factoextra::fviz_cluster(obesity.k4, clusteringDataset.norms)


############################################################################################################################
# Task 4
# Prediction and linear modeling
#
############################################################################################################################


# GLM for 50-50% Split of the training and testing data
clusteringDataset.train.glm.50_50 =  glm(NObeyesdad ~ .,
                                         data = clusteringDataset.tain.50_50,
                                         family = "gaussian")

clusteringDataset.test.pred.50_50 <- predict(clusteringDataset.train.glm.50_50,
                                              newdata = clusteringDataset.test.50_50,
                                              type = "response")

clusteringDataset.test.pred.50_50

summary(clusteringDataset.test.pred.50_50)

# Adjusting predictions and generating true labels for 50-50 split
clusteringDataset.test.pred.class.50_50 <- ifelse(clusteringDataset.test.pred.50_50 > 0.5, 1, 0)
clusteringDataset.labels.50_50 <- clusteringDataset.test.50_50$NObeyesdad

# Accuracy Calculation for 50-50 split
clusteringDataset.accuracy50_50 <- mean(clusteringDataset.labels.50_50 == clusteringDataset.test.pred.class.50_50)
cat("Accuracy for GLM predictions for a 50-50 split in the dataset is =", clusteringDataset.accuracy50_50, "\n")

# K-Means Clustering for the Test Set (50-50% Split)
kMeans.clusters.50_50 <- 7  
predicted.clusters.50_50 <- kmeans(clusteringDataset.test.50_50[, -ncol(clusteringDataset.test.50_50)],
                                   centers = kMeans.clusters.50_50)$cluster

predicted.clusters.50_50

# Cross-Tabulation to Compare Clusters and True Labels for 50-50 split
clusteringDataset.test.crossTable.k5.50_50 <- CrossTable(x = as.factor(clusteringDataset.labels.50_50),
                                                         y = as.factor(predicted.clusters.50_50),
                                                         prop.chisq = FALSE)

# Anova Test for GLM Model for 50-50 split of training and testing data set
clusteringDataset.train.glm.anova.50_50 <- anova(clusteringDataset.train.glm.50_50,
                                                 test = "Chisq")

clusteringDataset.train.glm.anova.50_50

plot(clusteringDataset.train.glm.50_50)

# Summarizing GLM Model for 50-50 split
summary(clusteringDataset.train.glm.50_50)

confint(clusteringDataset.train.glm.50_50)

#############################################################################################

# GLM for 60-40% Split of the training and testing data
clusteringDataset.train.glm.60_40 =  glm(NObeyesdad ~ .,
                                         data = clusteringDataset.tain.60_40,
                                         family = "gaussian")

clusteringDataset.test.pred.60_40 <- predict(clusteringDataset.train.glm.60_40,
                                             newdata = clusteringDataset.test.60_40,
                                             type = "response")

clusteringDataset.test.pred.60_40

summary(clusteringDataset.test.pred.60_40)

# Adjusting predictions and generating true labels for 60_40 split
clusteringDataset.test.pred.class.60_40 <- ifelse(clusteringDataset.test.pred.60_40 > 0.5, 1, 0)
clusteringDataset.labels.60_40 <- clusteringDataset.test.60_40$NObeyesdad

# Accuracy Calculation for 60_40 split
clusteringDataset.accuracy.60_40 <- mean(clusteringDataset.labels.60_40 == clusteringDataset.test.pred.class.60_40)
cat("Accuracy for GLM predictions for a 60_40 split in the dataset is =", clusteringDataset.accuracy.60_40, "\n")

# K-Means Clustering for the Test Set (60_40% Split)
kMeans.clusters.60_40 <- 7  
predicted.clusters.60_40 <- kmeans(clusteringDataset.test.60_40[, -ncol(clusteringDataset.test.60_40)],
                                   centers = kMeans.clusters.60_40)$cluster

predicted.clusters.60_40

# Cross-Tabulation to Compare Clusters and True Labels for 60_40 split
clusteringDataset.test.crossTable.k5.60_40 <- CrossTable(x = as.factor(clusteringDataset.labels.60_40),
                                                         y = as.factor(predicted.clusters.60_40),
                                                         prop.chisq = FALSE)

# Anova Test for GLM Model for 60_40 split of training and testing data set
clusteringDataset.train.glm.anova.60_40 <- anova(clusteringDataset.train.glm.60_40,
                                                 test = "Chisq")
clusteringDataset.train.glm.anova.60_40

plot(clusteringDataset.train.glm.60_40)

# Summarizing GLM Model for 60_40 split
summary(clusteringDataset.train.glm.60_40)

confint(clusteringDataset.train.glm.60_40)

#############################################################################################

# GLM for 70-30% Split of the training and testing data
clusteringDataset.train.glm.70_30 =  glm(NObeyesdad ~ .,
                                         data = clusteringDataset.tain.70_30,
                                         family = "gaussian")

clusteringDataset.test.pred.70_30 <- predict(clusteringDataset.train.glm.70_30,
                                             newdata = clusteringDataset.test.70_30,
                                             type = "response")

clusteringDataset.test.pred.70_30

summary(clusteringDataset.test.pred.70_30)

# Adjusting predictions and generating true labels for 70-30 split
clusteringDataset.test.pred.class.70_30 <- ifelse(clusteringDataset.test.pred.70_30 > 0.5, 1, 0)
clusteringDataset.labels.70_30 <- clusteringDataset.test.70_30$NObeyesdad

# Accuracy Calculation for 70-30 split
clusteringDataset.accuracy.70_30 <- mean(clusteringDataset.labels.70_30 == clusteringDataset.test.pred.class.70_30)
cat("Accuracy for GLM predictions for a 70-30 split in the dataset is =", clusteringDataset.accuracy.70_30, "\n")

# K-Means Clustering for the Test Set (70-30% Split)
kMeans.clusters.70_30 <- 7  
predicted.clusters.70_30 <- kmeans(clusteringDataset.test.70_30[, -ncol(clusteringDataset.test.70_30)],
                                   centers = kMeans.clusters.70_30)$cluster

predicted.clusters.70_30

# Cross-Tabulation to Compare Clusters and True Labels for 70-30 split
clusteringDataset.test.crossTable.k5.70_30 <- CrossTable(x = as.factor(clusteringDataset.labels.70_30),
                                                         y = as.factor(predicted.clusters.70_30),
                                                         prop.chisq = FALSE)

# Anova Test for GLM Model for 70-30 split of training and testing data set
clusteringDataset.train.glm.anova.70_30 <- anova(clusteringDataset.train.glm.70_30,
                                                 test = "Chisq")

clusteringDataset.train.glm.anova.70_30


plot(clusteringDataset.train.glm.70_30)

# Summarizing GLM Model for 70-30 split
summary(clusteringDataset.train.glm.70_30)

confint(clusteringDataset.train.glm.70_30)


# Plot 
plot(clusteringDataset.test.50_50$NObeyesdad, clusteringDataset.test.pred.50_50)

plot(clusteringDataset.test.60_40$NObeyesdad, clusteringDataset.test.pred.60_40)

plot(clusteringDataset.test.70_30$NObeyesdad, clusteringDataset.test.pred.70_30)


# Performance Measure
confusion_Matrix = clusteringDataset.test.crossTable.k5.70_30$t
confusion_Matrix

accuracy = sum(diag(confusion_Matrix)) / sum(confusion_Matrix)
accuracy
precision = diag(confusion_Matrix) / colSums(confusion_Matrix)
precision

recall = diag(confusion_Matrix) / rowSums(confusion_Matrix)
recall

specificity = sapply(1:nrow(confusion_Matrix), function(i) {
  TN = sum(confusion_Matrix[-i,-i])
  FP = sum(confusion_Matrix[-i, i])
  TN / (TN + FP)
})

specificity

error = 1 - accuracy
error

sum(confusion_Matrix)


