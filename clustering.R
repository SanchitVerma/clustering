# CLUSTERING

# Libraries used
library(factoextra) # clustering algorithms & visualization
library(cluster) # For clustering
library(NbClust) # for finding optimum number of clusters
library(GGally) # For pairs ploting
library(dendextend)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(Hmisc)

# Importing data, import as  text instead
raw_data <- read.csv("~/Documents/UNO/Data Mining/Project/NE_Full_original.csv")
describe(raw_data)

# Removing unecessary fields from our data
raw_data$X <- NULL
raw_data$Start_Lat <- NULL
raw_data$Start_Lng <- NULL
raw_data$County <- NULL
raw_data$POPESTIMATE2016 <- NULL
raw_data$POPESTIMATE2017 <- NULL
raw_data$POPESTIMATE2018 <- NULL

# Checking and removing NAs from all the field we need
# Check
colSums(is.na(raw_data))
# Remove
raw_data <- na.omit(raw_data)

# Converting fields with True/False to 1/0
raw_data$Amenity [raw_data$Amenity == "true"] <- 1
raw_data$Amenity [raw_data$Amenity == "false"] <- 0

raw_data$Bump [raw_data$Bump == "true"] <- 1
raw_data$Bump [raw_data$Bump == "false"] <- 0

raw_data$Crossing [raw_data$Crossing == "true"] <- 1
raw_data$Crossing [raw_data$Crossing == "false"] <- 0

raw_data$Crossing [raw_data$Crossing == "true"] <- 1
raw_data$Crossing [raw_data$Crossing == "false"] <- 0

raw_data$Give_Way [raw_data$Give_Way == "true"] <- 1
raw_data$Give_Way [raw_data$Give_Way == "false"] <- 0

raw_data$Junction [raw_data$Junction == "true"] <- 1
raw_data$Junction [raw_data$Junction == "false"] <- 0

raw_data$No_Exit [raw_data$No_Exit == "true"] <- 1
raw_data$No_Exit [raw_data$No_Exit == "false"] <- 0

raw_data$Railway [raw_data$Railway == "true"] <- 1
raw_data$Railway [raw_data$Railway == "false"] <- 0

raw_data$Station [raw_data$Station == "true"] <- 1
raw_data$Station [raw_data$Station == "false"] <- 0

raw_data$Stop [raw_data$Stop == "true"] <- 1
raw_data$Stop [raw_data$Stop == "false"] <- 0

raw_data$Traffic_Calming [raw_data$Traffic_Calming == "true"] <- 1
raw_data$Traffic_Calming [raw_data$Traffic_Calming == "false"] <- 0

raw_data$Traffic_Signal [raw_data$Traffic_Signal == "true"] <- 1
raw_data$Traffic_Signal [raw_data$Traffic_Signal == "false"] <- 0

# making factors
# checking structure of data before making factors
str(raw_data)
# Changing fields to factors
raw_data$Amenity <- as.factor(raw_data$Amenity) 
raw_data$Bump <- as.factor(raw_data$Bump) 
raw_data$Crossing <- as.factor(raw_data$Crossing) 
raw_data$Give_Way <- as.factor(raw_data$Give_Way) 
raw_data$Junction <- as.factor(raw_data$Junction) 
raw_data$No_Exit <- as.factor(raw_data$No_Exit) 
raw_data$Railway <- as.factor(raw_data$Railway) 
raw_data$Station <- as.factor(raw_data$Station) 
raw_data$Stop <- as.factor(raw_data$Stop) 
raw_data$Traffic_Calming <- as.factor(raw_data$Traffic_Calming) 
raw_data$Traffic_Signal <- as.factor(raw_data$Traffic_Signal) 

# Splitting data for training and test sets
# Using 20% of data for training
split <- sample(2, nrow(raw_data), replace=TRUE, prob =c(0.01,0.99))
training_data  <- raw_data[split==1,]
test_data <- raw_data[split==2,]

# Computing pairwise dissimilarities 
distance <- daisy(training_data, metric = "gower")
print(distance)

# Hierarchical clustering
cluster_model <- hclust(distance, method = "complete", members = NULL)
print(cluster_model)

# plotting dendogram
plot(cluster_model)

# Alternate method for plotting dendogram
dendogram <- as.dendrogram(cluster_model)
plot(dendogram)

# finding the optimal number of clusters - Elbow & Silhouette Methods
fviz_nbclust(training_data, FUN = hcut, method = "wss", print.summary = TRUE) + geom_vline(xintercept = 6, linetype = 2)+labs(subtitle = "Elbow method")
fviz_nbclust(training_data, FUN = hcut, method = "silhouette", print.summary = TRUE) + geom_vline(xintercept = 6, linetype = 2)+labs(subtitle = "Silhouette method")

# Generating clusters & their vizualizations
# k=2
cluster_2 <- cutree(cluster_model, k = 2)
print(cluster_2)
training_data <- cbind(training_data, cluster_2 = cluster_2)
fviz_cluster(list(data = distance, cluster = cluster_2))

# k=3
cluster_3 <- cutree(cluster_model, k = 3)
print(cluster_3)
training_data <- cbind(training_data, cluster_3 = cluster_3)
fviz_cluster(list(data = distance, cluster = cluster_3))

# k=4
cluster_4 <- cutree(cluster_model, k = 4)
print(cluster_4)
training_data <- cbind(training_data, cluster_4 = cluster_4)
fviz_cluster(list(data = distance, cluster = cluster_4))

# k=5
cluster_5 <- cutree(cluster_model, k = 5)
print(cluster_5)
training_data <- cbind(training_data, cluster_5 = cluster_5)
fviz_cluster(list(data = distance, cluster = cluster_5))

# k=6
cluster_6 <- cutree(cluster_model, k = 6)
print(cluster_6)
training_data <- cbind(training_data, cluster_6 = cluster_6)
fviz_cluster(list(data = distance, cluster = cluster_6))

# k=7
cluster_7 <- cutree(cluster_model, k = 7)
print(cluster_7)
training_data <- cbind(training_data, cluster_7 = cluster_7)
fviz_cluster(list(data = distance, cluster = cluster_7))

# Extracting training data with cluster number as a columns
write.csv(training_data,"~/Documents/UNO/Data Mining/Project/NE_Full_clusters.csv", row.names = FALSE)

# (2 Clusters) - Correlation of raw data with groups and color based on hierarchical clustering
g_1 <- ggpairs(data=training_data, columns=1:3, cardinality_threshold = 2087,
             mapping=ggplot2::aes(colour = as.character(cluster_2)),
             title="Pairwise plot") + theme(strip.placement = "outside", text = element_text(size = 7, lineheight = 3))
print(g_1)

g_2 <- ggpairs(data=training_data, columns=4:8, cardinality_threshold = 2087,
             mapping=ggplot2::aes(colour = as.character(cluster_2)),
             title="Pairwise plot") + theme(strip.placement = "outside", text = element_text(size = 7, lineheight = 3))
print(g_2)

g_3 <- ggpairs(data=training_data, columns=9:19, cardinality_threshold = 2087,
             mapping=ggplot2::aes(colour = as.character(cluster_2)),
             title="Pairwise plot") + theme(strip.placement = "outside", text = element_text(size = 7, lineheight = 3))
print(g_3)

g_4 <- ggpairs(data=training_data, columns=20:29, cardinality_threshold = 2087,
               mapping=ggplot2::aes(colour = as.character(cluster_2)),
               title="Pairwise plot") + theme(strip.placement = "outside", text = element_text(size = 7, lineheight = 3))
print(g_4)
