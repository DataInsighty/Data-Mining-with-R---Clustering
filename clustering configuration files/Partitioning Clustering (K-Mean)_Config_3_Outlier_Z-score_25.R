########################################################################
#-------------- 1. Loading Libraries and data -------------------------#
########################################################################



# Load libraries
library(readxl)  # Loads the readxl library for reading Excel files
library(cluster) # Loads the cluster library for clustering functions
library(NbClust)  # Loads the NbClust library for determining optimal number of clusters
library(corrplot)  # Loads the corrplot library for plotting correlation matrices
library(tidyr)  # Loads the tidyr library for data manipulation
library(fpc)  # Loads the fpc library for clustering and related tasks
library(MASS)  # Loads the MASS library for various statistical functions and datasets
library(ggplot2)  # Loads the ggplot2 library for creating plots
library(flexclust)  # Loads the flexclust library for flexible clustering algorithms
library(factoextra)


# Read data
df <- read_excel("DM_R_Answers//whitewine.xlsx")

# -------------------------------------------------------------------------


# Define z-score normalization function
z_score_normalization <- function(x) {
  return((x - mean(x)) / sd(x))
}

# Columns of interest
cols_of_interest <- c(
  "fixed acidity", "volatile acidity", "citric acid",
  "residual sugar", "chlorides", "free sulfur dioxide",
  "total sulfur dioxide", "density", "pH",
  "sulphates", "alcohol"
)

# Apply z-score normalization to the dataset
df_norm <- df %>%
  dplyr::mutate(
    across(
      all_of(cols_of_interest),
      .fns = z_score_normalization
    )
  )

# Summary statistics of the normalized dataset
summary(df_norm)

#---------------------------------------------------------------
# Splitting features and target from x
x <- df_norm[, 1:11]  # Extracting the first 11 columns as features
y <- df_norm$quality  # Extracting the 12th column as the target variable

# --------------------------------------------------------


#########################################################################
##-----------------Find the ideal number of clusters--------------------#
#########################################################################

##---------------------NbClust method

# NbClust is an R Package for determining the relevant number of clusters in a dataset.
# It provides 30 indices which determine the number of clusters in a data set and it offers also the best clustering scheme from different results to the user.

set.seed(26)

cluster=NbClust(x,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")

barplot(table(cluster$Best.n[1,]), # provide bar charts####
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 30 Criteria")


##---------------------------Elbow method

# We can also plot the Within Cluster Sum of Squares and the number of clusters 
# to find the location of a bend or a knee in the plot which is considered as an indicator of the appropriate number of clusters.

k_values <- 1:10 # specifying range of k values to test
wss <- vector("numeric", length = length(k_values)) # initializing vector to store within-cluster sum of squares (WSS)

for (i in 1:length(k_values)) {
  k <- k_values[i]
  set.seed(123) # for reproducibility
  km <- kmeans(x, centers = k)
  wss[i] <- km$tot.withinss # store WSS for current k value
}

ggplot() +
  geom_line(aes(x = k_values, y = wss), color = "blue") +
  geom_point(aes(x = k_values, y = wss), color = "blue", size = 3) +
  labs(title = "Elbow Method for Determining Optimal Number of Clusters",
       x = "Number of Clusters (k)",
       y = "Within-Cluster Sum of Squares (WSS)") 

#-----------------------Silhouette Method

# Compute silhouette scores for different values of k
silhouette_scores <- sapply(2:10, function(k) {
  km <- kmeans(x, centers = k)
  silhouette_avg <- silhouette(km$cluster, dist(x))
  mean(silhouette_avg[, 3])
})

# Plot silhouette scores against the number of clusters
plot(2:10, silhouette_scores, type = "b", xlab = "Number of Clusters", ylab = "Silhouette Score", main = "Silhouette Score for Different Numbers of Clusters")


# Find the optimal number of clusters based on silhouette scores
optimal_k_silhouette <- which.max(silhouette_scores) + 1
print(optimal_k_silhouette)


#######################################################################
#-------------------perform the k-means ------------------------------#
#######################################################################

# Perform k-means clustering with K=2
set.seed(26)
km_2 <- kmeans(x, centers = 2, nstart = 25)

print(km_2)


# Plot 1: alcohol vs pH
plot(x[, c("alcohol", "pH")], col = km_2$cluster)
points(km_2$centers[, c("alcohol", "pH")], col = 1:3, pch = 23, cex = 3)

# Perform k-means clustering with K=4
set.seed(26)

km_4 <- kmeans(x, centers = 4, nstart = 25)
print(km_4)

# Plot 1: alcohol vs pH
plot(x[, c("alcohol", "pH")], col = km_4$cluster)
points(km_4$centers[, c("alcohol", "pH")], col = 1:3, pch = 23, cex = 3)


# Find mean of each cluster for km_2
aggregate(x, by=list(cluster=km_2$cluster), mean)

# Add cluster assignment to original data for km_2
final_data_km2 <- cbind(df, cluster = km_2$cluster)

# View final data for km_2
head(final_data_km2)



# Find mean of each cluster for km_4
aggregate(x, by=list(cluster=km_4$cluster), mean)

# Add cluster assignment to original data for km_4
final_data_km4 <- cbind(df, cluster = km_4$cluster)

# View final data for km_4
head(final_data_km4)


# use plotcluster function from fpc package to draw discriminant projection plot
plotcluster(x, km_2$cluster, main = "K=2 Clusters")


plotcluster(x, km_4$cluster, main = "K=4 Clusters")

# Next, we draw parallel coordinates plot to see how variables contributed in each cluster
parcoord(x, km_2$cluster, col = km_2$cluster, main = "Parallel Coordinates Plot for K=2 Clusters")



parcoord(x, km_4$cluster, col = km_4$cluster, main = "Parallel Coordinates Plot for K=4 Clusters")

# Visualize clusters for km_2
fviz_cluster(km_2, data = x)

# Visualize clusters for km_4
fviz_cluster(km_4, data = x)



############################################################################
#--------------------------------Validation--------------------------------#
############################################################################

# Compare cluster centers
print("Cluster centers for K=2:")
print(km_2$centers)
print("Cluster centers for K=4:")
print(km_4$centers)

# Compare cluster sizes
print("Cluster sizes for K=2:")
table(km_2$cluster)
print("Cluster sizes for K=4:")
table(km_4$cluster)

# Compare silhouette scores
silhouette_km_2 <- silhouette(km_2$cluster, dist(x))
silhouette_km_4 <- silhouette(km_4$cluster, dist(x))
print("Silhouette scores for K=2:")
summary(silhouette_km_2)
print("Silhouette scores for K=4:")
summary(silhouette_km_4)


# Calculate confusion tables for K=2 and K=3 clustering
confuseTable.km2 <- table(df$quality, km_2$cluster)
confuseTable.km4 <- table(df$quality, km_4$cluster)
confuseTable.km2
confuseTable.km4


# Calculate accuracy, precision, recall, and F1-score for K=2 clustering
accuracy_km2 <- sum(diag(confuseTable.km2)) / sum(confuseTable.km2)
precision_km2 <- diag(confuseTable.km2) / colSums(confuseTable.km2)
recall_km2 <- diag(confuseTable.km2) / rowSums(confuseTable.km2)
f1_score_km2 <- 2 * precision_km2 * recall_km2 / (precision_km2 + recall_km2)

# Calculate accuracy, precision, recall, and F1-score for K=3 clustering
accuracy_km4 <- sum(diag(confuseTable.km4)) / sum(confuseTable.km4)
precision_km4 <- diag(confuseTable.km4) / colSums(confuseTable.km4)
recall_km4 <- diag(confuseTable.km4) / rowSums(confuseTable.km4)
f1_score_km4 <- 2 * precision_km4 * recall_km4 / (precision_km4 + recall_km4)

# Print evaluation metrics for K=2 clustering
print("Evaluation metrics for K=2 clustering:")
print(paste("Accuracy:", accuracy_km2))
print(paste("Precision:", mean(precision_km2)))
print(paste("Recall:", mean(recall_km2)))
print(paste("F1-score:", mean(f1_score_km2)))

# Print evaluation metrics for K=4 clustering
print("Evaluation metrics for K=4 clustering:")
print(paste("Accuracy:", accuracy_km4))
print(paste("Precision:", mean(precision_km4)))
print(paste("Recall:", mean(recall_km4)))
print(paste("F1-score:", mean(f1_score_km4)))

# Compute Rand Index for K=2 clustering
rand_index_km2 <- randIndex(confuseTable.km2)
print("Rand Index for K=2 clustering:")
print(rand_index_km2)

# Compute Rand Index for K=4 clustering
rand_index_km4 <- randIndex(confuseTable.km4)
print("Rand Index for K=4 clustering:")
print(rand_index_km4)


## ----------------------------------------------------------------------------##


