
###################################################################
#-------------1. Understanding the data -------------------------#
###################################################################


#-------------Loading Libraries and data -------------------------#

# Load libraries
library(readxl)  # Loads the readxl library for reading Excel files
library(NbClust)  # Loads the NbClust library for determining optimal number of clusters
library(corrplot)  # Loads the corrplot library for plotting correlation matrices
library(tidyr)  # Loads the tidyr library for data manipulation
library(MASS)  # Loads the MASS library for various statistical functions and datasets
library(ggplot2)  # Loads the ggplot2 library for creating plots


# Read data
df <- read_excel("Q1/whitewine.xlsx")



##############################################################
#---------- Exploration of Data -----------------------------#
##############################################################


# Displaying the first few rows of the dataset
head(df)

# Providing information about the dataset
str(df)

# Structure of the dataset
glimpse(df)

# Checking the dimension of the dataset
print(dim(df))

# Summary statistics of the dataset
summary(df)

# Checking for missing values
colSums(is.na(df))

# Displaying the column names
names(df)


##################################################################
#--------------Exploring each variable---------------------------#
##################################################################


## 1. fixed acidity

# Summary of Fixed Acidity
summary(df$`fixed acidity`)

# Boxplot of Fixed Acidity
boxplot(df$`fixed acidity`, col = "blue", xlab = "Fixed Acidity", ylab = "Values", main = "Boxplot of Fixed Acidity")

# Histogram of Fixed Acidity
hist(df$`fixed acidity`, col = "green", xlab = "Fixed Acidity", ylab = "Frequency", main = "Histogram of Fixed Acidity")

# Scatterplot of Fixed Acidity against Quality
p <- ggplot(df, aes(x = `fixed acidity`, y = quality)) +
  geom_point(color = "steelblue") +
  labs(title = "Scatterplot of Quality vs Fixed Acidity",
       x = "Fixed Acidity",
       y = "Quality")
print(p)

# Boxplot of Fixed Acidity by Quality
boxplot(df$`fixed acidity` ~ df$quality,
        xlab = "Quality",
        ylab = "Fixed Acidity",
        main = "Distribution of Fixed Acidity by Quality")

# Calculate quartiles and IQR
Q1 <- quantile(df$`fixed acidity`, 0.25)
Q3 <- quantile(df$`fixed acidity`, 0.75)
IQR <- Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- df$`fixed acidity` < lower_bound | df$`fixed acidity` > upper_bound

# Print outliers
print(df[outliers, ])




##----------------------------------------------------------------------------


## 2. volatile acidity

# Summary of volatile acidity
summary(df$`volatile acidity`)

# Boxplot of volatile acidity
boxplot(df$`volatile acidity`, col = "blue", xlab = "volatile acidity", ylab = "Values", main = "Boxplot of volatile acidity")

# Histogram of volatile acidity
hist(df$`volatile acidity`, col = "green", xlab = "volatile acidity", ylab = "Frequency", main = "Histogram of volatile acidity")

# Scatterplot of volatile acidity against Quality
p <- ggplot(df, aes(x = `volatile acidity`, y = quality)) +
  geom_point(color = "steelblue") +
  labs(title = "Scatterplot of Quality vs Volatile acidity",
       x = "Volatile acidity",
       y = "Quality")
print(p)

# Boxplot of volatile acidity by Quality
boxplot(df$`volatile acidity` ~ df$quality,
        xlab = "Quality",
        ylab = "Volatile acidity",
        main = "Distribution of volatile acidity by Quality")

# Calculate quartiles and IQR
Q1 <- quantile(df$`volatile acidity`, 0.25)
Q3 <- quantile(df$`volatile acidity`, 0.75)
IQR <- Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- df$`volatile acidity` < lower_bound | df$`volatile acidity` > upper_bound

# Print outliers
print(df[outliers, ])




##----------------------------------------------------------------------------


## 3. citric acid

# Summary of citric acid
summary(df$`citric acid`)

# Boxplot of citric acid
boxplot(df$`citric acid`, col = "blue", xlab = "citric acid", ylab = "Values", main = "Boxplot of citric acid")

# Histogram of citric acid
hist(df$`citric acid`, col = "green", xlab = "citric acid", ylab = "Frequency", main = "Histogram of citric acid")

# Scatterplot of citric acid against Quality
p <- ggplot(df, aes(x = `citric acid`, y = quality)) +
  geom_point(color = "steelblue") +
  labs(title = "Scatterplot of Quality vs citric acid",
       x = "Citric acid",
       y = "Quality")
print(p)

# Boxplot of citric acid by Quality
boxplot(df$`citric acid` ~ df$quality,
        xlab = "Quality",
        ylab = "citric acid",
        main = "Distribution of citric acid by Quality")

# Calculate quartiles and IQR
Q1 <- quantile(df$`citric acid`, 0.25)
Q3 <- quantile(df$`citric acid`, 0.75)
IQR <- Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- df$`citric acid` < lower_bound | df$`citric acid` > upper_bound

# Print outliers
print(df[outliers, ])



##----------------------------------------------------------------------------


## 4. residual sugar

# Summary of residual sugar
summary(df$`residual sugar`)

# Boxplot of residual sugar
boxplot(df$`residual sugar`, col = "blue", xlab = "residual sugar", ylab = "Values", main = "Boxplot of residual sugar")

# Histogram of residual sugar
hist(df$`residual sugar`, col = "green", xlab = "residual sugar", ylab = "Frequency", main = "Histogram of residual sugar")

# Scatterplot of residual sugar against Quality
p <- ggplot(df, aes(x = `residual sugar`, y = quality)) +
  geom_point(color = "steelblue") +
  labs(title = "Scatterplot of Quality vs residual sugar",
       x = "Residual sugar",
       y = "Quality")
print(p)

# Boxplot of residual sugar by Quality
boxplot(df$`residual sugar` ~ df$quality,
        xlab = "Quality",
        ylab = "residual sugar",
        main = "Distribution of residual sugar by Quality")


# Calculate quartiles and IQR
Q1 <- quantile(df$`residual sugar`, 0.25)
Q3 <- quantile(df$`residual sugar`, 0.75)
IQR <- Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- df$`residual sugar` < lower_bound | df$`residual sugar` > upper_bound

# Print outliers
print(df[outliers, ])



##----------------------------------------------------------------------------


## 5. chlorides

# Summary of chlorides
summary(df$`chlorides`)

# Boxplot of chlorides
boxplot(df$`chlorides`, col = "blue", xlab = "chlorides", ylab = "Values", main = "Boxplot of chlorides")

# Histogram of chlorides
hist(df$`chlorides`, col = "green", xlab = "chlorides", ylab = "Frequency", main = "Histogram of chlorides")

# Scatterplot of chlorides against Quality
p <- ggplot(df, aes(x = `chlorides`, y = quality)) +
  geom_point(color = "steelblue") +
  labs(title = "Scatterplot of Quality vs chlorides",
       x = "Chlorides",
       y = "Quality")
print(p)


# Boxplot of chlorides by Quality
boxplot(df$`chlorides` ~ df$quality,
        xlab = "Quality",
        ylab = "chlorides",
        main = "Distribution of chlorides by Quality")


# Calculate quartiles and IQR
Q1 <- quantile(df$`chlorides`, 0.25)
Q3 <- quantile(df$`chlorides`, 0.75)
IQR <- Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- df$`chlorides` < lower_bound | df$`chlorides` > upper_bound

# Print outliers
print(df[outliers, ])





##----------------------------------------------------------------------------


## 6. free sulfur dioxide

# Summary of free sulfur dioxide
summary(df$`free sulfur dioxide`)

# Boxplot of free sulfur dioxide
boxplot(df$`free sulfur dioxide`, col = "blue", xlab = "free sulfur dioxide", ylab = "Values", main = "Boxplot of free sulfur dioxide")

# Histogram of free sulfur dioxide
hist(df$`free sulfur dioxide`, col = "green", xlab = "free sulfur dioxide", ylab = "Frequency", main = "Histogram of free sulfur dioxide")

# Scatterplot of free sulfur dioxide against Quality
p <- ggplot(df, aes(x = `free sulfur dioxide`, y = quality)) +
  geom_point(color = "steelblue") +
  labs(title = "Scatterplot of Quality vs free sulfur dioxide",
       x = "Free sulfur dioxide",
       y = "Quality")
print(p)

# Boxplot of free sulfur dioxide by Quality
boxplot(df$`free sulfur dioxide` ~ df$quality,
        xlab = "Quality",
        ylab = "free sulfur dioxide",
        main = "Distribution of free sulfur dioxide by Quality")


# Calculate quartiles and IQR
Q1 <- quantile(df$`free sulfur dioxide`, 0.25)
Q3 <- quantile(df$`free sulfur dioxide`, 0.75)
IQR <- Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- df$`free sulfur dioxide` < lower_bound | df$`free sulfur dioxide` > upper_bound

# Print outliers
print(df[outliers, ])




##----------------------------------------------------------------------------


## 7. total sulfur dioxide

# Summary of total sulfur dioxide
summary(df$`total sulfur dioxide`)

# Boxplot of total sulfur dioxide
boxplot(df$`total sulfur dioxide`, col = "blue", xlab = "total sulfur dioxide", ylab = "Values", main = "Boxplot of total sulfur dioxide")

# Histogram of total sulfur dioxide
hist(df$`total sulfur dioxide`, col = "green", xlab = "total sulfur dioxide", ylab = "Frequency", main = "Histogram of total sulfur dioxide")

# Scatterplot of total sulfur dioxide against Quality
p <- ggplot(df, aes(x = `total sulfur dioxide`, y = quality)) +
  geom_point(color = "steelblue") +
  labs(title = "Scatterplot of Quality vs total sulfur dioxide",
       x = "Total sulfur dioxide",
       y = "Quality")
print(p)

# Boxplot of total sulfur dioxide by Quality
boxplot(df$`total sulfur dioxide` ~ df$quality,
        xlab = "Quality",
        ylab = "total sulfur dioxide",
        main = "Distribution of total sulfur dioxide by Quality")


# Calculate quartiles and IQR
Q1 <- quantile(df$`total sulfur dioxide`, 0.25)
Q3 <- quantile(df$`total sulfur dioxide`, 0.75)
IQR <- Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- df$`total sulfur dioxide` < lower_bound | df$`total sulfur dioxide` > upper_bound

# Print outliers
print(df[outliers, ])




##----------------------------------------------------------------------------


## 8. density

# Summary of density
summary(df$`density`)

# Boxplot of density
boxplot(df$`density`, col = "blue", xlab = "density", ylab = "Values", main = "Boxplot of density")

# Histogram of density
hist(df$`density`, col = "green", xlab = "density", ylab = "Frequency", main = "Histogram of density")

# Scatterplot of density against Quality
p <- ggplot(df, aes(x = `density`, y = quality)) +
  geom_point(color = "steelblue") +
  labs(title = "Scatterplot of Quality vs density",
       x = "density",
       y = "Quality")
print(p)


# Boxplot of density by Quality
boxplot(df$`density` ~ df$quality,
        xlab = "Quality",
        ylab = "density",
        main = "Distribution of density by Quality")

# Calculate quartiles and IQR
Q1 <- quantile(df$`density`, 0.25)
Q3 <- quantile(df$`density`, 0.75)
IQR <- Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- df$`density` < lower_bound | df$`density` > upper_bound

# Print outliers
print(df[outliers, ])



##----------------------------------------------------------------------------


## 9. pH

# Summary of pH
summary(df$`pH`)

# Boxplot of pH
boxplot(df$`pH`, col = "blue", xlab = "pH", ylab = "Values", main = "Boxplot of pH")

# Histogram of pH
hist(df$`pH`, col = "green", xlab = "pH", ylab = "Frequency", main = "Histogram of pH")

# Scatterplot of pH against Quality
p <- ggplot(df, aes(x = `pH`, y = quality)) +
  geom_point(color = "steelblue") +
  labs(title = "Scatterplot of Quality vs pH",
       x = "pH",
       y = "Quality")
print(p)


# Boxplot of pH by Quality
boxplot(df$`pH` ~ df$quality,
        xlab = "Quality",
        ylab = "pH",
        main = "Distribution of pH by Quality")


# Calculate quartiles and IQR
Q1 <- quantile(df$`pH`, 0.25)
Q3 <- quantile(df$`pH`, 0.75)
IQR <- Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- df$`pH` < lower_bound | df$`pH` > upper_bound

# Print outliers
print(df[outliers, ])



##----------------------------------------------------------------------------

## 10. sulphates

# Summary of sulphates
summary(df$`sulphates`)

# Boxplot of sulphates
boxplot(df$`sulphates`, col = "blue", xlab = "sulphates", ylab = "Values", main = "Boxplot of sulphates")

# Histogram of sulphates
hist(df$`sulphates`, col = "green", xlab = "sulphates", ylab = "Frequency", main = "Histogram of sulphates")

# Scatterplot of sulphates against Quality
p <- ggplot(df, aes(x = `sulphates`, y = quality)) +
  geom_point(color = "steelblue") +
  labs(title = "Scatterplot of Quality vs sulphates",
       x = "sulphates",
       y = "Quality")
print(p)


# Boxplot of sulphates by Quality
boxplot(df$`sulphates` ~ df$quality,
        xlab = "Quality",
        ylab = "sulphates",
        main = "Distribution of sulphates by Quality")


# Calculate quartiles and IQR
Q1 <- quantile(df$`sulphates`, 0.25)
Q3 <- quantile(df$`sulphates`, 0.75)
IQR <- Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- df$`sulphates` < lower_bound | df$`sulphates` > upper_bound

# Print outliers
print(df[outliers, ])




##----------------------------------------------------------------------------


## 11. alcohol

# Summary of alcohol
summary(df$`alcohol`)

# Boxplot of alcohol
boxplot(df$`alcohol`, col = "blue", xlab = "alcohol", ylab = "Values", main = "Boxplot of alcohol")

# Histogram of alcohol
hist(df$`alcohol`, col = "green", xlab = "alcohol", ylab = "Frequency", main = "Histogram of alcohol")

# Scatterplot of alcohol against Quality
p <- ggplot(df, aes(x = `alcohol`, y = quality)) +
  geom_point(color = "steelblue") +
  labs(title = "Scatterplot of Quality vs alcohol",
       x = "Alcohol",
       y = "Quality")
print(p)


# Boxplot of alcohol by Quality
boxplot(df$`alcohol` ~ df$quality,
        xlab = "Quality",
        ylab = "alcohol",
        main = "Distribution of alcohol by Quality")

# Calculate quartiles and IQR
Q1 <- quantile(df$`alcohol`, 0.25)
Q3 <- quantile(df$`alcohol`, 0.75)
IQR <- Q3 - Q1

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- df$`alcohol` < lower_bound | df$`alcohol` > upper_bound

# Print outliers
print(df[outliers, ])



##----------------------------------------------------------------------------


## 12. quality

# Summary of quality
summary(df$`quality`)

# Histogram of quality
hist(df$`quality`, col = "green", xlab = "quality", ylab = "Frequency", main = "Histogram of quality")

quality_frequency <- table(df$quality)
quality_frequency



#################################################################
##----------------------Coorelation matrix---------------------##
#################################################################



# Select columns of interest
cols_of_interest <- c("fixed acidity", "volatile acidity", "citric acid", "residual sugar", 
                      "chlorides", "free sulfur dioxide", "total sulfur dioxide", 
                      "density", "pH", "sulphates", "alcohol", "quality")

# Compute correlation matrix
correlation_matrix <- cor(df[, cols_of_interest])

# Print correlation matrix
print(correlation_matrix)

# Create correlation heatmap
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 30, 
         addCoef.col = "black", number.cex = 0.7)



# Compute the correlation matrix
correlation_matrix <- cor(df)

# Take the absolute values of the correlation matrix
absolute_correlation_matrix <- abs(correlation_matrix)

# Print or view the absolute correlation matrix
print(absolute_correlation_matrix)



#############################################################
#-----------All variables-----------------------------------#
#############################################################

df %>% 
  plot_histogram(
    geom_histogram_args = list("fill" = "#483D8B", "bins" = 20),
    title = "Frequency of all variables",
  )

plot_density(
  df, 
  geom_density_args = list("color" = "#483D8B", "fill" = "#483D8B", "alpha" = 0.5), 
  title = "Density of all variables",
)


###################################################################
#-------------2. Preprocessing of the data -------------------------#
###################################################################


#--------------  Loading Libraries and data -------------------------#



# Load libraries
library(readxl)  # Loads the readxl library for reading Excel files
library(tidyr)  # Loads the tidyr library for data manipulation
library(MASS)  # Loads the MASS library for various statistical functions and datasets



# Read data
df <- read_excel("DM_R_Answers//whitewine.xlsx")

# Summary statistics of the normalized dataset
summary(df)

dim(df)
########################################################################
#--------------------Z score with outliers -------------------------#
########################################################################

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
df_norm_z1 <- df %>%
  dplyr::mutate(
    across(
      all_of(cols_of_interest),
      .fns = z_score_normalization
    )
  )

# Summary statistics of the normalized dataset
summary(df_norm_z1)


########################################################################
#--------------------min-max with outliers -------------------------#
########################################################################


# Define min-max normalization function
min_max_normalization <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Columns of interest
cols_of_interest <- c(
  "fixed acidity", "volatile acidity", "citric acid",
  "residual sugar", "chlorides", "free sulfur dioxide",
  "total sulfur dioxide", "density", "pH",
  "sulphates", "alcohol"
)

# Apply min-max normalization to the dataset
df_norm_m1 <- df %>%
  dplyr::mutate(
    across(
      all_of(cols_of_interest),
      .fns = min_max_normalization
    )
  )

# Summary statistics of the normalized dataset
summary(df_norm_m1)

dim(df_norm_m1)


########################################################################
#--------------------Removing outliers -------------------------#
########################################################################


# Select columns of interest 
cols_of_interest <- c("fixed acidity", "volatile acidity", "citric acid", "residual sugar", "chlorides", "free sulfur dioxide", "total sulfur dioxide", 
                      "density", "pH", "sulphates", "alcohol")


# Function to remove outliers using z-score method
remove_outliers <- function(df, col) {
  z_scores <- abs(scale(df[[col]]))
  data_filtered <- df[z_scores < 3, ]
  return(data_filtered)
}

# Remove outliers for each feature
for (col in cols_of_interest) {
  df <- remove_outliers(df, col)
}

summary(df)

dim(df)


########################################################################
#--------------------min-max without outliers -------------------------#
########################################################################

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
df_norm_z2 <- df %>%
  dplyr::mutate(
    across(
      all_of(cols_of_interest),
      .fns = z_score_normalization
    )
  )

# Summary statistics of the normalized dataset
summary(df_norm_z2)


########################################################################
#--------------------min-max without outliers -------------------------#
########################################################################


# Define min-max normalization function
min_max_normalization <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Columns of interest
cols_of_interest <- c(
  "fixed acidity", "volatile acidity", "citric acid",
  "residual sugar", "chlorides", "free sulfur dioxide",
  "total sulfur dioxide", "density", "pH",
  "sulphates", "alcohol"
)

# Apply min-max normalization to the dataset
df_norm_m2 <- df %>%
  dplyr::mutate(
    across(
      all_of(cols_of_interest),
      .fns = min_max_normalization
    )
  )

# Summary statistics of the normalized dataset
summary(df_norm_m2)



#---------------------------------------------------------------
# Splitting features and target from x
x <- df_norm[, 1:11]  # Extracting the first 11 columns as features
y <- df_norm$quality  # Extracting the 12th column as the target variable

# --------------------------------------------------------


#---------------------------------------------------------------
# Splitting features and target from x
x <- df_norm[, 1:11]  # Extracting the first 11 columns as features
y <- df_norm$quality  # Extracting the 12th column as the target variable

# --------------------------------------------------------





###################################################################
#-------------3. Configuration 1 ---------------------------------#
#-------------No outlier eliminated , No scaling, nstart = 25-----#
###################################################################


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



# Splitting features and target
x <- df[, 1:11]  # Extracting the first 11 columns as features
y <- df$quality  # Extracting the 12th column as the target variable


######################################################################
##------ ------------------finding optimal clusters -----------------#
######################################################################

##---------------------NbClust method

# NbClust is an R Package for determining the relevant number of clusters in a dataset.
# It provides 30 indices which determine the number of clusters in a data set and it offers also the best clustering scheme from different results to the user.

set.seed(26)

cluster=NbClust(x,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")

barplot(table(cluster$Best.n[1,]), # provide bar charts####
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 30 Criteria")



cluster=NbClust(x,distance="manhattan", min.nc=2,max.nc=10,method="kmeans",index="all")

barplot(table(cluster$Best.n[1,]), # provide bar charts####
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 30 Criteria")


cluster=NbClust(x,distance="maximum", min.nc=2,max.nc=10,method="kmeans",index="all")

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

# Perform k-means clustering with K=3
set.seed(26)

km_3 <- kmeans(x, centers = 3, nstart = 25)
print(km_3)

# Plot 1: alcohol vs pH
plot(x[, c("alcohol", "pH")], col = km_3$cluster)
points(km_3$centers[, c("alcohol", "pH")], col = 1:3, pch = 23, cex = 3)


# Find mean of each cluster for km_2
aggregate(x, by=list(cluster=km_2$cluster), mean)

# Add cluster assignment to original data for km_2
final_data_km2 <- cbind(df, cluster = km_2$cluster)

# View final data for km_2
head(final_data_km2)



# Find mean of each cluster for km_3
aggregate(x, by=list(cluster=km_3$cluster), mean)

# Add cluster assignment to original data for km_3
final_data_km3 <- cbind(df, cluster = km_3$cluster)

# View final data for km_3
head(final_data_km3)


# use plotcluster function from fpc package to draw discriminant projection plot
plotcluster(x, km_2$cluster, main = "K=2 Clusters")
plotcluster(x, km_3$cluster, main = "K=3 Clusters")

# Next, we draw parallel coordinates plot to see how variables contributed in each cluster
parcoord(x, km_2$cluster, col = km_2$cluster, main = "Parallel Coordinates Plot for K=2 Clusters")
parcoord(x, km_3$cluster, col = km_3$cluster, main = "Parallel Coordinates Plot for K=3 Clusters")

# Visualize clusters for km_2
fviz_cluster(km_2, data = x)

# Visualize clusters for km_3
fviz_cluster(km_3, data = x)



############################################################################
#--------------------------------Validation--------------------------------#
############################################################################

# Compare cluster centers
print("Cluster centers for K=2:")
print(km_2$centers)
print("Cluster centers for K=3:")
print(km_3$centers)

# Compare cluster sizes
print("Cluster sizes for K=2:")
table(km_2$cluster)
print("Cluster sizes for K=3:")
table(km_3$cluster)

# Compare silhouette scores
silhouette_km_2 <- silhouette(km_2$cluster, dist(x))
silhouette_km_3 <- silhouette(km_3$cluster, dist(x))
print("Silhouette scores for K=2:")
summary(silhouette_km_2)
print("Silhouette scores for K=3:")
summary(silhouette_km_3)


# Calculate confusion tables for K=2 and K=3 clustering
confuseTable.km2 <- table(df$quality, km_2$cluster)
confuseTable.km3 <- table(df$quality, km_3$cluster)
confuseTable.km2
confuseTable.km3


# Calculate accuracy, precision, recall, and F1-score for K=2 clustering
precision_km2 <- diag(confuseTable.km2) / colSums(confuseTable.km2)
recall_km2 <- diag(confuseTable.km2) / rowSums(confuseTable.km2)
f1_score_km2 <- 2 * precision_km2 * recall_km2 / (precision_km2 + recall_km2)

# Calculate accuracy, precision, recall, and F1-score for K=3 clustering
precision_km3 <- diag(confuseTable.km3) / colSums(confuseTable.km3)
recall_km3 <- diag(confuseTable.km3) / rowSums(confuseTable.km3)
f1_score_km3 <- 2 * precision_km3 * recall_km3 / (precision_km3 + recall_km3)

# Print evaluation metrics for K=2 clustering
print("Evaluation metrics for K=2 clustering:")
print(paste("Precision:", mean(precision_km2)))
print(paste("Recall:", mean(recall_km2)))
print(paste("F1-score:", mean(f1_score_km2)))

# Print evaluation metrics for K=3 clustering
print("Evaluation metrics for K=3 clustering:")
print(paste("Precision:", mean(precision_km3)))
print(paste("Recall:", mean(recall_km3)))
print(paste("F1-score:", mean(f1_score_km3)))

# Compute Rand Index for K=2 clustering
rand_index_km2 <- randIndex(confuseTable.km2)
print("Rand Index for K=2 clustering:")
print(rand_index_km2)

# Compute Rand Index for K=3 clustering
rand_index_km3 <- randIndex(confuseTable.km3)
print("Rand Index for K=3 clustering:")
print(rand_index_km3)


## ----------------------------------------------------------------------------##



###################################################################
#-------------4. Configuration 2 ---------------------------------#
#-------------No outlier eliminated , Min-Max scaling, nstart = 25-----#
###################################################################

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

# Define min-max normalization function
min_max_normalization <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Columns of interest
cols_of_interest <- c(
  "fixed acidity", "volatile acidity", "citric acid",
  "residual sugar", "chlorides", "free sulfur dioxide",
  "total sulfur dioxide", "density", "pH",
  "sulphates", "alcohol"
)

# Apply min-max normalization to the dataset
df_norm <- df %>%
  dplyr::mutate(
    across(
      all_of(cols_of_interest),
      .fns = min_max_normalization
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

# Perform k-means clustering with K=3
set.seed(26)

km_3 <- kmeans(x, centers = 3, nstart = 25)
print(km_3)

# Plot 1: alcohol vs pH
plot(x[, c("alcohol", "pH")], col = km_3$cluster)
points(km_3$centers[, c("alcohol", "pH")], col = 1:3, pch = 23, cex = 3)

# Find mean of each cluster for km_2
aggregate(x, by=list(cluster=km_2$cluster), mean)

# Add cluster assignment to original data for km_2
final_data_km2 <- cbind(df, cluster = km_2$cluster)

# View final data for km_2
head(final_data_km2)



# Find mean of each cluster for km_3
aggregate(x, by=list(cluster=km_3$cluster), mean)

# Add cluster assignment to original data for km_3
final_data_km3 <- cbind(df, cluster = km_3$cluster)

# View final data for km_3
head(final_data_km3)


# use plotcluster function from fpc package to draw discriminant projection plot
plotcluster(x, km_2$cluster, main = "K=2 Clusters")


plotcluster(x, km_3$cluster, main = "K=3 Clusters")

# Next, we draw parallel coordinates plot to see how variables contributed in each cluster
parcoord(x, km_2$cluster, col = km_2$cluster, main = "Parallel Coordinates Plot for K=2 Clusters")



parcoord(x, km_3$cluster, col = km_3$cluster, main = "Parallel Coordinates Plot for K=3 Clusters")

# Visualize clusters for km_2
fviz_cluster(km_2, data = x)

# Visualize clusters for km_3
fviz_cluster(km_3, data = x)



############################################################################
#--------------------------------Validation--------------------------------#
############################################################################

# Compare cluster centers
print("Cluster centers for K=2:")
print(km_2$centers)
print("Cluster centers for K=3:")
print(km_3$centers)

# Compare cluster sizes
print("Cluster sizes for K=2:")
table(km_2$cluster)
print("Cluster sizes for K=3:")
table(km_3$cluster)

# Compare silhouette scores
silhouette_km_2 <- silhouette(km_2$cluster, dist(x))
silhouette_km_3 <- silhouette(km_3$cluster, dist(x))
print("Silhouette scores for K=2:")
summary(silhouette_km_2)
print("Silhouette scores for K=3:")
summary(silhouette_km_3)


# Calculate confusion tables for K=2 and K=3 clustering
confuseTable.km2 <- table(df$quality, km_2$cluster)
confuseTable.km3 <- table(df$quality, km_3$cluster)
confuseTable.km2
confuseTable.km3


# Calculate accuracy, precision, recall, and F1-score for K=2 clustering
accuracy_km2 <- sum(diag(confuseTable.km2)) / sum(confuseTable.km2)
precision_km2 <- diag(confuseTable.km2) / colSums(confuseTable.km2)
recall_km2 <- diag(confuseTable.km2) / rowSums(confuseTable.km2)
f1_score_km2 <- 2 * precision_km2 * recall_km2 / (precision_km2 + recall_km2)

# Calculate accuracy, precision, recall, and F1-score for K=3 clustering
accuracy_km3 <- sum(diag(confuseTable.km3)) / sum(confuseTable.km3)
precision_km3 <- diag(confuseTable.km3) / colSums(confuseTable.km3)
recall_km3 <- diag(confuseTable.km3) / rowSums(confuseTable.km3)
f1_score_km3 <- 2 * precision_km3 * recall_km3 / (precision_km3 + recall_km3)

# Print evaluation metrics for K=2 clustering
print("Evaluation metrics for K=2 clustering:")
print(paste("Accuracy:", accuracy_km2))
print(paste("Precision:", mean(precision_km2)))
print(paste("Recall:", mean(recall_km2)))
print(paste("F1-score:", mean(f1_score_km2)))

# Print evaluation metrics for K=3 clustering
print("Evaluation metrics for K=3 clustering:")
print(paste("Accuracy:", accuracy_km3))
print(paste("Precision:", mean(precision_km3)))
print(paste("Recall:", mean(recall_km3)))
print(paste("F1-score:", mean(f1_score_km3)))

# Compute Rand Index for K=2 clustering
rand_index_km2 <- randIndex(confuseTable.km2)
print("Rand Index for K=2 clustering:")
print(rand_index_km2)

# Compute Rand Index for K=3 clustering
rand_index_km3 <- randIndex(confuseTable.km3)
print("Rand Index for K=3 clustering:")
print(rand_index_km3)


## ----------------------------------------------------------------------------##



###################################################################
#-------------4. Configuration 3 ---------------------------------#
#-------------No outlier eliminated , Z score scaling, nstart = 25-----#
###################################################################


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




###################################################################
#-------------4. Configuration 4 ---------------------------------#
#------------- outlier eliminated , No scaling, nstart = 25-----#
###################################################################


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



# Select columns of interest
cols_of_interest <- c("fixed acidity", "volatile acidity", "citric acid", "residual sugar", "chlorides", "free sulfur dioxide", "total sulfur dioxide", 
                      "density", "pH", "sulphates", "alcohol")


# Function to remove outliers using z-score method
remove_outliers <- function(df, col) {
  z_scores <- abs(scale(df[[col]]))
  data_filtered <- df[z_scores < 3, ]
  return(data_filtered)
}

# Remove outliers for each feature
for (col in cols_of_interest) {
  df <- remove_outliers(df, col)
}


#---------------------------------------------------------------
# Splitting features and target from x
x <- df[, 1:11]  # Extracting the first 11 columns as features
y <- df$quality  # Extracting the 12th column as the target variable

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

# Perform k-means clustering with K=3
set.seed(26)

km_3 <- kmeans(x, centers = 3, nstart = 25)
print(km_3)

# Plot 1: alcohol vs pH
plot(x[, c("alcohol", "pH")], col = km_3$cluster)
points(km_3$centers[, c("alcohol", "pH")], col = 1:3, pch = 23, cex = 3)


# Find mean of each cluster for km_2
aggregate(x, by=list(cluster=km_2$cluster), mean)

# Add cluster assignment to original data for km_2
final_data_km2 <- cbind(df, cluster = km_2$cluster)

# View final data for km_2
head(final_data_km2)



# Find mean of each cluster for km_3
aggregate(x, by=list(cluster=km_3$cluster), mean)

# Add cluster assignment to original data for km_3
final_data_km3 <- cbind(df, cluster = km_3$cluster)

# View final data for km_3
head(final_data_km3)


# use plotcluster function from fpc package to draw discriminant projection plot
plotcluster(x, km_2$cluster, main = "K=2 Clusters")


plotcluster(x, km_3$cluster, main = "K=3 Clusters")

# Next, we draw parallel coordinates plot to see how variables contributed in each cluster
parcoord(x, km_2$cluster, col = km_2$cluster, main = "Parallel Coordinates Plot for K=2 Clusters")



parcoord(x, km_3$cluster, col = km_3$cluster, main = "Parallel Coordinates Plot for K=3 Clusters")

# Visualize clusters for km_2
fviz_cluster(km_2, data = x)

# Visualize clusters for km_3
fviz_cluster(km_3, data = x)



############################################################################
#--------------------------------Validation--------------------------------#
############################################################################

# Compare cluster centers
print("Cluster centers for K=2:")
print(km_2$centers)
print("Cluster centers for K=3:")
print(km_3$centers)

# Compare cluster sizes
print("Cluster sizes for K=2:")
table(km_2$cluster)
print("Cluster sizes for K=3:")
table(km_3$cluster)

# Compare silhouette scores
silhouette_km_2 <- silhouette(km_2$cluster, dist(x))
silhouette_km_3 <- silhouette(km_3$cluster, dist(x))
print("Silhouette scores for K=2:")
summary(silhouette_km_2)
print("Silhouette scores for K=3:")
summary(silhouette_km_3)


# Calculate confusion tables for K=2 and K=3 clustering
confuseTable.km2 <- table(df$quality, km_2$cluster)
confuseTable.km3 <- table(df$quality, km_3$cluster)
confuseTable.km2
confuseTable.km3


# Calculate accuracy, precision, recall, and F1-score for K=2 clustering
accuracy_km2 <- sum(diag(confuseTable.km2)) / sum(confuseTable.km2)
precision_km2 <- diag(confuseTable.km2) / colSums(confuseTable.km2)
recall_km2 <- diag(confuseTable.km2) / rowSums(confuseTable.km2)
f1_score_km2 <- 2 * precision_km2 * recall_km2 / (precision_km2 + recall_km2)

# Calculate accuracy, precision, recall, and F1-score for K=3 clustering
accuracy_km3 <- sum(diag(confuseTable.km3)) / sum(confuseTable.km3)
precision_km3 <- diag(confuseTable.km3) / colSums(confuseTable.km3)
recall_km3 <- diag(confuseTable.km3) / rowSums(confuseTable.km3)
f1_score_km3 <- 2 * precision_km3 * recall_km3 / (precision_km3 + recall_km3)

# Print evaluation metrics for K=2 clustering
print("Evaluation metrics for K=2 clustering:")
print(paste("Accuracy:", accuracy_km2))
print(paste("Precision:", mean(precision_km2)))
print(paste("Recall:", mean(recall_km2)))
print(paste("F1-score:", mean(f1_score_km2)))

# Print evaluation metrics for K=3 clustering
print("Evaluation metrics for K=3 clustering:")
print(paste("Accuracy:", accuracy_km3))
print(paste("Precision:", mean(precision_km3)))
print(paste("Recall:", mean(recall_km3)))
print(paste("F1-score:", mean(f1_score_km3)))

# Compute Rand Index for K=2 clustering
rand_index_km2 <- randIndex(confuseTable.km2)
print("Rand Index for K=2 clustering:")
print(rand_index_km2)

# Compute Rand Index for K=3 clustering
rand_index_km3 <- randIndex(confuseTable.km3)
print("Rand Index for K=3 clustering:")
print(rand_index_km3)


## ----------------------------------------------------------------------------##


###################################################################
#-------------6. Configuration 5 ---------------------------------#
#------------- outlier eliminated , Min-MAx scaling, nstart = 25-----#
###################################################################


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
library(factoextra) # 


# Read data
df <- read_excel("DM_R_Answers//whitewine.xlsx")


########################################################################
#--------------------Preprocessing of Data -------------------------#
########################################################################

# Select columns of interest 
cols_of_interest <- c("fixed acidity", "volatile acidity", "citric acid", "residual sugar", "chlorides", "free sulfur dioxide", "total sulfur dioxide", 
                      "density", "pH", "sulphates", "alcohol")


# Function to remove outliers using z-score method
remove_outliers <- function(df, col) {
  z_scores <- abs(scale(df[[col]]))
  data_filtered <- df[z_scores < 3, ]
  return(data_filtered)
}

# Remove outliers for each feature
for (col in cols_of_interest) {
  df <- remove_outliers(df, col)
}


# Define min-max normalization function
min_max_normalization <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Columns of interest
cols_of_interest <- c(
  "fixed acidity", "volatile acidity", "citric acid",
  "residual sugar", "chlorides", "free sulfur dioxide",
  "total sulfur dioxide", "density", "pH",
  "sulphates", "alcohol"
)

# Apply min-max normalization to the dataset
df_norm <- df %>%
  dplyr::mutate(
    across(
      all_of(cols_of_interest),
      .fns = min_max_normalization
    )
  )

# Summary statistics of the normalized dataset
summary(df_norm)



#---------------------------------------------------------------
# Splitting features and target from x
x <- df_norm[, 1:11]  # Extracting the first 11 columns as features
y <- df_norm$quality  # Extracting the 12th column as the target variable

# --------------------------------------------------------

######################################################################
##------ ------------------finding optimal clusters -----------------#
######################################################################

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

# Perform k-means clustering with K=3
set.seed(26)

km_3 <- kmeans(x, centers = 3, nstart = 25)
print(km_3)

# Plot 1: alcohol vs pH
plot(x[, c("alcohol", "pH")], col = km_3$cluster)
points(km_3$centers[, c("alcohol", "pH")], col = 1:3, pch = 23, cex = 3)


# Find mean of each cluster for km_2
aggregate(x, by=list(cluster=km_2$cluster), mean)

# Add cluster assignment to original data for km_2
final_data_km2 <- cbind(df, cluster = km_2$cluster)

# View final data for km_2
head(final_data_km2)



# Find mean of each cluster for km_3
aggregate(x, by=list(cluster=km_3$cluster), mean)

# Add cluster assignment to original data for km_3
final_data_km3 <- cbind(df, cluster = km_3$cluster)

# View final data for km_3
head(final_data_km3)


# use plotcluster function from fpc package to draw discriminant projection plot
plotcluster(x, km_2$cluster, main = "K=2 Clusters")


plotcluster(x, km_3$cluster, main = "K=3 Clusters")

# Next, we draw parallel coordinates plot to see how variables contributed in each cluster
parcoord(x, km_2$cluster, col = km_2$cluster, main = "Parallel Coordinates Plot for K=2 Clusters")



parcoord(x, km_3$cluster, col = km_3$cluster, main = "Parallel Coordinates Plot for K=3 Clusters")

# Visualize clusters for km_2
fviz_cluster(km_2, data = x)

# Visualize clusters for km_3
fviz_cluster(km_3, data = x)



############################################################################
#--------------------------------Validation--------------------------------#
############################################################################

# Compare cluster centers
print("Cluster centers for K=2:")
print(km_2$centers)
print("Cluster centers for K=3:")
print(km_3$centers)

# Compare cluster sizes
print("Cluster sizes for K=2:")
table(km_2$cluster)
print("Cluster sizes for K=3:")
table(km_3$cluster)

# Compare silhouette scores
silhouette_km_2 <- silhouette(km_2$cluster, dist(x))
silhouette_km_3 <- silhouette(km_3$cluster, dist(x))
print("Silhouette scores for K=2:")
summary(silhouette_km_2)
print("Silhouette scores for K=3:")
summary(silhouette_km_3)


# Calculate confusion tables for K=2 and K=3 clustering
confuseTable.km2 <- table(df$quality, km_2$cluster)
confuseTable.km3 <- table(df$quality, km_3$cluster)
confuseTable.km2
confuseTable.km3


# Calculate accuracy, precision, recall, and F1-score for K=2 clustering
accuracy_km2 <- sum(diag(confuseTable.km2)) / sum(confuseTable.km2)
precision_km2 <- diag(confuseTable.km2) / colSums(confuseTable.km2)
recall_km2 <- diag(confuseTable.km2) / rowSums(confuseTable.km2)
f1_score_km2 <- 2 * precision_km2 * recall_km2 / (precision_km2 + recall_km2)

# Calculate accuracy, precision, recall, and F1-score for K=3 clustering
accuracy_km3 <- sum(diag(confuseTable.km3)) / sum(confuseTable.km3)
precision_km3 <- diag(confuseTable.km3) / colSums(confuseTable.km3)
recall_km3 <- diag(confuseTable.km3) / rowSums(confuseTable.km3)
f1_score_km3 <- 2 * precision_km3 * recall_km3 / (precision_km3 + recall_km3)

# Print evaluation metrics for K=2 clustering
print("Evaluation metrics for K=2 clustering:")
print(paste("Accuracy:", accuracy_km2))
print(paste("Precision:", mean(precision_km2)))
print(paste("Recall:", mean(recall_km2)))
print(paste("F1-score:", mean(f1_score_km2)))

# Print evaluation metrics for K=3 clustering
print("Evaluation metrics for K=3 clustering:")
print(paste("Accuracy:", accuracy_km3))
print(paste("Precision:", mean(precision_km3)))
print(paste("Recall:", mean(recall_km3)))
print(paste("F1-score:", mean(f1_score_km3)))

# Compute Rand Index for K=2 clustering
rand_index_km2 <- randIndex(confuseTable.km2)
print("Rand Index for K=2 clustering:")
print(rand_index_km2)

# Compute Rand Index for K=3 clustering
rand_index_km3 <- randIndex(confuseTable.km3)
print("Rand Index for K=3 clustering:")
print(rand_index_km3)


## ----------------------------------------------------------------------------##

###################################################################
#-------------6. Configuration 6 ---------------------------------#
#------------- outlier eliminated ,Z-score scaling, nstart = 25-----#
###################################################################



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


########################################################################
#--------------------Preprocessing of Data -------------------------#
########################################################################

# Select columns of interest 
cols_of_interest <- c("fixed acidity", "volatile acidity", "citric acid", "residual sugar", "chlorides", "free sulfur dioxide", "total sulfur dioxide", 
                      "density", "pH", "sulphates", "alcohol")


# Function to remove outliers using z-score method
remove_outliers <- function(df, col) {
  z_scores <- abs(scale(df[[col]]))
  data_filtered <- df[z_scores < 3, ]
  return(data_filtered)
}

# Remove outliers for each feature
for (col in cols_of_interest) {
  df <- remove_outliers(df, col)
}




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


######################################################################
##------ ------------------finding optimal clusters -----------------#
######################################################################

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

# Perform k-means clustering with K=3
set.seed(26)

km_3 <- kmeans(x, centers = 3, nstart = 25)
print(km_3)

# Plot 1: alcohol vs pH
plot(x[, c("alcohol", "pH")], col = km_3$cluster)
points(km_3$centers[, c("alcohol", "pH")], col = 1:3, pch = 23, cex = 3)


# Find mean of each cluster for km_2
aggregate(x, by=list(cluster=km_2$cluster), mean)

# Add cluster assignment to original data for km_2
final_data_km2 <- cbind(df, cluster = km_2$cluster)

# View final data for km_2
head(final_data_km2)



# Find mean of each cluster for km_3
aggregate(x, by=list(cluster=km_3$cluster), mean)

# Add cluster assignment to original data for km_3
final_data_km3 <- cbind(df, cluster = km_3$cluster)

# View final data for km_3
head(final_data_km3)


# use plotcluster function from fpc package to draw discriminant projection plot
plotcluster(x, km_2$cluster, main = "K=2 Clusters")


plotcluster(x, km_3$cluster, main = "K=3 Clusters")

# Next, we draw parallel coordinates plot to see how variables contributed in each cluster
parcoord(x, km_2$cluster, col = km_2$cluster, main = "Parallel Coordinates Plot for K=2 Clusters")



parcoord(x, km_3$cluster, col = km_3$cluster, main = "Parallel Coordinates Plot for K=3 Clusters")

# Visualize clusters for km_2
fviz_cluster(km_2, data = x)

# Visualize clusters for km_3
fviz_cluster(km_3, data = x)



############################################################################
#--------------------------------Validation--------------------------------#
############################################################################

# Compare cluster centers
print("Cluster centers for K=2:")
print(km_2$centers)
print("Cluster centers for K=3:")
print(km_3$centers)

# Compare cluster sizes
print("Cluster sizes for K=2:")
table(km_2$cluster)
print("Cluster sizes for K=3:")
table(km_3$cluster)

# Compare silhouette scores
silhouette_km_2 <- silhouette(km_2$cluster, dist(x))
silhouette_km_3 <- silhouette(km_3$cluster, dist(x))
print("Silhouette scores for K=2:")
summary(silhouette_km_2)
print("Silhouette scores for K=3:")
summary(silhouette_km_3)


# Calculate confusion tables for K=2 and K=3 clustering
confuseTable.km2 <- table(df$quality, km_2$cluster)
confuseTable.km3 <- table(df$quality, km_3$cluster)
confuseTable.km2
confuseTable.km3


# Calculate accuracy, precision, recall, and F1-score for K=2 clustering
accuracy_km2 <- sum(diag(confuseTable.km2)) / sum(confuseTable.km2)
precision_km2 <- diag(confuseTable.km2) / colSums(confuseTable.km2)
recall_km2 <- diag(confuseTable.km2) / rowSums(confuseTable.km2)
f1_score_km2 <- 2 * precision_km2 * recall_km2 / (precision_km2 + recall_km2)

# Calculate accuracy, precision, recall, and F1-score for K=3 clustering
accuracy_km3 <- sum(diag(confuseTable.km3)) / sum(confuseTable.km3)
precision_km3 <- diag(confuseTable.km3) / colSums(confuseTable.km3)
recall_km3 <- diag(confuseTable.km3) / rowSums(confuseTable.km3)
f1_score_km3 <- 2 * precision_km3 * recall_km3 / (precision_km3 + recall_km3)

# Print evaluation metrics for K=2 clustering
print("Evaluation metrics for K=2 clustering:")
print(paste("Accuracy:", accuracy_km2))
print(paste("Precision:", mean(precision_km2)))
print(paste("Recall:", mean(recall_km2)))
print(paste("F1-score:", mean(f1_score_km2)))

# Print evaluation metrics for K=3 clustering
print("Evaluation metrics for K=3 clustering:")
print(paste("Accuracy:", accuracy_km3))
print(paste("Precision:", mean(precision_km3)))
print(paste("Recall:", mean(recall_km3)))
print(paste("F1-score:", mean(f1_score_km3)))

# Compute Rand Index for K=2 clustering
rand_index_km2 <- randIndex(confuseTable.km2)
print("Rand Index for K=2 clustering:")
print(rand_index_km2)

# Compute Rand Index for K=3 clustering
rand_index_km3 <- randIndex(confuseTable.km3)
print("Rand Index for K=3 clustering:")
print(rand_index_km3)


## ----------------------------------------------------------------------------##



###################################################################
#-------------8. Configuration 7 ---------------------------------#
#------------- outlier eliminated ,Min-MAx scaling, nstart = 50-----#
###################################################################


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


########################################################################
#--------------------Preprocessing of Data -------------------------#
########################################################################

# Select columns of interest 
cols_of_interest <- c("fixed acidity", "volatile acidity", "citric acid", "residual sugar", "chlorides", "free sulfur dioxide", "total sulfur dioxide", 
                      "density", "pH", "sulphates", "alcohol")


# Function to remove outliers using z-score method
remove_outliers <- function(df, col) {
  z_scores <- abs(scale(df[[col]]))
  data_filtered <- df[z_scores < 3, ]
  return(data_filtered)
}

# Remove outliers for each feature
for (col in cols_of_interest) {
  df <- remove_outliers(df, col)
}


# Define min-max normalization function
min_max_normalization <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Columns of interest
cols_of_interest <- c(
  "fixed acidity", "volatile acidity", "citric acid",
  "residual sugar", "chlorides", "free sulfur dioxide",
  "total sulfur dioxide", "density", "pH",
  "sulphates", "alcohol"
)

# Apply min-max normalization to the dataset
df_norm <- df %>%
  dplyr::mutate(
    across(
      all_of(cols_of_interest),
      .fns = min_max_normalization
    )
  )

# Summary statistics of the normalized dataset
summary(df_norm)



#---------------------------------------------------------------
# Splitting features and target from x
x <- df_norm[, 1:11]  # Extracting the first 11 columns as features
y <- df_norm$quality  # Extracting the 12th column as the target variable

# --------------------------------------------------------

######################################################################
##------ ------------------finding optimal clusters -----------------#
######################################################################

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
km_2 <- kmeans(x, centers = 2, nstart = 50)

print(km_2)


# Plot 1: alcohol vs pH
plot(x[, c("alcohol", "pH")], col = km_2$cluster)
points(km_2$centers[, c("alcohol", "pH")], col = 1:3, pch = 23, cex = 3)

# Perform k-means clustering with K=3
set.seed(26)

km_3 <- kmeans(x, centers = 3, nstart = 50)
print(km_3)

# Plot 1: alcohol vs pH
plot(x[, c("alcohol", "pH")], col = km_3$cluster)
points(km_3$centers[, c("alcohol", "pH")], col = 1:3, pch = 23, cex = 3)


# Find mean of each cluster for km_2
aggregate(x, by=list(cluster=km_2$cluster), mean)

# Add cluster assignment to original data for km_2
final_data_km2 <- cbind(df, cluster = km_2$cluster)

# View final data for km_2
head(final_data_km2)



# Find mean of each cluster for km_3
aggregate(x, by=list(cluster=km_3$cluster), mean)

# Add cluster assignment to original data for km_3
final_data_km3 <- cbind(df, cluster = km_3$cluster)

# View final data for km_3
head(final_data_km3)


# use plotcluster function from fpc package to draw discriminant projection plot
plotcluster(x, km_2$cluster, main = "K=2 Clusters")


plotcluster(x, km_3$cluster, main = "K=3 Clusters")

# Next, we draw parallel coordinates plot to see how variables contributed in each cluster
parcoord(x, km_2$cluster, col = km_2$cluster, main = "Parallel Coordinates Plot for K=2 Clusters")



parcoord(x, km_3$cluster, col = km_3$cluster, main = "Parallel Coordinates Plot for K=3 Clusters")

# Visualize clusters for km_2
fviz_cluster(km_2, data = x)

# Visualize clusters for km_3
fviz_cluster(km_3, data = x)



############################################################################
#--------------------------------Validation--------------------------------#
############################################################################

# Compare cluster centers
print("Cluster centers for K=2:")
print(km_2$centers)
print("Cluster centers for K=3:")
print(km_3$centers)

# Compare cluster sizes
print("Cluster sizes for K=2:")
table(km_2$cluster)
print("Cluster sizes for K=3:")
table(km_3$cluster)

# Compare silhouette scores
silhouette_km_2 <- silhouette(km_2$cluster, dist(x))
silhouette_km_3 <- silhouette(km_3$cluster, dist(x))
print("Silhouette scores for K=2:")
summary(silhouette_km_2)
print("Silhouette scores for K=3:")
summary(silhouette_km_3)


# Calculate confusion tables for K=2 and K=3 clustering
confuseTable.km2 <- table(df$quality, km_2$cluster)
confuseTable.km3 <- table(df$quality, km_3$cluster)
confuseTable.km2
confuseTable.km3


# Calculate accuracy, precision, recall, and F1-score for K=2 clustering
accuracy_km2 <- sum(diag(confuseTable.km2)) / sum(confuseTable.km2)
precision_km2 <- diag(confuseTable.km2) / colSums(confuseTable.km2)
recall_km2 <- diag(confuseTable.km2) / rowSums(confuseTable.km2)
f1_score_km2 <- 2 * precision_km2 * recall_km2 / (precision_km2 + recall_km2)

# Calculate accuracy, precision, recall, and F1-score for K=3 clustering
accuracy_km3 <- sum(diag(confuseTable.km3)) / sum(confuseTable.km3)
precision_km3 <- diag(confuseTable.km3) / colSums(confuseTable.km3)
recall_km3 <- diag(confuseTable.km3) / rowSums(confuseTable.km3)
f1_score_km3 <- 2 * precision_km3 * recall_km3 / (precision_km3 + recall_km3)

# Print evaluation metrics for K=2 clustering
print("Evaluation metrics for K=2 clustering:")
print(paste("Accuracy:", accuracy_km2))
print(paste("Precision:", mean(precision_km2)))
print(paste("Recall:", mean(recall_km2)))
print(paste("F1-score:", mean(f1_score_km2)))

# Print evaluation metrics for K=3 clustering
print("Evaluation metrics for K=3 clustering:")
print(paste("Accuracy:", accuracy_km3))
print(paste("Precision:", mean(precision_km3)))
print(paste("Recall:", mean(recall_km3)))
print(paste("F1-score:", mean(f1_score_km3)))

# Compute Rand Index for K=2 clustering
rand_index_km2 <- randIndex(confuseTable.km2)
print("Rand Index for K=2 clustering:")
print(rand_index_km2)

# Compute Rand Index for K=3 clustering
rand_index_km3 <- randIndex(confuseTable.km3)
print("Rand Index for K=3 clustering:")
print(rand_index_km3)


## ----------------------------------------------------------------------------##





###################################################################
#-------------11. Configuration 9 ---------------------------------#
#------------- outlier eliminated ,Z-score scaling, nstart = 10-----#
###################################################################


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



# Read data
df <- read_excel("DM_R_Answers//whitewine.xlsx")


########################################################################
#--------------------Preprocessing of Data -------------------------#
########################################################################

# Select columns of interest 
cols_of_interest <- c("fixed acidity", "volatile acidity", "citric acid", "residual sugar", "chlorides", "free sulfur dioxide", "total sulfur dioxide", 
                      "density", "pH", "sulphates", "alcohol")


# Function to remove outliers using z-score method
remove_outliers <- function(df, col) {
  z_scores <- abs(scale(df[[col]]))
  data_filtered <- df[z_scores < 3, ]
  return(data_filtered)
}

# Remove outliers for each feature
for (col in cols_of_interest) {
  df <- remove_outliers(df, col)
}




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


######################################################################
##------ ------------------finding optimal clusters -----------------#
######################################################################

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
km_2 <- kmeans(x, centers = 2, nstart = 50)

print(km_2)


# Plot 1: alcohol vs pH
plot(x[, c("alcohol", "pH")], col = km_2$cluster)
points(km_2$centers[, c("alcohol", "pH")], col = 1:3, pch = 23, cex = 3)

# Perform k-means clustering with K=3
set.seed(26)

km_3 <- kmeans(x, centers = 3, nstart = 50)
print(km_3)

# Plot 1: alcohol vs pH
plot(x[, c("alcohol", "pH")], col = km_3$cluster)
points(km_3$centers[, c("alcohol", "pH")], col = 1:3, pch = 23, cex = 3)


# Find mean of each cluster for km_2
aggregate(x, by=list(cluster=km_2$cluster), mean)

# Add cluster assignment to original data for km_2
final_data_km2 <- cbind(df, cluster = km_2$cluster)

# View final data for km_2
head(final_data_km2)



# Find mean of each cluster for km_3
aggregate(x, by=list(cluster=km_3$cluster), mean)

# Add cluster assignment to original data for km_3
final_data_km3 <- cbind(df, cluster = km_3$cluster)

# View final data for km_3
head(final_data_km3)


# use plotcluster function from fpc package to draw discriminant projection plot
plotcluster(x, km_2$cluster, main = "K=2 Clusters")


plotcluster(x, km_3$cluster, main = "K=3 Clusters")

# Next, we draw parallel coordinates plot to see how variables contributed in each cluster
parcoord(x, km_2$cluster, col = km_2$cluster, main = "Parallel Coordinates Plot for K=2 Clusters")



parcoord(x, km_3$cluster, col = km_3$cluster, main = "Parallel Coordinates Plot for K=3 Clusters")

# Visualize clusters for km_2
fviz_cluster(km_2, data = x)

# Visualize clusters for km_3
fviz_cluster(km_3, data = x)



############################################################################
#--------------------------------Validation--------------------------------#
############################################################################

# Compare cluster centers
print("Cluster centers for K=2:")
print(km_2$centers)
print("Cluster centers for K=3:")
print(km_3$centers)

# Compare cluster sizes
print("Cluster sizes for K=2:")
table(km_2$cluster)
print("Cluster sizes for K=3:")
table(km_3$cluster)

# Compare silhouette scores
silhouette_km_2 <- silhouette(km_2$cluster, dist(x))
silhouette_km_3 <- silhouette(km_3$cluster, dist(x))
print("Silhouette scores for K=2:")
summary(silhouette_km_2)
print("Silhouette scores for K=3:")
summary(silhouette_km_3)


# Calculate confusion tables for K=2 and K=3 clustering
confuseTable.km2 <- table(df$quality, km_2$cluster)
confuseTable.km3 <- table(df$quality, km_3$cluster)
confuseTable.km2
confuseTable.km3


# Calculate accuracy, precision, recall, and F1-score for K=2 clustering
accuracy_km2 <- sum(diag(confuseTable.km2)) / sum(confuseTable.km2)
precision_km2 <- diag(confuseTable.km2) / colSums(confuseTable.km2)
recall_km2 <- diag(confuseTable.km2) / rowSums(confuseTable.km2)
f1_score_km2 <- 2 * precision_km2 * recall_km2 / (precision_km2 + recall_km2)

# Calculate accuracy, precision, recall, and F1-score for K=3 clustering
accuracy_km3 <- sum(diag(confuseTable.km3)) / sum(confuseTable.km3)
precision_km3 <- diag(confuseTable.km3) / colSums(confuseTable.km3)
recall_km3 <- diag(confuseTable.km3) / rowSums(confuseTable.km3)
f1_score_km3 <- 2 * precision_km3 * recall_km3 / (precision_km3 + recall_km3)

# Print evaluation metrics for K=2 clustering
print("Evaluation metrics for K=2 clustering:")
print(paste("Accuracy:", accuracy_km2))
print(paste("Precision:", mean(precision_km2)))
print(paste("Recall:", mean(recall_km2)))
print(paste("F1-score:", mean(f1_score_km2)))

# Print evaluation metrics for K=3 clustering
print("Evaluation metrics for K=3 clustering:")
print(paste("Accuracy:", accuracy_km3))
print(paste("Precision:", mean(precision_km3)))
print(paste("Recall:", mean(recall_km3)))
print(paste("F1-score:", mean(f1_score_km3)))

# Compute Rand Index for K=2 clustering
rand_index_km2 <- randIndex(confuseTable.km2)
print("Rand Index for K=2 clustering:")
print(rand_index_km2)

# Compute Rand Index for K=3 clustering
rand_index_km3 <- randIndex(confuseTable.km3)
print("Rand Index for K=3 clustering:")
print(rand_index_km3)


## ----------------------------------------------------------------------------##



###################################################################
#-------------8. Configuration 9 ---------------------------------#
#------------- outlier eliminated ,Z-score scaling, nstart = 50-----#
###################################################################




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


########################################################################
#--------------------Preprocessing of Data -------------------------#
########################################################################

# Select columns of interest 
cols_of_interest <- c("fixed acidity", "volatile acidity", "citric acid", "residual sugar", "chlorides", "free sulfur dioxide", "total sulfur dioxide", 
                      "density", "pH", "sulphates", "alcohol")


# Function to remove outliers using z-score method
remove_outliers <- function(df, col) {
  z_scores <- abs(scale(df[[col]]))
  data_filtered <- df[z_scores < 3, ]
  return(data_filtered)
}

# Remove outliers for each feature
for (col in cols_of_interest) {
  df <- remove_outliers(df, col)
}




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


######################################################################
##------ ------------------finding optimal clusters -----------------#
######################################################################

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
km_2 <- kmeans(x, centers = 2, nstart = 10)

print(km_2)


# Plot 1: alcohol vs pH
plot(x[, c("alcohol", "pH")], col = km_2$cluster)
points(km_2$centers[, c("alcohol", "pH")], col = 1:3, pch = 23, cex = 3)

# Perform k-means clustering with K=3
set.seed(26)

km_3 <- kmeans(x, centers = 3, nstart = 10)
print(km_3)

# Plot 1: alcohol vs pH
plot(x[, c("alcohol", "pH")], col = km_3$cluster)
points(km_3$centers[, c("alcohol", "pH")], col = 1:3, pch = 23, cex = 3)


# Find mean of each cluster for km_2
aggregate(x, by=list(cluster=km_2$cluster), mean)

# Add cluster assignment to original data for km_2
final_data_km2 <- cbind(df, cluster = km_2$cluster)

# View final data for km_2
head(final_data_km2)



# Find mean of each cluster for km_3
aggregate(x, by=list(cluster=km_3$cluster), mean)

# Add cluster assignment to original data for km_3
final_data_km3 <- cbind(df, cluster = km_3$cluster)

# View final data for km_3
head(final_data_km3)


# use plotcluster function from fpc package to draw discriminant projection plot
plotcluster(x, km_2$cluster, main = "K=2 Clusters")


plotcluster(x, km_3$cluster, main = "K=3 Clusters")

# Next, we draw parallel coordinates plot to see how variables contributed in each cluster
parcoord(x, km_2$cluster, col = km_2$cluster, main = "Parallel Coordinates Plot for K=2 Clusters")



parcoord(x, km_3$cluster, col = km_3$cluster, main = "Parallel Coordinates Plot for K=3 Clusters")

# Visualize clusters for km_2
fviz_cluster(km_2, data = x)

# Visualize clusters for km_3
fviz_cluster(km_3, data = x)



############################################################################
#--------------------------------Validation--------------------------------#
############################################################################

# Compare cluster centers
print("Cluster centers for K=2:")
print(km_2$centers)
print("Cluster centers for K=3:")
print(km_3$centers)

# Compare cluster sizes
print("Cluster sizes for K=2:")
table(km_2$cluster)
print("Cluster sizes for K=3:")
table(km_3$cluster)

# Compare silhouette scores
silhouette_km_2 <- silhouette(km_2$cluster, dist(x))
silhouette_km_3 <- silhouette(km_3$cluster, dist(x))
print("Silhouette scores for K=2:")
summary(silhouette_km_2)
print("Silhouette scores for K=3:")
summary(silhouette_km_3)


# Calculate confusion tables for K=2 and K=3 clustering
confuseTable.km2 <- table(df$quality, km_2$cluster)
confuseTable.km3 <- table(df$quality, km_3$cluster)
confuseTable.km2
confuseTable.km3


# Calculate accuracy, precision, recall, and F1-score for K=2 clustering
accuracy_km2 <- sum(diag(confuseTable.km2)) / sum(confuseTable.km2)
precision_km2 <- diag(confuseTable.km2) / colSums(confuseTable.km2)
recall_km2 <- diag(confuseTable.km2) / rowSums(confuseTable.km2)
f1_score_km2 <- 2 * precision_km2 * recall_km2 / (precision_km2 + recall_km2)

# Calculate accuracy, precision, recall, and F1-score for K=3 clustering
accuracy_km3 <- sum(diag(confuseTable.km3)) / sum(confuseTable.km3)
precision_km3 <- diag(confuseTable.km3) / colSums(confuseTable.km3)
recall_km3 <- diag(confuseTable.km3) / rowSums(confuseTable.km3)
f1_score_km3 <- 2 * precision_km3 * recall_km3 / (precision_km3 + recall_km3)

# Print evaluation metrics for K=2 clustering
print("Evaluation metrics for K=2 clustering:")
print(paste("Accuracy:", accuracy_km2))
print(paste("Precision:", mean(precision_km2)))
print(paste("Recall:", mean(recall_km2)))
print(paste("F1-score:", mean(f1_score_km2)))

# Print evaluation metrics for K=3 clustering
print("Evaluation metrics for K=3 clustering:")
print(paste("Accuracy:", accuracy_km3))
print(paste("Precision:", mean(precision_km3)))
print(paste("Recall:", mean(recall_km3)))
print(paste("F1-score:", mean(f1_score_km3)))

# Compute Rand Index for K=2 clustering
rand_index_km2 <- randIndex(confuseTable.km2)
print("Rand Index for K=2 clustering:")
print(rand_index_km2)

# Compute Rand Index for K=3 clustering
rand_index_km3 <- randIndex(confuseTable.km3)
print("Rand Index for K=3 clustering:")
print(rand_index_km3)


## ----------------------------------------------------------------------------##









####################---QUESTION 02 ----------------------###############
###################--Hierarchical Partitioning Clustering------##########



#-------------- 1. Loading Libraries and data -------------------------#

# Load libraries
library(readxl)  # Loads the readxl library for reading Excel files
library(cluster) # Loads the cluster library for clustering functions
library(ggplot2)  # Loads the ggplot2 library for creating plots
library(flexclust)
library(dendextend)# Load dendextend library
library(corrplot) 

# Read data
df <- read_excel("DM_R_Answers/whitewine.xlsx")

#---------------------------------------------------------------
# Splitting features and target from x
x <- df[, 1:11]  # Extracting the first 11 columns as features
y <- df$quality  # Extracting the 12th column as the target variable

# --------------------------------------------------------

d_wine <- dist(x)

hclust_methods <- c("ward.D", "single", "complete", "average", "mcquitty","median", "centroid", "ward.D2")

wine_dendlist <- dendlist()
for(i in seq_along(hclust_methods)) {
  hc_wine <- hclust(d_wine, method = hclust_methods[i])
  wine_dendlist <- dendlist(wine_dendlist, as.dendrogram(hc_wine))
  # Plot the obtained dendrogram
  plot(hc_wine, cex = 0.6, hang = -1)
}

names(wine_dendlist) <- hclust_methods
wine_dendlist


wine_dendlist_cor <- cor.dendlist(wine_dendlist)
wine_dendlist_cor


corrplot::corrplot(wine_dendlist_cor, "pie", "lower")


#------------------------------------------------------------------------------