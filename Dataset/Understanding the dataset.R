###################################################################
#-------------Loading Libraries and data -------------------------#
###################################################################


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
