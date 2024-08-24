

########################################################################
#-------------- 1. Loading Libraries and data -------------------------#
########################################################################



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


