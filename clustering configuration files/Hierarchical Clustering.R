#-------------- 1. Loading Libraries and data -------------------------#

# Load libraries
library(readxl)  # Loads the readxl library for reading Excel files
library(cluster) # Loads the cluster library for clustering functions
library(ggplot2)  # Loads the ggplot2 library for creating plots
library(flexclust)
library(dendextend)# Load dendextend library


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


# Next, we can look at the cophenetic correlation between each clustering result using cor.dendlist. 
# (This can be nicely plotted using the corrplot function from the corrplot package):

  
wine_dendlist_cor <- cor.dendlist(wine_dendlist)
wine_dendlist_cor

#


library(corrplot)
corrplot::corrplot(wine_dendlist_cor, "pie", "lower")


wine_dendlist_cor_spearman <- cor.dendlist(wine_dendlist, method_coef = "spearman")
corrplot::corrplot(wine_dendlist_cor_spearman, "pie", "lower")


par(mfrow = c(4, 2), mar = c(2, 2, 3, 3))  # Adjust margin size as needed

for(i in 1:8) {
  wine_dendlist[[i]] %>% set("branches_k_color", k=2) %>% plot(axes = FALSE, horiz = TRUE)
  title(names(wine_dendlist)[i])
}



# It seems that the cophenetic correlation is very biased towards the influence of the main clusters. 
# Another correlation measure to use is the cor_common_nodes correlation (giving the proportion of nodes which share the exact same list of labels in both dendrograms). We can also check it out:
wine_dendlist_cor2 <- cor.dendlist(wine_dendlist, method = "common")
wine_dendlist_cor2

corrplot::corrplot(wine_dendlist_cor2, "pie", "lower")

# This gives us another perspective on our clustering algorithms. 
# We can see that most methods have around 75% common nodes with one another.
# Centroid and median seem relatively close to one another, as well as ward.D2 and ward.D to one another and to complete, average, and mcquitty (as compared to the other methods).