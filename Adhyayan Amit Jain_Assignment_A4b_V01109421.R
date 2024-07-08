# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("cluster", "FactoMineR", "factoextra", "pheatmap")

# Install and load required packages
install_and_load(packages)

# Read the survey data
survey_df <- read.csv('Survey.csv', header = TRUE)

# Select relevant columns for analysis
sur_int <- survey_df[, 20:46]

# Print dimensions and structure of the selected data
cat("Dimensions of the selected data:\n")
print(dim(sur_int))

cat("\nStructure of the selected data:\n")
print(str(sur_int))

# Perform Cluster Analysis and Characterization
library(cluster)
library(factoextra)

# Show summary statistics of selected columns
cat("\nSummary statistics of selected columns:\n")
print(summary(sur_int))

# Determine optimal number of clusters using gap statistic
cat("\nDetermining optimal number of clusters using Gap Statistic:\n")
nbclust_out <- fviz_nbclust(sur_int, kmeans, method = "gap_stat")

# Print optimal number of clusters suggested by Gap Statistic
print(nbclust_out)

# Perform k-means clustering
set.seed(123)
km.res <- kmeans(sur_int, 4, nstart = 25)

# Visualize k-means clustering results
cat("\nVisualizing k-means clustering results:\n")
fviz_cluster(km.res, data = sur_int, palette = "jco", ggtheme = theme_minimal())

# Perform hierarchical clustering
res.hc <- hclust(dist(sur_int), method = "ward.D2")

# Visualize hierarchical clustering results as a dendrogram
cat("\nVisualizing hierarchical clustering results as a dendrogram:\n")
fviz_dend(res.hc, cex = 0.5, k = 4, palette = "jco")

# Heatmap visualization of the transposed data
library(pheatmap)
cat("\nGenerating heatmap visualization:\n")
pheatmap(t(sur_int), cutree_cols = 4)
