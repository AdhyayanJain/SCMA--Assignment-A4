# Load necessary libraries
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("ggplot2", "dplyr", "scales")

# Call the function to install and load packages
install_and_load(packages)

# Load the ice cream data
icecream_df <- read.csv('icecream.csv', header = TRUE)

# Display dimensions and column names of the dataset
cat("Dimensions of the dataset:\n")
print(dim(icecream_df))

cat("\nColumn names in the dataset:\n")
print(names(icecream_df))

# Display the first few rows of the dataset
cat("\nFirst few rows of the dataset:\n")
print(head(icecream_df))

# Display the structure of the dataset
cat("\nStructure of the dataset:\n")
str(icecream_df)

# Check for missing values
cat("\nChecking for missing values:\n")
print(sum(is.na(icecream_df)))

# Remove the 'Brand' column for analysis
ice <- icecream_df %>% select(-Brand)

# Display the structure and dimensions of the selected data subset
cat("\nStructure of the selected data subset:\n")
str(ice)

cat("\nDimensions of the selected data subset:\n")
print(dim(ice))

# Display the first few rows of the selected data subset
cat("\nFirst few rows of the selected data subset:\n")
print(head(ice))

# Calculate the distance matrix
distance_matrix <- dist(ice, method = "euclidean")
cat("\nDistance matrix calculated:\n")
print(distance_matrix)

# Apply Multidimensional Scaling (MDS)
mds_result <- cmdscale(distance_matrix, k = 2)
cat("\nMDS result:\n")
print(mds_result)

# Create a DataFrame for MDS results
mds_df <- as.data.frame(mds_result)
names(mds_df) <- c("Dimension1", "Dimension2")
mds_df$Brand <- icecream_df$Brand

# Display the first few rows of the MDS result DataFrame
cat("\nFirst few rows of the MDS result DataFrame:\n")
print(head(mds_df))

# Plot MDS results
ggplot(mds_df, aes(x = Dimension1, y = Dimension2, color = Brand, label = Brand)) +
  geom_point(size = 3) +
  geom_text(vjust = 1.5, hjust = 0.5) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "MDS Plot for Ice Cream Data", x = "Dimension 1", y = "Dimension 2") +
  theme(legend.title = element_blank(), legend.position = "right", plot.title = element_text(hjust = 0.5))

# Interpretation
cat("\nInterpretation of MDS Results:\n")
cat("The MDS plot shows the relative positions of the different ice cream brands based on the provided variables.\n")
cat("Points that are closer together represent brands that are more similar to each other in terms of the variables used.\n")
cat("The axes (Dimension 1 and Dimension 2) do not have intrinsic meanings but are used to visualize the distances between the points.\n")
