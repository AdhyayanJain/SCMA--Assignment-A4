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
packages <- c("dplyr", "psych", "tidyr", "GPArotation", "FactoMineR", "factoextra", "pheatmap","factoextra")

# Call the function to install and load packages
install_and_load(packages)

# Load the survey data
survey_df <- read.csv('Survey.csv', header = TRUE)

# Display dimensions, column names, and structure of the dataset
cat("Dimensions of the dataset:\n")
print(dim(survey_df))

cat("\nColumn names in the dataset:\n")
print(names(survey_df))

cat("\nFirst few rows of the dataset:\n")
print(head(survey_df))

cat("\nStructure of the dataset:\n")
str(survey_df)

# Check for missing values
cat("\nChecking for missing values:\n")
print(sum(is.na(survey_df)))

# Select the relevant columns for PCA and Factor Analysis
sur_int <- survey_df[, 20:46]

cat("\nStructure of the selected data subset:\n")
str(sur_int)

cat("\nDimensions of the selected data subset:\n")
print(dim(sur_int))

# Perform Principal Component Analysis (PCA)
cat("\nPerforming Principal Component Analysis (PCA):\n")
pca <- principal(sur_int, 5, n.obs = 162, rotate = "promax")
print(pca)

# Perform Factor Analysis using the omega function
cat("\nPerforming Factor Analysis (omega):\n")
om.h <- omega(sur_int, n.obs = 162, sl = FALSE)
op <- par(mfrow = c(1, 1)) # Reset plotting parameters
om <- omega(sur_int, n.obs = 162)


# PCA using FactoMineR
cat("\nPCA using FactoMineR:\n")
pca_FactoMineR <- PCA(sur_int, scale.unit = TRUE)
summary(pca_FactoMineR)
fviz_pca_biplot(pca_FactoMineR)

# Show final structure and dimensions of the selected data subset
cat("\nFinal structure of the selected data subset:\n")
str(sur_int)

cat("\nFinal dimensions of the selected data subset:\n")
print(dim(sur_int))

cat("\nShowing the selected data subset:\n")
print(head(sur_int))

#Factor Analysis 

factor_analysis<-fa(sur_int,nfactors = 4,rotate = "varimax") 
names(factor_analysis) 
print(factor_analysis$loadings,reorder=TRUE) 
fa.diagram(factor_analysis) 
print(factor_analysis$communality) 
print(factor_analysis$scores) 
