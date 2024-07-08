# Load required libraries (assuming they are already loaded)
library(stats)
library(dplyr)
library(tidyr)
library(ggplot2)  # Added for plotting

# Step 1: Load dataset (replace 'pizza_data.csv' with your actual dataset path)
df <- read.csv('pizza_data.csv')

# Step 2: Define the linear regression model
model <- formula(ranking ~ brand + price + weight + crust + cheese + size + toppings + spicy)
model_fit <- lm(model, data = df)

# Step 3: Print model summary
print(summary(model_fit))

# Step 4: Define conjoint attributes
conjoint_attributes <- c('brand', 'price', 'weight', 'crust', 'cheese', 'size', 'toppings', 'spicy')

# Step 5: Calculate part-worths and attribute importance
part_worth <- list()
part_worth_range <- c()
important_levels <- list()
end <- 1

for (item in conjoint_attributes) {
  nlevels <- length(unique(df[[item]]))
  level_name <- unique(df[[item]])
  
  begin <- end
  end <- begin + nlevels - 1
  
  new_part_worth <- coef(model_fit)[begin:end]
  new_part_worth <- c(new_part_worth, (-1) * sum(new_part_worth))
  
  important_levels[[item]] <- which.max(new_part_worth)
  part_worth[[item]] <- new_part_worth
  part_worth_range <- c(part_worth_range, max(new_part_worth) - min(new_part_worth))
}

# Print part-worths and attribute importance
print("Part-Worths:")
print(part_worth)
print("Attribute Importance:")
attribute_importance <- round(100 * (part_worth_range / sum(part_worth_range)), 2)
print(attribute_importance)

# Step 6: Calculate utility scores for each profile
part_worth_dict <- list()
attrib_level <- list()
for (item in seq_along(conjoint_attributes)) {
  part_worth_dict[[conjoint_attributes[item]]] <- setNames(part_worth[[item]], level_name[item])
  attrib_level[[conjoint_attributes[item]]] <- level_name[item]
}

df$utility <- rowSums(sapply(conjoint_attributes, function(item) part_worth_dict[[item]][df[[item]]]))

# Print the profile with maximum utility
print("Profile with highest utility score:")
print(df[which.max(df$utility), ])

# Determine preferred levels in each attribute
for (item in seq_along(conjoint_attributes)) {
  pref_index <- important_levels[[conjoint_attributes[item]]]
  pref_level <- names(attrib_level[[conjoint_attributes[item]]])[pref_index + 1]  # Adjust index due to R indexing starting from 1
  print(paste("Preferred level in", conjoint_attributes[item], "is:", pref_level))
}

# Plot the relative importance of attributes
importance_df <- data.frame(Attribute = conjoint_attributes, Importance = attribute_importance)
ggplot(importance_df, aes(x = Attribute, y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Relative Importance of Attributes", x = "Attributes", y = "Importance (%)") +
  theme_minimal()
