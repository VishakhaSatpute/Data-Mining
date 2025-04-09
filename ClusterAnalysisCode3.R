# Load necessary libraries
library(dplyr)
library(cluster)
library(factoextra)
library(ggplot2)
library(stats)
library(readr)

# Load the dataset
dataset <- read_csv("~/Data Mining/Project1/COVID-19_cases_plus_census.csv")

# Checking for missing values and imputing where necessary
numeric_columns <- sapply(dataset, is.numeric)
dataset[numeric_columns] <- lapply(dataset[numeric_columns], function(x) {
  ifelse(is.na(x), median(x, na.rm = TRUE), x)
})

getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

categorical_columns <- sapply(dataset, is.factor)
dataset[categorical_columns] <- lapply(dataset[categorical_columns], function(x) {
  ifelse(is.na(x), getMode(x), x)
})

# Normalizing the numeric data
dataset[numeric_columns] <- scale(dataset[numeric_columns])

# Select economic features for clustering
economic_data <- dataset %>%
  select(median_income, 
         income_per_capita,
         percent_income_spent_on_rent,
         median_rent,
         vacant_housing_units,
         housing_units,
         owner_occupied_housing_units,
         renter_occupied_housing_units_paying_cash_median_gross_rent,
         unemployed_pop,
         civilian_labor_force,
         employed_pop,
         median_year_structure_built)

# Perform K-means clustering
set.seed(123) # For reproducibility
kmeans_result <- kmeans(economic_data, centers = 3, nstart = 25)

# PCA for dimensionality reduction for visualization
pca_result <- prcomp(economic_data, scale. = TRUE)
pca_data <- data.frame(pca_result$x, Cluster = kmeans_result$cluster)

# K-means clustering plot with rectangles
km_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(Cluster))) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("#00AFBB", "#FC4E07", "#E7B800")) +
  geom_rect(data = pca_data %>% group_by(Cluster) %>% 
              summarise(xmin = min(PC1),
                        xmax = max(PC1),
                        ymin = min(PC2),
                        ymax = max(PC2)), 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            inherit.aes = FALSE, 
            fill = NA, 
            color = "black") +
  labs(title = "K-means Clusters with PCA") +
  xlab(paste("PC1 - ", round(var(pca_result$x[,1]) / sum(pca_result$sdev^2) * 100, 1), "% Variance")) +
  ylab(paste("PC2 - ", round(var(pca_result$x[,2]) / sum(pca_result$sdev^2) * 100, 1), "% Variance")) +
  theme_minimal()

# Print the k-means clustering plot
print(km_plot)
