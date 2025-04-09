# Load necessary libraries
library(dplyr)
library(readr)
library(tidyr)

# Loading the dataset
dataset <- read_csv("C:/Users/juhic/Juhi/SMU/Semester_2/Data_Mining/Projects/Datasets/COVID-19_cases_plus_census.csv")


# Checking for missing values
print("Missing values per column before imputation:")
print(sapply(dataset, function(x) sum(is.na(x))))

# Imputing missing values for numeric columns with the median
numeric_columns <- sapply(dataset, is.numeric)
dataset[numeric_columns] <- lapply(dataset[numeric_columns], function(x) {
  ifelse(is.na(x), median(x, na.rm = TRUE), x)
})

# Impute missing values in non-numeric columns with the mode
getMode <- function(x) {
  tab <- table(x)
  val <- as.numeric(names(which.max(tab)))
  ifelse(is.na(val), as.character(x[1]), as.character(val)) # Handle NA mode
}

non_numeric_columns <- names(dataset)[!numeric_columns]
for (col in non_numeric_columns) {
  dataset[[col]] <- ifelse(is.na(dataset[[col]]), getMode(dataset[[col]]), dataset[[col]])
}

# Checking for and removing duplicate rows
dataset <- distinct(dataset)

# Normalizing numeric data - Min-Max Normalization
normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

dataset[numeric_columns] <- lapply(dataset[numeric_columns], normalize)

# Statistical summary for specified features
important_features <- c("confirmed_cases", "deaths", "median_income", "income_per_capita",
                        "median_age", "total_pop", "male_pop", "female_pop", "white_pop",
                        "black_pop", "asian_pop", "hispanic_pop", "percent_income_spent_on_rent",
                        "vacant_housing_units", "housing_units", "median_rent", 
                        "owner_occupied_housing_units", "renter_occupied_housing_units_paying_cash_median_gross_rent",
                        "families_with_young_children", "unemployed_pop", "civilian_labor_force", 
                        "employed_pop", "not_in_labor_force",
                        "commuters_by_public_transportation", "median_year_structure_built", "households",
                        "pop_15_and_over", "pop_5_years_over", "pop_25_years_over", "population_3_years_over",
                        "pop_25_64")

# Load necessary libraries
library(ggplot2)

# Loop through each feature and create a boxplot
library(ggplot2)

library(tidyr)
library(dplyr)


# Ensure your dataset is named 'dataset' and contains columns listed in 'important_features'
# Convert the dataset to long format
long_dataset <- dataset %>% 
  pivot_longer(cols = all_of(important_features), names_to = "Feature", values_to = "Value")

# Generate the boxplot
ggplot(long_dataset, aes(x = Feature, y = Value)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Boxplots of Important Features", x = "Feature", y = "Value")


library(dplyr)

# Start by creating an empty list to hold the indices of rows without outliers
rows_without_outliers <- list()

# Identify and remove outliers for each feature
for (feature in important_features) {
  Q1 <- quantile(dataset[[feature]], 0.25, na.rm = TRUE)
  Q3 <- quantile(dataset[[feature]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  # Calculate the bounds for outliers
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Get the indices of rows without outliers
  non_outlier_rows <- which(dataset[[feature]] >= lower_bound & dataset[[feature]] <= upper_bound)
  
  # Store the indices for each feature
  rows_without_outliers[[feature]] <- non_outlier_rows
}

# Assuming we want to keep a row if it's not an outlier in ANY of the features,
# we can take the intersection of all non-outlier indices
all_non_outlier_rows <- Reduce(intersect, rows_without_outliers)

# Subset the original dataset to only include rows that are not outliers in any feature
dataset_without_outliers <- dataset[all_non_outlier_rows, ]

# Reshape the cleaned dataset to long format
long_dataset_without_outliers <- pivot_longer(
  dataset_without_outliers,
  cols = all_of(important_features),
  names_to = "Feature",
  values_to = "Value"
)

# Plot the boxplots for all features without outliers
boxplot_graph_without_outliers <- ggplot(long_dataset_without_outliers, aes(x = Feature, y = Value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Boxplots of Important Features Without Outliers", x = "Feature", y = "Value")

# Print the plot
print(boxplot_graph_without_outliers)

# Subset the original dataset to only include non-outlier data for important features
cleaned_important_features <- dataset_without_outliers[important_features]

# Optionally, if you want to reassign this to the same variable name 'important_features' for consistency
important_features <- names(cleaned_important_features)

summary_select <- summary(dataset[important_features])
print(summary_select)

# Load necessary libraries
library(dplyr)

# Assuming 'dataset' is your DataFrame and it's already loaded and cleaned

# Select only the numeric features relevant for clustering
data_for_clustering <- select(dataset, confirmed_cases, deaths, median_income, income_per_capita,
                              median_age, total_pop, male_pop, female_pop, white_pop,
                              black_pop, asian_pop, hispanic_pop, percent_income_spent_on_rent,
                              vacant_housing_units, housing_units, median_rent, 
                              owner_occupied_housing_units, renter_occupied_housing_units_paying_cash_median_gross_rent,
                              families_with_young_children, unemployed_pop, civilian_labor_force, 
                              employed_pop, not_in_labor_force, commuters_by_public_transportation, 
                              median_year_structure_built, households, pop_15_and_over, 
                              pop_5_years_over, pop_25_years_over, population_3_years_over, pop_25_64)

# Calculate Euclidean distance
distance_matrix <- dist(data_for_clustering, method = "euclidean")

# Perform hierarchical clustering using Ward's method
hc <- hclust(distance_matrix, method = "ward.D2")



# Determine the optimal number of clusters (optional, for dynamic tree cut)
# This requires the dynamicTreeCut library, uncomment the next two lines if you wish to use it
# install.packages("dynamicTreeCut")
# library(dynamicTreeCut)
# dynamic_clusters <- cutreeDynamic(dendro = hc, distM = as.dist(distance_matrix), deepSplit = TRUE)

# Plotting the dendrogram with enhanced visualization using factoextra
# Here 'k' is the number of clusters you wish to highlight; you can choose it based on your analysis or set dynamically
distance_matrix <- dist(data_for_clustering, method = "euclidean")

# Perform hierarchical clustering
hc <- hclust(distance_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hc, label = FALSE)  

# Load necessary libraries
# Assuming 'dataset' is your full DataFrame
# Selecting only the features for Subset 1
# Load necessary libraries
library(dplyr)
library(cluster)   # For clustering algorithms
library(factoextra) # For cluster visualization and PCA

# Assuming 'dataset' is your DataFrame, and it has been appropriately loaded
# Selecting only the features for Subset 1
data_for_clustering <- dataset %>%
  select(confirmed_cases, deaths, median_age, total_pop, male_pop, female_pop, 
         white_pop, black_pop, asian_pop, hispanic_pop, households, 
         pop_15_and_over, pop_5_years_over, pop_25_years_over, 
         population_3_years_over, pop_25_64)

# Calculate variance while safely handling potential NA values
variance_info <- sapply(data_for_clustering, function(x) var(x, na.rm = TRUE))
non_zero_variance <- !is.na(variance_info) & variance_info > 0

# Subsetting columns with non-zero variance
data_for_clustering <- data_for_clustering[, non_zero_variance, drop = FALSE]

# Scale the data
data_for_clustering_scaled <- scale(data_for_clustering)
data_for_clustering_scaled <- na.omit(data_for_clustering_scaled) # Remove any rows with NA after scaling

# Perform K-means clustering
set.seed(123) # Ensure reproducibility
kmeans_result <- kmeans(data_for_clustering_scaled, centers = 3, nstart = 25)

# Perform hierarchical clustering
distance_matrix <- dist(data_for_clustering_scaled, method = "euclidean")
hc <- hclust(distance_matrix, method = "ward.D2")

# Cutting the dendrogram to form clusters based on visual inspection or other criteria
clusters_hc <- cutree(hc, k = 3)


# Scale the data
data_for_clustering_scaled <- scale(data_for_clustering)
data_for_clustering_scaled <- na.omit(data_for_clustering_scaled) # Remove any rows with NA after scaling

# Create a data frame from the scaled data
data_for_clustering_scaled_df <- as.data.frame(data_for_clustering_scaled)

# Adding cluster assignments to the data frame
data_for_clustering_scaled_df$cluster_km <- kmeans_result$cluster
data_for_clustering_scaled_df$cluster_hc <- clusters_hc

# Convert cluster assignments to factors for visualization
data_for_clustering_scaled_df$cluster_km <- as.factor(data_for_clustering_scaled_df$cluster_km)
data_for_clustering_scaled_df$cluster_hc <- as.factor(data_for_clustering_scaled_df$cluster_hc)


# Perform PCA for visualization
pca_res_km <- prcomp(data_for_clustering_scaled_df[, -c(ncol(data_for_clustering_scaled_df)-1, ncol(data_for_clustering_scaled_df))], scale. = TRUE)

# Visualizing K-means Clusters with PCA
# Ensure cluster labels are treated as discrete categories
library(ggplot2)

# Adjust the limits of the x-axis to focus on the main data cluster
# Prepare the data for PCA visualization
# Extract PCA scores
library(ggplot2)
library(dplyr)

# Extract PCA scores and variance percentages
pca_scores <- pca_res_km$x
pca_var_perc <- round((pca_res_km$sdev^2 / sum(pca_res_km$sdev^2))[1:2] * 100, 2)

# Create a data frame for plotting
plotting_data <- data.frame(PC1 = pca_scores[, 1], PC2 = pca_scores[, 2], cluster_km = data_for_clustering_scaled_df$cluster_km)

# Calculate the boundaries for rectangles for each cluster
rect_data <- plotting_data %>%
  group_by(cluster_km) %>%
  summarize(xmin = min(PC1), xmax = max(PC1), ymin = min(PC2), ymax = max(PC2), .groups = 'drop')

# Remove rectangles for clusters with less than two points
rect_data <- rect_data %>%
  filter(xmax != xmin & ymax != ymin)

# Create the PCA plot
pca_plot <- ggplot() +
  geom_rect(data = rect_data,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = as.factor(cluster_km)),
            alpha = 0.2, color = "black") +
  geom_point(data = plotting_data,
             aes(x = PC1, y = PC2, color = as.factor(cluster_km), shape = as.factor(cluster_km)),
             alpha = 0.6, size = 3) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_fill_manual(values = c("#00AFBB33", "#E7B80033", "#FC4E0733")) +
  scale_shape_manual(values = c(16, 17, 18)) + # Use different shapes
  theme_minimal(base_size = 14) + # Use a minimal theme with larger base font size
  labs(title = " K-means Clusters",
       x = paste("Dim1 (", pca_var_perc[1], "%)", sep = ""),
       y = paste("Dim2 (", pca_var_perc[2], "%)", sep = "")) +
  theme(legend.position = "right", # Adjust legend position
        legend.title = element_blank(), # Remove the legend title
        plot.title = element_text(hjust = 0.5, size = 20), # Center and increase the plot title size
        axis.text = element_text(size = 14), # Increase axis text size
        axis.title = element_text(size = 16)) # Increase axis title size

# Print the plot
print(pca_plot)




# PCA for Subset 2 : Juhi: Start
library(tidyverse)
library(cluster)
#,confirmed_cases,deaths
data_for_clustering_subset_2 <- dataset %>%
  select(median_income
         ,income_per_capita
         ,percent_income_spent_on_rent
         ,vacant_housing_units
         ,housing_units
         ,median_rent
         ,renter_occupied_housing_units_paying_cash_median_gross_rent
         ,unemployed_pop
         ,civilian_labor_force
         ,employed_pop
         ,not_in_labor_force
         ,commuters_by_public_transportation
         ,median_year_structure_built)

variance_info_subset_2 <- sapply(data_for_clustering_subset_2, function(x) var(x, na.rm = TRUE))
non_zero_variance_subset_2 <- !is.na(variance_info_subset_2) & variance_info_subset_2 > 0
data_for_clustering_subset_2 <- data_for_clustering_subset_2[, non_zero_variance_subset_2, drop = FALSE]

data_for_clustering_scaled_subset_2 <- scale(data_for_clustering_subset_2)
data_for_clustering_scaled_subset_2 <- na.omit(data_for_clustering_scaled_subset_2)

data_for_clustering_scaled_df_subset_2 <- as.data.frame(data_for_clustering_scaled_subset_2)

set.seed(123)
kmeans_result_subset_2 <- kmeans(data_for_clustering_scaled_subset_2, centers = 3, nstart = 25)
data_for_clustering_scaled_df_subset_2$cluster_km <- as.factor(kmeans_result_subset_2$cluster)

distance_matrix_subset_2 <- dist(data_for_clustering_scaled_subset_2, method = "euclidean")
hc_subset_2 <- hclust(distance_matrix_subset_2, method = "ward.D2")
clusters_hc_subset_2 <- cutree(hc_subset_2, k = 3)
data_for_clustering_scaled_df_subset_2$clusters_hc_subset_2 <- as.factor(clusters_hc_subset_2)

data_for_clustering_scaled_df_subset_2$cluster_km <- as.factor(data_for_clustering_scaled_df_subset_2$cluster_km)
data_for_clustering_scaled_df_subset_2$cluster_hc <- as.factor(data_for_clustering_scaled_df_subset_2$clusters_hc_subset_2)

numeric_data <- data_for_clustering_scaled_df_subset_2 %>%
  select_if(is.numeric)
pca_res_km_subset_2 <- prcomp(numeric_data, scale. = TRUE)

library(ggplot2)

pca_scores_subset_2 <- pca_res_km_subset_2$x
pca_var_perc_subset_2 <- round((pca_res_km_subset_2$sdev^2 / sum(pca_res_km_subset_2$sdev^2))[1:2] * 100, 2)

plotting_data_subset_2 <- data.frame(PC1 = pca_scores_subset_2[, 1], PC2 = pca_scores_subset_2[, 2], cluster_km = data_for_clustering_scaled_df_subset_2$cluster_km)

rect_data_subset_2 <- plotting_data_subset_2 %>%
  group_by(cluster_km) %>%
  summarize(xmin = min(PC1), xmax = max(PC1), ymin = min(PC2), ymax = max(PC2), .groups = 'drop') %>%
  filter(xmax != xmin & ymax != ymin)

pca_plot_subset_2 <- ggplot() +
  geom_rect(data = rect_data_subset_2,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = as.factor(cluster_km)),
            alpha = 0.2, color = "black") +
  geom_point(data = plotting_data_subset_2,
             aes(x = PC1, y = PC2, color = as.factor(cluster_km), shape = as.factor(cluster_km)),
             alpha = 0.6, size = 3) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_fill_manual(values = c("#00AFBB33", "#E7B80033", "#FC4E0733")) +
  scale_shape_manual(values = c(16, 17, 18)) +
  theme_minimal(base_size = 14) +
  labs(title = "K-means Clusters for Subset 2 with 3 Clusters",
       x = paste("PC1 (", pca_var_perc_subset_2[1], "%)", sep = ""),
       y = paste("PC2 (", pca_var_perc_subset_2[2], "%)", sep = "")) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

print(pca_plot_subset_2)



# Visualization with 4,5 clusters for subset 2: Juhi: Start

# Visualization for 4 clusters
set.seed(123)
kmeans_result_subset_2_4 <- kmeans(data_for_clustering_scaled_subset_2, centers = 4, nstart = 25)
data_for_clustering_scaled_df_subset_2$cluster_km_4 <- as.factor(kmeans_result_subset_2_4$cluster)

distance_matrix_subset_2_4 <- dist(data_for_clustering_scaled_subset_2, method = "euclidean")
hc_subset_2_4 <- hclust(distance_matrix_subset_2_4, method = "ward.D2")
clusters_hc_subset_2_4 <- cutree(hc_subset_2_4, k = 4)
data_for_clustering_scaled_df_subset_2$clusters_hc_subset_2_4 <- as.factor(clusters_hc_subset_2_4)

data_for_clustering_scaled_df_subset_2$cluster_km_4 <- as.factor(data_for_clustering_scaled_df_subset_2$cluster_km_4)
data_for_clustering_scaled_df_subset_2$cluster_hc <- as.factor(data_for_clustering_scaled_df_subset_2$clusters_hc_subset_2_4)

numeric_data <- data_for_clustering_scaled_df_subset_2 %>%
  select_if(is.numeric)
pca_res_km_subset_2 <- prcomp(numeric_data, scale. = TRUE)

library(ggplot2)

pca_scores_subset_2 <- pca_res_km_subset_2$x
pca_var_perc_subset_2 <- round((pca_res_km_subset_2$sdev^2 / sum(pca_res_km_subset_2$sdev^2))[1:2] * 100, 2)

plotting_data_subset_2_4 <- data.frame(PC1 = pca_scores_subset_2[, 1], PC2 = pca_scores_subset_2[, 2], 
                                       cluster_km = data_for_clustering_scaled_df_subset_2$cluster_km_4)

rect_data_subset_2_4 <- plotting_data_subset_2_4 %>%
  group_by(cluster_km) %>%
  summarize(xmin = min(PC1), xmax = max(PC1), ymin = min(PC2), ymax = max(PC2), .groups = 'drop') %>%
  filter(xmax != xmin & ymax != ymin)

pca_plot_subset_2_4 <- ggplot() +
  geom_rect(data = rect_data_subset_2_4,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = as.factor(cluster_km)),
            alpha = 0.2, color = "black") +
  geom_point(data = plotting_data_subset_2_4,
             aes(x = PC1, y = PC2, color = as.factor(cluster_km), shape = as.factor(cluster_km)),
             alpha = 0.6, size = 3) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#6F42C1")) +
  scale_fill_manual(values = c("#00AFBB33", "#E7B80033", "#FC4E0733", "#6F42C133")) +
  scale_shape_manual(values = c(16, 17, 18, 19)) +
  theme_minimal(base_size = 14) +
  labs(title = "K-means Clusters for Subset 2 with 4 Clusters",
       x = paste("PC1 (", pca_var_perc_subset_2[1], "%)", sep = ""),
       y = paste("PC2 (", pca_var_perc_subset_2[2], "%)", sep = "")) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

print(pca_plot_subset_2_4)



# Visualization for 5 clusters
set.seed(123)
kmeans_result_subset_2_5 <- kmeans(data_for_clustering_scaled_subset_2, centers = 5, nstart = 25)
data_for_clustering_scaled_df_subset_2$cluster_km_5 <- as.factor(kmeans_result_subset_2_5$cluster)

distance_matrix_subset_2_5 <- dist(data_for_clustering_scaled_subset_2, method = "euclidean")
hc_subset_2_5 <- hclust(distance_matrix_subset_2_5, method = "ward.D2")
clusters_hc_subset_2_5 <- cutree(hc_subset_2_5, k = 5)
data_for_clustering_scaled_df_subset_2$clusters_hc_subset_2_5 <- as.factor(clusters_hc_subset_2_5)

data_for_clustering_scaled_df_subset_2$cluster_km_5 <- as.factor(data_for_clustering_scaled_df_subset_2$cluster_km_5)
data_for_clustering_scaled_df_subset_2$cluster_hc <- as.factor(data_for_clustering_scaled_df_subset_2$clusters_hc_subset_2_5)

numeric_data <- data_for_clustering_scaled_df_subset_2 %>%
  select_if(is.numeric)
pca_res_km_subset_2 <- prcomp(numeric_data, scale. = TRUE)

library(ggplot2)

pca_scores_subset_2 <- pca_res_km_subset_2$x
pca_var_perc_subset_2 <- round((pca_res_km_subset_2$sdev^2 / sum(pca_res_km_subset_2$sdev^2))[1:2] * 100, 2)

plotting_data_subset_2_5 <- data.frame(PC1 = pca_scores_subset_2[, 1], PC2 = pca_scores_subset_2[, 2], 
                                       cluster_km = data_for_clustering_scaled_df_subset_2$cluster_km_5)

rect_data_subset_2_5 <- plotting_data_subset_2_5 %>%
  group_by(cluster_km) %>%
  summarize(xmin = min(PC1), xmax = max(PC1), ymin = min(PC2), ymax = max(PC2), .groups = 'drop') %>%
  filter(xmax != xmin & ymax != ymin)

pca_plot_subset_2_5 <- ggplot() +
  geom_rect(data = rect_data_subset_2_5,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = as.factor(cluster_km)),
            alpha = 0.2, color = "black") +
  geom_point(data = plotting_data_subset_2_5,
             aes(x = PC1, y = PC2, color = as.factor(cluster_km), shape = as.factor(cluster_km)),
             alpha = 0.6, size = 3) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07", "#6F42C1", "#D6336C")) +
  scale_fill_manual(values = c("#00AFBB33", "#E7B80033", "#FC4E0733", "#6F42C133", "#D6336C33")) +
  scale_shape_manual(values = c(16, 17, 18, 19, 20)) +
  theme_minimal(base_size = 14) +
  labs(title = "K-means Clusters for Subset 2 with 5 Clusters",
       x = paste("PC1 (", pca_var_perc_subset_2[1], "%)", sep = ""),
       y = paste("PC2 (", pca_var_perc_subset_2[2], "%)", sep = "")) +
  theme(legend.position = "right",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

print(pca_plot_subset_2_5)


# Visualization with 3,4,5 clusters for subset 2: Juhi: End



# After performing K-means clustering
kmeans_result_subset_2 <- kmeans(data_for_clustering_scaled_subset_2, centers = 3, nstart = 25)

# To extract the centroids
centroids <- kmeans_result_subset_2$centers

# Display the centroids
print(centroids)





# Display centroid: Juhi: Start

# Assuming you have your PCA result in pca_res_km_subset_2 and your centroids in centroids
centroid_pca <- predict(pca_res_km_subset_2, newdata = centroids)
# Convert to a data frame
centroid_pca_df <- as.data.frame(centroid_pca)
# Add a cluster label for the plot
centroid_pca_df$cluster <- factor(1:nrow(centroid_pca_df))
# Continuing from your existing ggplot PCA plot called pca_plot_subset_2
pca_plot_subset_2 <- pca_plot_subset_2 + 
  geom_point(data = centroid_pca_df, aes(x = PC1, y = PC2, color = cluster), size = 4, shape = 4) +
  geom_label(data = centroid_pca_df, aes(x = PC1, y = PC2, label = cluster), nudge_y = 0.5)
# Print the plot with centroids
print(pca_plot_subset_2)

# Display centroid: Juhi: End






# PCA for subset 2 : Juhi: End


# 3D plot for subset 2:  Juhi: Start

# Assuming pca_res_km is your PCA result object and has at least three dimensions
# Create a data frame for plotting
plotting_data_3d_subset_2 <- data.frame(
  PC1_subset_2 = pca_res_km_subset_2$x[, 1],
  PC2_subset_2 = pca_res_km_subset_2$x[, 2],
  PC3_subset_2 = pca_res_km_subset_2$x[, 3],  # Include third principal component
  cluster_km_subset_2 = as.factor(data_for_clustering_scaled_df_subset_2$cluster_km)
)

# Create a 3D scatter plot
plot_3d_subset_2 <- plot_ly(data = plotting_data_3d_subset_2, x = ~PC1_subset_2, 
                            y = ~PC2_subset_2, 
                            z = ~PC3_subset_2, 
                            color = ~cluster_km_subset_2, 
                            colors = c("#00AFBB", "#E7B800", "#FC4E07"), 
                            type = "scatter3d", mode = "markers", 
                            marker = list(size = 5, opacity = 0.6)) %>%
  layout(title = "3D PCA of K-means Clusters for Subset 2",
         scene = list(
           xaxis = list(title = "PC1"),
           yaxis = list(title = "PC2"),
           zaxis = list(title = "PC3")
         ))


# Render the plot
plot_3d_subset_2

# 3D plot for subset 2:  Juhi: End


# Assuming 'dataset' is your DataFrame, and it has been appropriately loaded

# Selecting only the features for Subset 1
# Assuming you have already installed the reshape2 package
# Assuming you have already installed the reshape2 package
library(ggplot2)
library(reshape2)
library(dplyr)

# Ensure 'data_for_clustering' contains your data and no constant columns
data_for_clustering <- data_for_clustering %>% 
  select_if(~ var(.) != 0)

# Perform PCA on the cleaned data
pca_result <- prcomp(data_for_clustering, center = TRUE, scale. = TRUE)

# Create a data frame of loadings for the first principal component only
loadings <- as.data.frame(pca_result$rotation[, 1])
loadings$feature <- rownames(loadings)
loadings$value <- loadings[, 1]  # Use the first PC's loadings
loadings$abs_value <- abs(loadings$value)  # Absolute value for ordering

# Order features by the absolute value of their loadings
loadings <- loadings %>%
  arrange(desc(abs_value)) %>%
  mutate(feature = factor(feature, levels = rev(feature)))  # Factor with levels ordered by magnitude

# Create the lollipop chart
lollipop_chart <- ggplot(loadings, aes(x = feature, y = value, fill = feature)) +
  geom_segment(aes(x = feature, xend = feature, y = 0, yend = value), color = "grey") +
  geom_point(size = 3) +
  coord_flip() +  # Flip coordinates to make it horizontal
  scale_fill_viridis_d() +  # Use a discrete viridis color scale
  theme_minimal() +
  labs(title = "Contribution of Features to PC1",
       x = "",
       y = "Loading Value")

# Print the lollipop chart
print(lollipop_chart)



# Feature contribution for subset 2: Juhi: Start
cluster_means_subset_2 <- data_for_clustering_scaled_df_subset_2 %>%
  group_by(cluster_km) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  pivot_longer(cols = -cluster_km, names_to = "feature", values_to = "mean")


# Create a lollipop chart for each cluster--- NEED TO RECHECK FOR THE COLUMS - UNABLE TO UNDERSTAND WHICH ARE THIS COLUMNS
lollipop_chart_subset_2 <- ggplot(cluster_means_subset_2, aes(x = reorder(feature, mean), y = mean, fill = as.factor(cluster_km))) +
  geom_segment(aes(x = feature, xend = feature, y = 0, yend = mean), color = "grey") +
  geom_point(size = 3) +
  coord_flip() + # Make the chart horizontal
  facet_wrap(~cluster_km, scales = 'free') + # Create separate plots for each cluster
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) + # Use manual colors
  theme_minimal() +
  labs(title = "Feature Contribution to Clusters: Subset 2",
       x = "",
       y = "Mean Value",
       fill = "Cluster") +
  theme(legend.position = "bottom", 
        axis.title = element_text(size = 14), # Increase size of axis titles
        axis.text = element_text(size = 10), # Increase size of axis text
        legend.text = element_text(size = 10)) # Increase size of legend text

# Print the lollipop chart
print(lollipop_chart_subset_2)

# Feature contribution after PCA for subset 2: Juhi: End

library(ggplot2)
library(dplyr)
library(tidyr)

# Assuming 'data_for_clustering_scaled_df' contains your scaled data and cluster assignments

# Check the names of the columns to confirm the cluster assignment column
print(names(data_for_clustering_scaled_df))

# Assuming 'cluster_km' is the correct column name for cluster assignments
# Calculate the mean of each numeric feature within each cluster
cluster_means <- data_for_clustering_scaled_df %>%
  group_by(cluster_km) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  pivot_longer(cols = -cluster_km, names_to = "feature", values_to = "mean")

# If the above summarise() line results in error, replace it with:
# summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

# Create a lollipop chart for each cluster
lollipop_chart <- ggplot(cluster_means, aes(x = reorder(feature, mean), y = mean, fill = as.factor(cluster_km))) +
  geom_segment(aes(x = feature, xend = feature, y = 0, yend = mean), color = "grey") +
  geom_point(size = 3) +
  coord_flip() + # Make the chart horizontal
  facet_wrap(~cluster_km, scales = 'free') + # Create separate plots for each cluster
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) + # Use manual colors
  theme_minimal() +
  labs(title = "Feature Contribution to Clusters",
       x = "",
       y = "Mean Value",
       fill = "Cluster") +
  theme(legend.position = "bottom") # Adjust legend position

# Print the lollipop chart
print(lollipop_chart)

# Load necessary libraries
# Load necessary libraries
# Correctly prepare 'cases_TX' with appropriate cluster assignments
cases = read_csv("C:/Users/juhic/Juhi/SMU/Semester_2/Data_Mining/Projects/Datasets/COVID-19_cases_plus_census.csv")
cases_CA <-  cases %>% filter(state == "CA")
dim(cases_CA)
#counties <- as_tibble(map_data("county"))
#counties_TX <- counties %>% dplyr::filter(region == "texas") %>% 
#  rename(c(county = subregion))
#cases_TX_scaled <- cases_TX %>% 
#   select(confirmed_cases, deaths, median_age, total_pop, male_pop, female_pop, 
#          white_pop, black_pop, asian_pop, hispanic_pop, households, 
#          pop_15_and_over, pop_5_years_over, pop_25_years_over, 
#          population_3_years_over, pop_25_64) %>% 
#   scale() %>% as_tibble()
# 
# variance_info1 <- sapply(cases_TX_scaled, function(x) var(x, na.rm = TRUE))
# non_zero_variance1 <- !is.na(variance_info1) & variance_info1 > 0
# 
# # Subsetting columns with non-zero variance
# cases_TX_scaled <- cases_TX_scaled[, non_zero_variance1, drop = FALSE]
# 
# # Scale the data
# cases_TX_scaled_1 <- scale(cases_TX_scaled)
# cases_TX_scaled_1 <- na.omit(cases_TX_scaled_1)
# km <- kmeans(cases_TX_scaled, centers = 3)
# km
# ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"), 
#                     cols = colnames(km$centers)), 
#        aes(y = name, x = value)) +
#   geom_bar(stat = "identity") +
#   facet_grid(rows = vars(cluster))

# counties <- as_tibble(map_data("county"))
# counties_CA <- counties %>% dplyr::filter(region == "california") %>% 
#   rename(c(county = subregion))
# 
# cases_CA <- cases_CA %>% mutate(county = county_name %>% 
#                                   str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
# 
# counties_CA_clust <- counties_CA %>% left_join(cases_CA %>% add_column(cluster = factor(km_$cluster)))
# 
# ggplot(counties_CA_clust, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = cluster)) +
#   coord_quickmap() + 
#   scale_fill_viridis_d() + 
#   labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")
# 
# library(factoextra)
# library(cluster)
# library(ggplot2)

# Geo map for California for subset 2 cluster distribution: Juhi: Start

counties <- as_tibble(map_data("county"))
counties_CA <- counties %>% dplyr::filter(region == "california") %>% 
  rename(c(county = subregion))

cases_CA_scaled_subset_2 <- cases_CA %>%
  select(median_income
         ,income_per_capita
         ,percent_income_spent_on_rent
         ,vacant_housing_units
         ,housing_units
         ,median_rent
         ,renter_occupied_housing_units_paying_cash_median_gross_rent
         ,unemployed_pop
         ,civilian_labor_force
         ,employed_pop
         ,not_in_labor_force
         ,commuters_by_public_transportation
         ,median_year_structure_built) %>%
  scale() %>% as_tibble()

variance_info1_subset_2 <- sapply(cases_CA_scaled_subset_2, function(x) var(x, na.rm = TRUE))
non_zero_variance1_subset_2 <- !is.na(variance_info1_subset_2) & variance_info1_subset_2 > 0


# Subsetting columns with non-zero variance
cases_CA_scaled_subset_2 <- cases_CA_scaled_subset_2[, non_zero_variance1_subset_2, drop = FALSE]

# Scale the data
cases_CA_scaled_1_subset_2 <- scale(cases_CA_scaled_subset_2)
cases_CA_scaled_1_subset_2 <- na.omit(cases_CA_scaled_1_subset_2)

km_map_subset_2 <- kmeans(cases_CA_scaled_1_subset_2, centers = 3)
km_map_subset_2

ggplot(pivot_longer(as_tibble(km_map_subset_2$centers,  rownames = "cluster"),
                    cols = colnames(km_map_subset_2$centers)),
       aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))



cases_CA <- cases_CA %>% mutate(county = county_name %>% 
                                  str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

counties_CA_clust_subset_2 <- counties_CA %>% left_join(cases_CA %>% add_column(cluster = factor(km_map_subset_2$cluster)))

bar_plot <- ggplot(pivot_longer(as_tibble(km_map_subset_2$centers, rownames = "cluster"),
                                cols = colnames(km_map_subset_2$centers)),
                   aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))
print(bar_plot)

map_plot <- ggplot(counties_CA_clust_subset_2, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")
print(map_plot)

# Geo map for California for subset 2 cluster distribution: Juhi: End

# We'll first calculate the total within-cluster sum of squares for different numbers of clusters
# to use in the elbow method
set.seed(123)  # for reproducibility

wss <- (nrow(data_for_clustering_scaled) - 1) * sum(apply(data_for_clustering_scaled, 2, var))
for (i in 2:15) {
  set.seed(123)
  wss[i] <- sum(kmeans(data_for_clustering_scaled, centers = i, nstart = 25)$withinss)
}

# Plotting the elbow curve
elbow_curve <- data.frame(Clusters = 1:15, WSS = wss)
ggplot(elbow_curve, aes(x = Clusters, y = WSS)) +
  geom_line() +
  scale_x_continuous(breaks = 1:15) +
  theme_minimal() +
  ggtitle("Elbow Method for Determining Optimal Number of Clusters")

# Calculating the average silhouette width for different numbers of clusters
sil_width <- c(NA)
for (i in 2:15) {
  set.seed(123)
  sil_width[i] <- mean(silhouette(kmeans(data_for_clustering_scaled, centers = i, nstart = 25)$cluster, dist(data_for_clustering_scaled))[, 3])
}

# Plotting the silhouette width
silhouette_curve <- data.frame(Clusters = 2:15, Silhouette = sil_width[-1])
ggplot(silhouette_curve, aes(x = Clusters, y = Silhouette)) +
  geom_line() +
  scale_x_continuous(breaks = 2:15) +
  theme_minimal() +
  ggtitle("Silhouette Method for Determining Optimal Number of Clusters")

# Now let's print out both plots
print(elbow_curve)
print(silhouette_curve)

install.packages(c("fpc", "mclust"))
library(fpc)
library(mclust)





# Elbow and Silhoutte method for subset 2: Juhi: Start
set.seed(123)  # for reproducibility

wss_subset_2 <- (nrow(data_for_clustering_scaled_subset_2) - 1) * sum(apply(data_for_clustering_scaled_subset_2, 2, var))
for (i in 2:15) {
  set.seed(123)
  wss_subset_2[i] <- sum(kmeans(data_for_clustering_scaled_subset_2, centers = i, nstart = 25)$withinss)
}

elbow_curve_subset_2 <- data.frame(Clusters = 1:15, WSS = wss_subset_2)
ggplot(elbow_curve_subset_2, aes(x = Clusters, y = WSS)) +
  geom_line() +
  scale_x_continuous(breaks = 1:15) +
  theme_minimal() +
  ggtitle("Elbow Method for Determining Optimal Number of Clusters for Subset 2")


sil_width_subset_2 <- c(NA)
for (i in 2:15) {
  set.seed(123)
  sil_width_subset_2[i] <- mean(silhouette(kmeans(data_for_clustering_scaled_subset_2, centers = i, nstart = 50, 
                                                  iter.max = 50)$cluster, dist(data_for_clustering_scaled_subset_2))[, 3])
}

# Plotting the silhouette width
silhouette_curve_subset_2 <- data.frame(Clusters = 2:15, Silhouette = sil_width_subset_2[-1])
ggplot(silhouette_curve_subset_2, aes(x = Clusters, y = Silhouette)) +
  geom_line() +
  scale_x_continuous(breaks = 2:15) +
  theme_minimal() +
  ggtitle("Silhouette Method for Determining Optimal Number of Clusters for Subset 2")

# Now let's print out both plots
print(elbow_curve_subset_2)
print(silhouette_curve_subset_2)

# Elbow and Silhoutte method for subset 2: Juhi: End








# Assuming 'data_for_clustering_scaled' is your scaled data ready for clustering.

# DBSCAN Clustering
dbscan_result <- dbscan(data_for_clustering_scaled, eps = 0.5, minPts = 5) # eps and minPts need to be chosen appropriately

# Plot DBSCAN results
plot(data_for_clustering_scaled, col = dbscan_result$cluster + 1L, pch = 20, cex = 0.5)
legend("topright", legend = unique(dbscan_result$cluster), col = 1:length(unique(dbscan_result$cluster)), pch = 20)

# Gaussian Mixture Model Clustering
# Determine the optimal model based on BIC
gmm_bic <- mclustBIC(data_for_clustering_scaled)
optimal_model <- which.max(gmm_bic)
gmm_result <- Mclust(data_for_clustering_scaled, G = optimal_model)

# Plot GMM results
# Check this....
plot(gmm_result, data_for_clustering_scaled)

install.packages("dbscan")


library(dbscan)
library(mclust)
library(ggplot2)

# Assuming 'data_for_clustering_scaled' is your scaled data ready for clustering

# DBSCAN Clustering
# Here you would tune eps and minPts until you get a desirable clustering which may not necessarily result in exactly three clusters
#
dbscan_result <- dbscan(data_for_clustering_scaled, eps = 0.5, minPts = 5)

# Visualize the DBSCAN clustering
plot(data_for_clustering_scaled[, c("confirmed_cases", "deaths")], col = dbscan_result$cluster + 1L, pch = 20, main = "DBSCAN Clustering")
legend("topright", legend = c("Noise", paste("Cluster", 1:max(dbscan_result$cluster))), col = 1:(max(dbscan_result$cluster) + 1), pch = 20)

# Gaussian Mixture Model Clustering with 3 components
gmm_result <- Mclust(data_for_clustering_scaled, G = 3)

# Visualize the GMM clustering
clustering <- data.frame(data_for_clustering_scaled, Cluster = gmm_result$classification)
ggplot(clustering, aes(x = confirmed_cases, y = deaths, color = factor(Cluster))) + 
  geom_point() + 
  theme_minimal() +
  ggtitle("Gaussian Mixture Model Clustering")

# Plot classification probabilities
probs <- gmm_result$z
max_prob <- apply(probs, 1, which.max)
classified_data <- data.frame(data_for_clustering_scaled, Cluster = max_prob)
ggplot(classified_data, aes(x = confirmed_cases, y = deaths, color = factor(Cluster))) +
  geom_point(alpha = 0.5) + 
  theme_minimal() +
  ggtitle("GMM Clustering with Classification Probabilities")



# DBSCAN for subset 2: Juhi: Start
dbscan_result_subset_2 <- dbscan(data_for_clustering_scaled_subset_2, eps = 0.5, minPts = 5)

# Visualize the DBSCAN clustering
plot(data_for_clustering_scaled_subset_2[, c("confirmed_cases", "deaths")], 
     col = dbscan_result_subset_2$cluster + 1L, pch = 20, main = "DBSCAN Clustering for Subset 2")
legend("topright", legend = c("Noise", paste("Cluster", 1:max(dbscan_result$cluster))), 
       col = 1:(max(dbscan_result_subset_2$cluster) + 1), pch = 20)

# Gaussian Mixture Model Clustering with 3 components
gmm_result_subset_2 <- Mclust(data_for_clustering_scaled_subset_2, G = 3)

# Visualize the GMM clustering
clustering_subset_2 <- data.frame(data_for_clustering_scaled_subset_2, 
                                  Cluster = gmm_result_subset_2$classification)
ggplot(clustering_subset_2, aes(x = confirmed_cases, y = deaths, color = factor(Cluster))) + 
  geom_point() + 
  theme_minimal() +
  ggtitle("Gaussian Mixture Model Clustering for Subset 2")

# Plot classification probabilities
probs <- gmm_result$z
max_prob <- apply(probs, 1, which.max)
classified_data <- data.frame(data_for_clustering_scaled, Cluster = max_prob)
ggplot(classified_data, aes(x = confirmed_cases, y = deaths, color = factor(Cluster))) +
  geom_point(alpha = 0.5) + 
  theme_minimal() +
  ggtitle("GMM Clustering with Classification Probabilities")

# DBSCAN for subset 2: Juhi: End

# You would need to install and load the clusterCrit package for computing Dunn index
# install.packages("clusterCrit")

library(clValid)
library(ggplot2)

# Assume 'data_for_clustering_scaled' is your scaled dataset
# and is already loaded into your R session

# Calculate Dunn index for different values of k
dunn_index_values <- numeric(length = 9)
ks <- 2:10  # Range of k we want to test

for (k in ks) {
  set.seed(123)  # Set a random seed for reproducibility
  clustering <- kmeans(data_for_clustering_scaled, centers = k, nstart = 25)
  cluster_assignment <- clustering$cluster
  # Use the clValid function to calculate the Dunn index
  dunn_index_values[k-1] <- dunn(clustering$centers, as.matrix(dist(data_for_clustering_scaled)), cluster_assignment)
}

# Create a dataframe for plotting
dunn_data <- data.frame(ks = ks, DunnIndex = dunn_index_values)

# Create the Dunn index plot
dunn_plot <- ggplot(dunn_data, aes(x = ks, y = DunnIndex)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = which.max(dunn_index_values)+1, linetype="dashed", color = "red") +
  ggtitle("Dunn Index: Optimal Number of Clusters for Subset 2 KM") +
  xlab("ks") +
  ylab("Dunn Index")

# Print the plot
print(dunn_plot)

library(cluster)
library(ggplot2)
library(dplyr)
library(factoextra)

# Assume kmeans_result contains the result of your kmeans clustering
# If not, run k-means clustering first
kmeans_result <- kmeans(data_for_clustering_scaled, centers = 3, nstart = 25)

# Calculate silhouette information
silhouette_info <- silhouette(kmeans_result$cluster, dist(data_for_clustering_scaled))

# Convert silhouette information into a dataframe for ggplot
silhouette_df <- data.frame(cluster = kmeans_result$cluster, silhouette_width = silhouette_info[, 'sil_width'])

# Create the silhouette plot using ggplot2
silhouette_plot <- ggplot(silhouette_df, aes(x = cluster, y = silhouette_width, fill = factor(cluster))) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(title = 'Clusters silhouette plot', x = 'Silhouette width')

# Print the silhouette plot
print(silhouette_plot)

# Enhanced silhouette plot using factoextra
fviz_silhouette(silhouette_info) + theme_minimal()



# Internal Validation: Calculate silhoutte information for subset 2: Juhi: Start
silhouette_info_subset_2 <- silhouette(kmeans_result_subset_2$cluster, dist(data_for_clustering_scaled_subset_2))

# Convert silhouette information into a dataframe for ggplot
silhouette_df_subset_2 <- data.frame(cluster = kmeans_result_subset_2$cluster, 
                                     silhouette_width = silhouette_info_subset_2[, 'sil_width'])

# Create the silhouette plot using ggplot2
silhouette_plot_subset_2 <- ggplot(silhouette_df_subset_2, aes(x = cluster, y = silhouette_width, 
                                                               fill = factor(cluster))) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(title = 'Clusters silhouette plot for subset 2', x = 'Silhouette width')

# Print the silhouette plot
print(silhouette_plot_subset_2)

# Enhanced silhouette plot using factoextra
fviz_silhouette(silhouette_info_subset_2) + theme_minimal()
# Internal Validation:  Calculate silhoutte information for subset 2: Juhi: End

# Define breaks that make sense for your specific context, such as quantiles
# Load necessary libraries
library(dplyr)
library(cluster)  # For silhouette
library(mclust)   # For Adjusted Rand Index
library(factoextra) # For enhanced clustering visualization

# Assuming 'data_for_clustering_scaled' is your preprocessed dataset ready for clustering
# and 'dataset' is the original dataset with the 'median_age' column.

# Step 1: Perform Clustering (example with k-means)
set.seed(123) # Ensure reproducibility
kmeans_result <- kmeans(data_for_clustering_scaled, centers = 3, nstart = 25)

# Step 2: Create Synthetic Ground Truth based on 'median_age'
# Here's an example strategy - customize this as needed:
age_groups <- cut(dataset$median_age, breaks = quantile(dataset$median_age, probs = 0:3 / 3), include.lowest = TRUE, labels = FALSE)
dataset$age_group_numeric <- as.numeric(age_groups) # Ensure it's numeric for comparison

# Step 3: Calculate External Validation Metrics
# Adjusted Rand Index
ari <- adjustedRandIndex(dataset$age_group_numeric, kmeans_result$cluster)

# Silhouette Widths
sil_widths <- silhouette(kmeans_result$cluster, dist(data_for_clustering_scaled))
mean_sil_width <- mean(sil_widths[, "sil_width"]) # Mean silhouette width for the entire dataset

# Purity
# Compute confusion matrix and derive purity
confusion_matrix <- table(True = dataset$age_group_numeric, Predicted = kmeans_result$cluster)
purity <- sum(apply(confusion_matrix, 2, max)) / sum(confusion_matrix)

# Print the metrics
cat("Adjusted Rand Index (ARI):", ari, "\n")
cat("Mean Silhouette Width:", mean_sil_width, "\n")
cat("Purity:", purity, "\n")

# Note: If you encounter any function recognition issues, ensure all packages are correctly installed and loaded.


# External Validation for subset 2: Juhi: Start


# Step 1: Perform Clustering (example with k-means)
set.seed(123) # Ensure reproducibility
kmeans_result_subset_2 <- kmeans(data_for_clustering_scaled_subset_2, centers = 3, nstart = 25)

# Step 2: Create Synthetic Ground Truth based on 'confirmed_cases'
# Here's an example strategy - customize this as needed:
# Bin 'confirmed_cases' into discrete groups based on quantiles
case_groups <- cut(dataset$confirmed_cases, 
                   breaks = quantile(dataset$confirmed_cases, 
                                     probs = seq(0, 1, length.out = 4), # For 3 groups
                                     na.rm = TRUE),
                   include.lowest = TRUE, 
                   labels = FALSE)
dataset$case_group_numeric <- as.numeric(case_groups) # Ensure it's numeric for comparison

# Step 3: Calculate External Validation Metrics
# Adjusted Rand Index
ari_subset_2 <- adjustedRandIndex(dataset$case_group_numeric, kmeans_result_subset_2$cluster)

# Silhouette Widths
sil_widths_subset_2 <- silhouette(kmeans_result_subset_2$cluster, dist(data_for_clustering_scaled_subset_2))
mean_sil_width_subset_2 <- mean(sil_widths_subset_2[, "sil_width"]) # Mean silhouette width for the entire dataset

# Purity
# Compute confusion matrix and derive purity
confusion_matrix_subset_2 <- table(True = dataset$case_group_numeric, Predicted = kmeans_result_subset_2$cluster)
purity_subset_2 <- sum(apply(confusion_matrix_subset_2, 1, max)) / sum(confusion_matrix_subset_2)

# Print the metrics
cat("Adjusted Rand Index (ARI):", ari_subset_2, "\n")
cat("Mean Silhouette Width:", mean_sil_width_subset_2, "\n")
cat("Purity:", purity_subset_2, "\n")
library(mclust)


# External Validation for subset 2: Juhi: End



# Hierarchical Clustering Juhi: Start

# HC find optimal number of clusters for subset 1: Juhi: Start

# Using NbClust to find the optimal number of clusters
install.packages("NbClust")
library(NbClust)
library(tidyverse)
nbclust_result_subset_1 <- NbClust(data_for_clustering_scaled, distance = "euclidean", min.nc = 2, 
                                   max.nc = 15, method = "ward.D2")

nbclust_result_subset_1
# Plotting the results
plot(nbclust_result_subset_1)


# Extract the frequency of suggested cluster numbers
cluster_freq_subset_1 <- table(nbclust_result_subset_1$Best.n[1,])

# Convert to a data frame for ggplot
cluster_freq_subset_1_df <- as.data.frame(cluster_freq_subset_1)
names(cluster_freq_subset_1_df) <- c("Number_of_Clusters", "Frequency")

# Plot using ggplot2
library(ggplot2)
ggplot(cluster_freq_subset_1_df, aes(x = Number_of_Clusters, y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Optimal Number of Clusters - Subset 1",
       x = "Number of Clusters",
       y = "Frequency") +
  geom_text(aes(label = Frequency), vjust = -0.5)

# HC find optimal number of clusters for subset 1: Juhi: End

# HC find optimal number of clusters for subset 2: Juhi: Start

nbclust_result_subset_2 <- NbClust(data_for_clustering_scaled_subset_2, distance = "euclidean", min.nc = 2, 
                                   max.nc = 15, method = "ward.D2")

nbclust_result_subset_2
# Plotting the results
plot(nbclust_result_subset_2)

# Extract the frequency of suggested cluster numbers
cluster_freq_subset_2 <- table(nbclust_result_subset_2$Best.n[1,])

# Convert to a data frame for ggplot
cluster_freq_subset_2_df <- as.data.frame(cluster_freq_subset_2)
names(cluster_freq_subset_2_df) <- c("Number_of_Clusters", "Frequency")

# Plot using ggplot2
library(ggplot2)
ggplot(cluster_freq_subset_2_df, aes(x = Number_of_Clusters, y = Frequency)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Optimal Number of Clusters - Subset 2",
       x = "Number of Clusters",
       y = "Frequency") +
  geom_text(aes(label = Frequency), vjust = -0.5)

# HC find optimal number of clusters for subset 2: Juhi: Start


# Dendrogram for subset 1: Juhi: Start

set.seed(123) # for reproducibility
distance_matrix_subset_1 <- dist(data_for_clustering_scaled, method = "euclidean")
hc_subset_1 <- hclust(distance_matrix_subset_1, method = "ward.D2")

# Cut the dendrogram to get 3 clusters and get cluster labels
cluster_cut_subset_1 <- cutree(hc_subset_1, k = 3)

# Coloring the dendrogram by the clusters
# Load the dendextend package
if (!requireNamespace("dendextend", quietly = TRUE)) {
  install.packages("dendextend")
}
library(dendextend)

# Convert hclust to dendrogram object
dend_subset_1 <- as.dendrogram(hc_subset_1)

# Color branches by cluster cut
dend_colored_subset_1 <- color_branches(dend_subset_1, k = 3)

# Color branches by cluster cut
dend_colored_subset_1 <- color_branches(dend_subset_1, k = 3)

# Set different parameters to make the plot more informative
dend_colored_subset_1 %>% 
  set("branches_lwd", 2) %>%  # Set the thickness of the branches
  set("branches_col", c("skyblue", "salmon", "seagreen")) %>%  # Color branches by cluster
  set("labels_cex", 0.6) %>%  # Set the size of the leaf labels
  set("labels_col", cluster_cut_subset_1) %>%  # Color labels by their cluster
  plot(main = "Dendrogram for Subset 1")  # Plot the dendrogram

# Optionally, you can cut the dendrogram for a clearer view of top-level clusters
dend_reduced_subset_1 <- cut(dend_colored_subset_1, h = 100)$upper  # Adjust the height 'h' as needed
plot(dend_reduced_subset_1, main = "Subset 1 Dendrogram with Reduced Leaves")


# Dendrogram for subset 1: Juhi: End

# Dendrogram for subset 2: Juhi: Start

set.seed(123) # for reproducibility
distance_matrix_subset_2 <- dist(data_for_clustering_scaled_subset_2, method = "euclidean")
hc_subset_2 <- hclust(distance_matrix_subset_2, method = "ward.D2")

# Cut the dendrogram to get 3 clusters and get cluster labels
cluster_cut_subset_2 <- cutree(hc_subset_2, k = 3)

# Coloring the dendrogram by the clusters
# Load the dendextend package
if (!requireNamespace("dendextend", quietly = TRUE)) {
  install.packages("dendextend")
}
library(dendextend)

# Convert hclust to dendrogram object
dend_subset_2 <- as.dendrogram(hc_subset_2)

# Color branches by cluster cut
dend_colored_subset_2 <- color_branches(dend_subset_2, k = 3)

# Set different parameters to make the plot more informative
dend_colored_subset_2 %>% 
  set("branches_lwd", 2) %>%  # Set the thickness of the branches
  set("branches_col", c("skyblue", "salmon", "seagreen")) %>%  # Color branches by cluster
  set("labels_cex", 0.6) %>%  # Set the size of the leaf labels
  set("labels_col", cluster_cut_subset_2) %>%  # Color labels by their cluster
  plot(main = "Dendrogram for Subset 2")  # Plot the dendrogram

# Optionally, you can cut the dendrogram for a clearer view of top-level clusters
dend_reduced_subset_2 <- cut(dend_colored_subset_2, h = 100)$upper  # Adjust the height 'h' as needed
plot(dend_reduced_subset_2, main = "Subset 2 Dendrogram with Reduced Leaves")


# Dendrogram for subset 2: Juhi: End

# Internal validation for subset 1: Juhi: Start

library(cluster)

silhouette_hc_subset_1 <- silhouette(cluster_cut_subset_1, distance_matrix_subset_1)

# Convert silhouette information into a dataframe for ggplot
silhouette_df_hc_subset_1 <- data.frame(cluster = cluster_cut_subset_1, 
                                        silhouette_width = silhouette_hc_subset_1[, 'sil_width'])

# Create the silhouette plot using ggplot2
silhouette_hc_plot_subset_1 <- ggplot(silhouette_df_hc_subset_1, aes(x = cluster, y = silhouette_width, 
                                                                     fill = factor(cluster))) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(title = 'HC silhouette plot for subset 1', x = 'Silhouette width')

# Print the silhouette plot
print(silhouette_hc_plot_subset_1)
# Enhanced silhouette plot using factoextra
fviz_silhouette(silhouette_hc_subset_1) + theme_minimal()


# Internal validation for subset 1: Juhi: End


# Internal validation for subset 2: Juhi: Start

library(cluster)

silhouette_hc_subset_2 <- silhouette(cluster_cut_subset_2, distance_matrix_subset_2)

# Convert silhouette information into a dataframe for ggplot
silhouette_df_hc_subset_2 <- data.frame(cluster = cluster_cut_subset_2, 
                                        silhouette_width = silhouette_hc_subset_2[, 'sil_width'])

# Create the silhouette plot using ggplot2
silhouette_hc_plot_subset_2 <- ggplot(silhouette_df_hc_subset_2, aes(x = cluster, y = silhouette_width, 
                                                                     fill = factor(cluster))) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_minimal() +
  scale_fill_viridis_d() +
  labs(title = 'HC silhouette plot for subset 1', x = 'Silhouette width')

# Print the silhouette plot
print(silhouette_hc_plot_subset_2)
# Enhanced silhouette plot using factoextra
fviz_silhouette(silhouette_hc_subset_2) + theme_minimal()


# Internal validation for subset 2: Juhi: End



# This code is not part of the report: Start
cluster_cut_5_subset_1 <- cutree(hc_subset_1, k = 5)
cluster_cut_5_subset_1

# Convert hclust to dendrogram object
dend_subset_1 <- as.dendrogram(hc_subset_1)

# Color branches by cluster cut
dend_colored_subset_1 <- color_branches(dend_subset_1, k = 5)

# Plot the colored dendrogram
par(mar = c(2,2,2,2)) # Optional: Adjust margin size
plot(dend_colored_subset_1, main = "Subset 1 Dendrogram with 5 Cuts")

# This code is not part of the report: End



# External validation for HC subset 1: Juhi: Start
confirmed_cases_hc <- cut(dataset$confirmed_cases, breaks = quantile(dataset$confirmed_cases, probs = 0:3 / 3), include.lowest = TRUE, labels = FALSE)
dataset$confirmed_cases_numeric_hc <- as.numeric(confirmed_cases_hc)

# Step 3: Calculate External Validation Metrics for Hierarchical Clustering
ari_hc_subset_1 <- adjustedRandIndex(dataset$confirmed_cases_numeric_hc, cluster_cut_subset_1)

# Silhouette Widths for Hierarchical Clustering
sil_widths_hc_subset_1 <- silhouette(cluster_cut_subset_1, dist(data_for_clustering_scaled))
mean_sil_width_hc_subset_1 <- mean(sil_widths_hc_subset_1[, "sil_width"])

# Purity for Hierarchical Clustering
confusion_matrix_hc_subset_1 <- table(True = dataset$confirmed_cases_numeric_hc, Predicted = cluster_cut_subset_1)
purity_hc_subset_1 <- sum(apply(confusion_matrix_hc_subset_1, 2, max)) / sum(confusion_matrix_hc_subset_1)

# Print the metrics for Hierarchical Clustering 
# I think ari should be lesser
cat("Adjusted Rand Index (ARI) for HC:", ari_hc_subset_1, "\n")
cat("Mean Silhouette Width for HC:", mean_sil_width_hc_subset_1, "\n")
cat("Purity for HC:", purity_hc_subset_1, "\n")


# External validation for HC subset 1:Juhi:  End


# External validation for HC subset 2: Juhi: Start

# Step 3: Calculate External Validation Metrics for Hierarchical Clustering
ari_hc_subset_2 <- adjustedRandIndex(dataset$confirmed_cases_numeric_hc, cluster_cut_subset_2)

# Silhouette Widths for Hierarchical Clustering
sil_widths_hc_subset_2 <- silhouette(cluster_cut_subset_2, dist(data_for_clustering_scaled_subset_2))
mean_sil_width_hc_subset_2 <- mean(sil_widths_hc_subset_2[, "sil_width"])

# Purity for Hierarchical Clustering
confusion_matrix_hc_subset_2 <- table(True = dataset$confirmed_cases_numeric_hc, Predicted = cluster_cut_subset_2)
purity_hc_subset_2 <- sum(apply(confusion_matrix_hc_subset_2, 2, max)) / sum(confusion_matrix_hc_subset_2)

# Print the metrics for Hierarchical Clustering 
# I think ari should be lesser
cat("Adjusted Rand Index (ARI) for HC:", ari_hc_subset_2, "\n")
cat("Mean Silhouette Width for HC:", mean_sil_width_hc_subset_2, "\n")
cat("Purity for HC:", purity_hc_subset_2, "\n")


# External validation for HC subset 2: Juhi: End



# # Hierarchical Clustering Subset 2: Juhi: Start
# 
# # Load necessary libraries
# library(dplyr)
# library(cluster)
# 
# # Assuming that 'data_for_clustering' has already been scaled and subset 1 has been selected
# 
# # Perform hierarchical clustering 
# set.seed(123) # for reproducibility
# distance_matrix_subset_2 <- dist(data_for_clustering_subset_2, method = "euclidean")
# hc_subset_2 <- hclust(distance_matrix_subset_2, method = "ward.D2")
# 
# # Cut the dendrogram to get 3 clusters and get cluster labels
# cluster_cut_subset_2 <- cutree(hc_subset_2, k = 3)
# 
# # Convert hclust to dendrogram object
# dend_subset_2 <- as.dendrogram(hc_subset_2)
# 
# # Color branches by cluster cut
# dend_colored_subset_2 <- color_branches(dend_subset_2, k = 3)
# 
# # Plot the colored dendrogram
# par(mar = c(2,2,2,2)) # Optional: Adjust margin size
# plot(dend_colored_subset_2, main = "Subset 2 Dendrogram")
# 
# 
# # Internal Validation > calculate silhoutte width for subset 1: Start
# 
# library(cluster)
# 
# silhouette_hc_subset_2 <- silhouette(cluster_cut_subset_2, distance_matrix_subset_2)
# 
# # Convert silhouette information into a dataframe for ggplot
# silhouette_df_hc_subset_2 <- data.frame(cluster = cluster_cut_subset_2, 
#                                         silhouette_width = silhouette_hc_subset_2[, 'sil_width'])
# 
# # Create the silhouette plot using ggplot2
# silhouette_hc_plot_subset_2 <- ggplot(silhouette_df_hc_subset_2, aes(x = cluster, y = silhouette_width, 
#                                                                      fill = factor(cluster))) +
#   geom_bar(stat = 'identity') +
#   coord_flip() +
#   theme_minimal() +
#   scale_fill_viridis_d() +
#   labs(title = 'HC silhouette plot for subset 2', x = 'Silhouette width')
# 
# # Print the silhouette plot
# print(silhouette_hc_plot_subset_2)
# # Enhanced silhouette plot using factoextra
# fviz_silhouette(silhouette_hc_subset_2) + theme_minimal()
# 
# # Internal Validation > calculate silhoutte width for subset 1: End
# 
# # External validation for HC subset 1: Start
# age_groups_hc <- cut(dataset$median_age, breaks = quantile(dataset$median_age, probs = 0:3 / 3), 
#                      include.lowest = TRUE, labels = FALSE)
# dataset$age_group_numeric_hc <- as.numeric(age_groups_hc)
# 
# # Step 3: Calculate External Validation Metrics for Hierarchical Clustering
# ari_hc_subset_2 <- adjustedRandIndex(dataset$age_group_numeric_hc, cluster_cut_subset_2)
# 
# # Silhouette Widths for Hierarchical Clustering
# sil_widths_hc_subset_2 <- silhouette(cluster_cut_subset_2, dist(data_for_clustering_subset_2))
# mean_sil_width_hc_subset_2 <- mean(sil_widths_hc_subset_2[, "sil_width"])
# 
# # Purity for Hierarchical Clustering
# confusion_matrix_hc_subset_2 <- table(True = dataset$age_group_numeric_hc, Predicted = cluster_cut_subset_2)
# purity_hc_subset_2 <- sum(apply(confusion_matrix_hc_subset_2, 2, max)) / sum(confusion_matrix_hc_subset_2)
# 
# # Print the metrics for Hierarchical Clustering 
# # I think ari should be lesser
# cat("Adjusted Rand Index (ARI) for HC:", ari_hc_subset_2, "\n")
# cat("Mean Silhouette Width for HC:", mean_sil_width_hc_subset_2, "\n")
# cat("Purity for HC:", purity_hc_subset_2, "\n")
# 
# # External validation for HC subset 1: End
