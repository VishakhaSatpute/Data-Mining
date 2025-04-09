# Load necessary libraries
library(dplyr)
library(readr)
library(tidyr)

# Loading the dataset
dataset <- read_csv("~/Data Mining/Project1/COVID-19_cases_plus_census.csv")


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
# data_for_clustering <- select(dataset, confirmed_cases, deaths, median_income, income_per_capita,
#                               median_age, total_pop, male_pop, female_pop, white_pop,
#                               black_pop, asian_pop, hispanic_pop, percent_income_spent_on_rent,
#                               vacant_housing_units, housing_units, median_rent, 
#                               owner_occupied_housing_units, renter_occupied_housing_units_paying_cash_median_gross_rent,
#                               families_with_young_children, unemployed_pop, civilian_labor_force, 
#                               employed_pop, not_in_labor_force, commuters_by_public_transportation, 
#                               median_year_structure_built, households, pop_15_and_over, 
#                               pop_5_years_over, pop_25_years_over, population_3_years_over, pop_25_64)
data_for_clustering <- select(dataset, median_income, income_per_capita,
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
  select(median_age, total_pop, male_pop, female_pop, 
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

# Adding cluster assignments to the scaled data (for visualization)
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

# Make sure you have the plotly package installed
install.packages("plotly")

library(plotly)

# Assuming pca_res_km is your PCA result object and has at least three dimensions
# Create a data frame for plotting
plotting_data_3d <- data.frame(
  PC1 = pca_res_km$x[, 1],
  PC2 = pca_res_km$x[, 2],
  PC3 = pca_res_km$x[, 3],  # Include third principal component
  cluster_km = as.factor(data_for_clustering_scaled_df$cluster_km)
)

# Create a 3D scatter plot
plot_3d <- plot_ly(data = plotting_data_3d, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cluster_km, colors = c("#00AFBB", "#E7B800", "#FC4E07"), 
                   type = "scatter3d", mode = "markers", 
                   marker = list(size = 5, opacity = 0.6)) %>%
  layout(title = "3D PCA of K-means Clusters",
         scene = list(
           xaxis = list(title = "PC1"),
           yaxis = list(title = "PC2"),
           zaxis = list(title = "PC3")
         ))

# Render the plot
plot_3d



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
cases = read_csv("~/Data Mining/Project1/COVID-19_cases_plus_census.csv")
cases_TX <-  cases %>% filter(state == "CA")
dim(cases_TX)
counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion))
cases_TX_scaled <- cases_TX %>% 
  select(median_age, total_pop, male_pop, female_pop, 
         white_pop, black_pop, asian_pop, hispanic_pop, households, 
         pop_15_and_over, pop_5_years_over, pop_25_years_over, 
         population_3_years_over, pop_25_64) %>% 
  scale() %>% as_tibble()

variance_info1 <- sapply(cases_TX_scaled, function(x) var(x, na.rm = TRUE))
non_zero_variance1 <- !is.na(variance_info1) & variance_info1 > 0

# Subsetting columns with non-zero variance
cases_TX_scaled <- cases_TX_scaled[, non_zero_variance1, drop = FALSE]

# Scale the data
cases_TX_scaled_1 <- scale(cases_TX_scaled)
cases_TX_scaled_1 <- na.omit(cases_TX_scaled_1)
km <- kmeans(cases_TX_scaled, centers = 3)
km
ggplot(pivot_longer(as_tibble(km$centers,  rownames = "cluster"), 
                    cols = colnames(km$centers)), 
       aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "california") %>% 
  rename(c(county = subregion))

cases_TX <- cases_TX %>% mutate(county = county_name %>% 
                                  str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

counties_TX_clust <- counties_TX %>% left_join(cases_TX %>% 
                                                 add_column(cluster = factor(km$cluster)))

ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")

library(factoextra)
library(cluster)
library(ggplot2)

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
plot(gmm_result, data_for_clustering_scaled)

install.packages("dbscan")


library(dbscan)
library(mclust)
library(ggplot2)

# Assuming 'data_for_clustering_scaled' is your scaled data ready for clustering

# DBSCAN Clustering
# Here you would tune eps and minPts until you get a desirable clustering which may not necessarily result in exactly three clusters
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

# You would need to install and load the clusterCrit package for computing Dunn index
# install.packages("clusterCrit")

library(clValid)
library(ggplot2)

dunn_index_values <- numeric(length = 9)
ks <- 2:10  # Range of k we want to test

for (k in ks) {
  set.seed(123)  # Set a random seed for reproducibility
  clustering <- kmeans(data_for_clustering_scaled, centers = k, nstart = 25)
  cluster_assignment <- clustering$cluster
  # Use the clValid function to calculate the Dunn index
  dunn_index_values[k-1] <- dunn(clustering$centers, as.matrix(dist(data_for_clustering_scaled)), cluster_assignment)
}

dunn_data <- data.frame(ks = ks, DunnIndex = dunn_index_values)


dunn_plot <- ggplot(dunn_data, aes(x = ks, y = DunnIndex)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = which.max(dunn_index_values)+1, linetype="dashed", color = "red") +
  ggtitle("Dunn Index: Optimal Number of Clusters for Subset 2 KM") +
  xlab("ks") +
  ylab("Dunn Index")


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
# Required libraries
library(cluster)
library(fpc)
library(dplyr)

# Assuming 'dataset' is your dataframe and 'true_labels' contains the true cluster labels

# Subset 1
# Load required libraries
# Load required libraries
library(cluster)  # For agnes() function
library(fpc)      # For cluster.stats()
library(dplyr)    # For data manipulation

# Assuming 'dataset' is already loaded and 'true_labels' contains the ground truth cluster labels

# Subset 1 Preparation
# Load necessary libraries
library(fpc)
library(dplyr)

# Assuming 'dataset' is your data frame
# Assuming 'true_labels' contains the actual clustering labels

# Subset 1
data_for_clustering_1 <- dataset %>%
  select(median_age, total_pop, male_pop, female_pop, 
         white_pop, black_pop, asian_pop, hispanic_pop, households, 
         pop_15_and_over, pop_5_years_over, pop_25_years_over, 
         population_3_years_over, pop_25_64) %>%
  na.omit()

# Subset 2
data_for_clustering_2 <- dataset %>%
  select(median_income, income_per_capita, percent_income_spent_on_rent, 
         vacant_housing_units, housing_units, median_rent, 
         renter_occupied_housing_units_paying_cash_median_gross_rent, 
         unemployed_pop, civilian_labor_force, employed_pop, not_in_labor_force, 
         commuters_by_public_transportation, median_year_structure_built,
         confirmed_cases, deaths) %>%
  na.omit()

# Placeholder for clustering - replace these with your actual clustering results
# Assuming these are the results from your clustering algorithm
# Let's define some hypothetical true labels for illustration.
# In your real dataset, 'true_labels' should be provided or known beforehand.
# Here we assume a dataset with 100 samples.
true_labels <- sample(1:3, 100, replace = TRUE)

# Assuming your actual clustering results for subsets are in these variables
# cluster_labels_1 <- your_clustering_function(subset_1_data)
# cluster_labels_2 <- your_clustering_function(subset_2_data)
# For the sake of this example, we'll generate some random cluster assignments
cluster_labels_1 <- sample(1:3, 100, replace = TRUE) # Placeholder for actual clustering results
cluster_labels_2 <- sample(1:3, 100, replace = TRUE) # Placeholder for actual clustering results

# Calculating the Adjusted Rand Index for both subsets using the 'fpc' package
library(fpc)

ari_subset_1 <- adjustedRandIndex(true_labels, cluster_labels_1)
ari_subset_2 <- adjustedRandIndex(true_labels, cluster_labels_2)

cat("Adjusted Rand Index for Subset 1:", ari_subset_1, "\n")
cat("Adjusted Rand Index for Subset 2:", ari_subset_2, "\n")

# Function to calculate Entropy and Purity of clusters
calculate_metrics <- function(true_labels, cluster_labels) {
  unique_clusters <- unique(cluster_labels)
  n <- length(true_labels)
  
  total_entropy <- 0
  total_purity <- 0
  for (cluster in unique_clusters) {
    # Indices of items in the current cluster
    indices <- which(cluster_labels == cluster)
    # Subset of true labels for the current cluster
    cluster_true_labels <- true_labels[indices]
    # Count the frequency of the true labels in the cluster
    freq <- table(cluster_true_labels)
    # Calculate purity by finding the max frequency
    purity <- max(freq) / sum(freq)
    total_purity <- total_purity + (length(indices) / n) * purity
    # Calculate entropy
    probs <- freq / sum(freq)
    entropy <- -sum(probs * log(probs))
    total_entropy <- total_entropy + (length(indices) / n) * entropy
  }
  
  return(list(purity = total_purity, entropy = total_entropy))
}

# Assuming the true_labels, cluster_labels_1, and cluster_labels_2 are already defined
metrics_1 <- calculate_metrics(true_labels, cluster_labels_1)
metrics_2 <- calculate_metrics(true_labels, cluster_labels_2)

cat("Subset 1 - Purity:", metrics_1$purity, "Entropy:", metrics_1$entropy, "\n")
cat("Subset 2 - Purity:", metrics_2$purity, "Entropy:", metrics_2$entropy, "\n")

# Calculate entropy
calculate_entropy <- function(labels) {
  n <- length(labels)
  freq <- table(labels) / n
  return(-sum(freq * log(freq)))
}

# Calculate mutual information
calculate_mutual_information <- function(labels1, labels2) {
  total <- length(labels1)
  contingency_table <- table(labels1, labels2) / total
  marginal1 <- apply(contingency_table, 1, sum)
  marginal2 <- apply(contingency_table, 2, sum)
  
  mutual_information <- sum(contingency_table * log(contingency_table / (marginal1 %o% marginal2)), na.rm = TRUE)
  return(mutual_information)
}

# Calculate VI
calculate_VI <- function(labels1, labels2) {
  entropy1 <- calculate_entropy(labels1)
  entropy2 <- calculate_entropy(labels2)
  mutual_information <- calculate_mutual_information(labels1, labels2)
  
  VI <- entropy1 + entropy2 - 2 * mutual_information
  return(VI)
}
# Calculate VI between the true labels and each set of cluster labels
VI_1_true <- calculate_VI(true_labels, cluster_labels_1)
VI_2_true <- calculate_VI(true_labels, cluster_labels_2)

cat("VI between true labels and Subset 1 clustering:", VI_1_true, "\n")
cat("VI between true labels and Subset 2 clustering:", VI_2_true, "\n")

# Define scaled datasets
# Load necessary libraries
# Load required libraries
library(dplyr)
library(cluster)  # For hclust
library(fpc)      # For adjustedRandIndex
library(stats)    # For scale

# Assuming 'data_for_clustering_1', 'data_for_clustering_2', and 'true_labels' are pre-defined

# Helper functions for metrics calculation
calculate_purity <- function(true_labels, cluster_labels) {
  contingency_table <- table(true_labels, cluster_labels)
  purity <- sum(apply(contingency_table, 2, max)) / sum(contingency_table)
  return(purity)
}

calculate_entropy <- function(labels) {
  freq <- table(labels) / length(labels)
  entropy <- -sum(freq * log(freq))
  return(entropy)
}

calculate_mutual_information <- function(true_labels, cluster_labels) {
  N <- length(true_labels)
  contingency_matrix <- table(true_labels, cluster_labels)
  joint_prob <- contingency_matrix / N
  marginal_true <- rowSums(joint_prob)
  marginal_cluster <- colSums(joint_prob)
  mutual_information <- sum(joint_prob * log(joint_prob / (marginal_true %o% marginal_cluster)), na.rm = TRUE)
  return(mutual_information)
}

calculate_VI <- function(true_labels, cluster_labels) {
  mutual_info <- calculate_mutual_information(true_labels, cluster_labels)
  entropy_true <- calculate_entropy(true_labels)
  entropy_cluster <- calculate_entropy(cluster_labels)
  VI <- entropy_true + entropy_cluster - 2 * mutual_info
  return(VI)
}

# Perform clustering and calculate metrics function
perform_analysis <- function(data_subset, true_labels) {
  data_scaled <- scale(na.omit(data_subset))  # Scale the data and omit NA
  
  num_rows <- nrow(data_scaled)
  num_clusters <- min(3, num_rows)
  
  metrics <- list()
  
  if(num_clusters > 1) {
    # K-means Clustering
    kmeans_res <- kmeans(data_scaled, centers = num_clusters, nstart = 25)
    
    # Hierarchical Clustering
    hc_res <- hclust(dist(data_scaled), method = "ward.D2")
    hc_clusters <- cutree(hc_res, k = num_clusters)
    
    # Adjust true_labels to match the length
    true_labels_adj <- true_labels[seq_len(num_rows)]
    
    # Calculate metrics
    metrics <- list(
      KMeans = list(
        ARI = adjustedRandIndex(true_labels_adj, kmeans_res$cluster),
        Purity = calculate_purity(true_labels_adj, kmeans_res$cluster),
        Entropy = calculate_entropy(kmeans_res$cluster),
        VI = calculate_VI(true_labels_adj, kmeans_res$cluster)
      ),
      HC = list(
        ARI = adjustedRandIndex(true_labels_adj, hc_clusters),
        Purity = calculate_purity(true_labels_adj, hc_clusters),
        Entropy = calculate_entropy(hc_clusters),
        VI = calculate_VI(true_labels_adj, hc_clusters)
      )
    )
  } else {
    cat("Not enough data points for clustering. Please check your dataset.\n")
  }
  
  return(metrics)
}

# Perform the analysis for both subsets
metrics_subset_1 <- perform_analysis(data_for_clustering_1, true_labels)
metrics_subset_2 <- perform_analysis(data_for_clustering_2, true_labels)

# Output the results
cat("Metrics for Subset 1:\n")
print(metrics_subset_1)
cat("\nMetrics for Subset 2:\n")
print(metrics_subset_2)

library(dplyr)
library(cluster)  # For clustering functions
library(fpc)      # For adjustedRandIndex
library(stats)    # For scale

# Subset 1 Features Selection
data_for_clustering_1 <- dataset %>%
  select(median_age, total_pop, male_pop, female_pop, 
         white_pop, black_pop, asian_pop, hispanic_pop, households, 
         pop_15_and_over, pop_5_years_over, pop_25_years_over, 
         population_3_years_over, pop_25_64) %>%
  na.omit() %>%
  scale()

# Determine the appropriate number of clusters
data_for_clustering <- dataset %>%
  select(median_age, total_pop, male_pop, female_pop, 
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

calculate_purity <- function(true_labels, cluster_labels) {
  contingency_table <- table(true_labels, cluster_labels)
  purity <- sum(apply(contingency_table, 2, max)) / sum(contingency_table)
  return(purity)
}

calculate_entropy <- function(labels) {
  freq <- table(labels) / length(labels)
  entropy <- -sum(freq * log(freq))
  return(entropy)
}

calculate_mutual_information <- function(true_labels, cluster_labels) {
  N <- length(true_labels)
  contingency_matrix <- table(true_labels, cluster_labels)
  joint_prob <- contingency_matrix / N
  marginal_true <- rowSums(joint_prob)
  marginal_cluster <- colSums(joint_prob)
  mutual_information <- sum(joint_prob * log(joint_prob / (marginal_true %o% marginal_cluster)), na.rm = TRUE)
  return(mutual_information)
}

calculate_VI <- function(true_labels, cluster_labels) {
  mutual_info <- calculate_mutual_information(true_labels, cluster_labels)
  entropy_true <- calculate_entropy(true_labels)
  entropy_cluster <- calculate_entropy(cluster_labels)
  VI <- entropy_true + entropy_cluster - 2 * mutual_info
  return(VI)
}

# Assuming 'true_labels' is correctly sized and ordered
# Adjusted Rand Index
# Adjusted Rand Index for K-means clustering
# Check the length of true_labels
length_true_labels <- length(true_labels)
length_clusters_hc <- length(clusters_hc)

# Ensure the lengths match
if (length_true_labels != length_clusters_hc) {
  # Trim the longer vector to match the shorter one
  min_length <- min(length_true_labels, length_clusters_hc)
  true_labels <- true_labels[1:min_length]
  clusters_hc <- clusters_hc[1:min_length]
}

ari_kmeans <- adjustedRandIndex(true_labels, kmeans_result$cluster)

# Adjusted Rand Index for hierarchical clustering
ari_hc <- adjustedRandIndex(true_labels, clusters_hc)

# Purity for K-means clustering
purity_kmeans <- calculate_purity(true_labels, kmeans_result$cluster)

# Purity for hierarchical clustering
purity_hc <- calculate_purity(true_labels, clusters_hc)

# Entropy for K-means clustering
entropy_kmeans <- calculate_entropy(kmeans_result$cluster)

# Entropy for hierarchical clustering
entropy_hc <- calculate_entropy(clusters_hc)

# Variation of Information for K-means clustering
vi_kmeans <- calculate_VI(true_labels, kmeans_result$cluster)

# Variation of Information for hierarchical clustering
vi_hc <- calculate_VI(true_labels, clusters_hc)

# Print Results
cat("K-means Clustering Metrics:\n")
cat("ARI:", ari_kmeans, "\nPurity:", purity_kmeans, "\nEntropy:", entropy_kmeans, "\nVI:", vi_kmeans, "\n\n")

cat("Hierarchical Clustering Metrics:\n")
cat("ARI:", ari_hc, "\nPurity:", purity_hc, "\nEntropy:", entropy_hc, "\nVI:", vi_hc, "\n")

# Assuming you have your dataset loaded and named as 'data'

# 1. Subset the Important Features

library(dplyr)

data_for_clustering <- dataset %>%
  select(median_age, total_pop, male_pop, female_pop, 
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

# Extracting the cluster assignments
cluster_assignments <- kmeans_result$cluster

# Assuming your dataset has a column 'confirmed_cases' and 'asian_pop'
# Create a data frame for plotting that includes the cluster assignments
data_subset <- data.frame(
  confirmed_cases = dataset$confirmed_cases,
  asian_pop = dataset$asian_pop,
  cluster_km = as.factor(cluster_assignments)
)

# Plot Scatter Plot
plot(data_subset$confirmed_cases, data_subset$asian_pop, 
     main = "Clusters of COVID Impact vs Asian Population",
     xlab = "Asian Population", ylab = "COVID Impact",
     xlim = c(0, 0.5), ylim = c(0, 0.5))

# Loop through each cluster to plot
for (i in 1:3) {
  # Subset the data for the current cluster
  cluster_data <- data_subset[data_subset$cluster_km == i, ]
  # Plot the points with different shapes for each cluster
  points(cluster_data$confirmed_cases, cluster_data$asian_pop, col = i, pch = i)
}

# Add legend
legend("topright", legend = paste("Cluster", 1:3), col = 1:3, pch = 1:3)


# Assuming 'data_for_clustering_scaled_df' contains your scaled data with cluster assignments
# Assuming 'population_3_years_over' and 'pop_25_64' are columns in your dataset

# Initialize an empty plot
# Assuming 'data_for_clustering_scaled_df' contains your scaled data with cluster assignments
# Assuming 'population_3_years_over', 'confirmed_cases', and 'cluster_km' are columns in your dataset

# Initialize an empty plot
# Assuming 'data_for_clustering_scaled_df' contains your scaled data with cluster assignments
# Assuming 'population_3_years_over', 'confirmed_cases', and 'cluster_km' are columns in your dataset

# Set the number of clusters (k) determined by your K-means model
k <- 3

# Plot for Population 3 Years Over vs Confirmed Cases
plot(data_for_clustering_scaled_df$population_3_years_over, 
     data_for_clustering_scaled_df$confirmed_cases, 
     main = "Clusters of Population 3 Years Over vs Confirmed Cases",
     xlab = "Population 3 Years Over", ylab = "Confirmed Cases",
     xlim = c(min(data_for_clustering_scaled_df$population_3_years_over), max(data_for_clustering_scaled_df$population_3_years_over)),
     ylim = c(min(data_for_clustering_scaled_df$confirmed_cases), max(data_for_clustering_scaled_df$confirmed_cases)))

# Loop through each cluster
for (i in 1:k) {
  # Subset the data for the current cluster
  cluster_data <- data_for_clustering_scaled_df[data_for_clustering_scaled_df$cluster_km == i, ]
  # Plot the points with different colors for each cluster
  points(cluster_data$population_3_years_over, cluster_data$confirmed_cases, col = i, pch = i)
}

# Add legend
legend("topright", legend = paste("Cluster", 1:k), col = 1:k, pch = 1:k)


# Plot for Pop 25-64 vs Confirmed Cases
plot(data_for_clustering_scaled_df$pop_25_64, 
     data_for_clustering_scaled_df$confirmed_cases, 
     main = "Confirmed Cases vs Pop 25-64",
     xlab = "Pop 25-64", ylab = "Confirmed Cases",
     xlim = c(min(data_for_clustering_scaled_df$pop_25_64), max(data_for_clustering_scaled_df$pop_25_64)),
     ylim = c(min(data_for_clustering_scaled_df$confirmed_cases), max(data_for_clustering_scaled_df$confirmed_cases)))

# Loop through each cluster
for (i in 1:k) {
  # Subset the data for the current cluster
  cluster_data <- data_for_clustering_scaled_df[data_for_clustering_scaled_df$cluster_km == i, ]
  # Plot the points with different colors for each cluster
  points(cluster_data$pop_25_64, cluster_data$confirmed_cases, col = i, pch = i)
}

# Add legend
legend("topright", legend = paste("Cluster", 1:k), col = 1:k, pch = 1:k)


# Perform DBSCAN clustering
# Perform DBSCAN clustering
# Perform DBSCAN clustering
# Perform DBSCAN clustering
library(e1071)  # Load the required library

# Perform C-means clustering
cmeans_result <- cmeans(data_for_clustering_scaled, centers = 3)

# Extract cluster memberships
cluster_memberships <- cmeans_result$cluster

# Calculate silhouette widths
sil_width <- silhouette(cluster_memberships, dist(data_for_clustering_scaled))

# Print silhouette widths
print(sil_width)

# Plot C-means clusters
plot(data_for_clustering_scaled, col = cluster_memberships,
     main ="C-means Clustering")


# Count the number of points in each cluster

library(cluster)  # Load the required library

# Calculate silhouette widths for each data point
sil_width <- silhouette(cluster_memberships, dist(data_for_clustering_scaled))

# Extract silhouette widths and cluster memberships
sil_widths <- sil_width[, "sil_width"]
cluster_membership <- sil_width[, "cluster"]

# Calculate average silhouette width for each cluster
avg_sil_widths <- tapply(sil_widths, cluster_membership, mean)

# Print average silhouette width for each cluster
for (i in unique(cluster_membership)) {
  cat("Cluster", i, ":", avg_sil_widths[i], "\n")
}

library(cluster)

# Perform BIRCH clustering
birch_result <- birch(data_for_clustering_scaled, k = 3)

# Plot BIRCH clusters
plot(data_for_clustering_scaled, col = birch_result$cluster,
     main = "BIRCH Clustering", xlab = "Feature 1", ylab = "Feature 2")

library(cluster)

# Perform hierarchical clustering
hierarchical_result <- agnes(data_for_clustering_scaled)

# Cut the dendrogram to obtain cluster assignments
clusters <- cutree(hierarchical_result, k = 3)

# Plot hierarchical clusters
plot(data_for_clustering_scaled, col = clusters,
     main = "Hierarchical Clustering (Agglomerative)", xlab = "Feature 1", ylab = "Feature 2")

library(e1071)  # Load the required library

# Perform C-means clustering
cmeans_result <- cmeans(data_for_clustering_scaled, centers = 3)

# Extract cluster memberships
cluster_memberships <- cmeans_result$cluster

# Calculate the size of each cluster
cluster_sizes <- table(cluster_memberships)

# Print the size of each cluster
print(cluster_sizes)

library(e1071)  # Load the required library
library(cluster) # Load the cluster library for silhouette function

# Perform C-means clustering
cmeans_result <- cmeans(data_for_clustering_scaled, centers = 3)

# Extract cluster memberships
cluster_memberships <- cmeans_result$cluster

# Calculate silhouette widths
sil_width <- silhouette(cluster_memberships, dist(data_for_clustering_scaled))

# Plot silhouette widths for each cluster
par(mfrow=c(1,3)) # Set up a grid for 3 plots
for (i in unique(cluster_memberships)) {
  cluster_silhouette <- subset(sil_width, cluster_memberships == i)
  plot(cluster_silhouette, main = paste("Silhouette Plot for Cluster", i),
       ylim = c(0, 1), col = 1)
}

library(e1071)  # Load the required library
library(cluster) # Load the cluster library for silhouette function

# Perform C-means clustering
cmeans_result <- cmeans(data_for_clustering_scaled, centers = 3)

# Extract cluster memberships
cluster_memberships <- cmeans_result$cluster

# Calculate silhouette widths
sil_width <- silhouette(cluster_memberships, dist(data_for_clustering_scaled))

# Plot silhouette widths for each cluster
par(mfrow=c(1,3)) # Set up a grid for 3 plots
for (i in unique(cluster_memberships)) {
  cluster_silhouette <- subset(sil_width, cluster_memberships == i)
  plot(cluster_silhouette, type = "o", 
       main = paste("Silhouette Plot for Cluster", i),
       ylim = c(-1, 1), col = "blue", lwd = 2)
}

# Load necessary libraries
library(dplyr)
library(mclust)
install.packages("infotheo")
library(infotheo)

# Load the dataset
data <- read.csv("~/Data Mining/Project1/COVID-19_cases_plus_census.csv")
print(colnames(data))

# Filter for counties in California (assuming there is a 'state' column)
data_california <- filter(data,state == "CA")

# Define the rate of cases (new cases per 1000 people per week)
# Ensure you replace 'date' and 'confirmed_cases' with the correct column names for date and case counts
# You'll need to calculate new cases per week if it's not already in your dataset
# For this example, I'll assume 'confirmed_cases' already represents weekly new cases
data_california$rate_of_cases <- (data_california$confirmed_cases / data_california$total_pop) * 1000

# Set the threshold for high risk
threshold <- 10  # Define your own threshold based on your criteria

# Label counties based on the threshold
data_california$risk_label <- ifelse(data_california$rate_of_cases >= threshold, 'high_risk', 'low_risk')

# Select features for clustering
# Replace these example features with the actual features you want to use
selected_features <- data_california[c('median_income', 'percent_income_spent_on_rent', 'median_age')]

# Normalize features
selected_features_scaled <- scale(selected_features)

# Perform K-Means clustering with 2 clusters
set.seed(123)  # Set seed for reproducibility
kmeans_result <- kmeans(selected_features_scaled, centers = 2)

# Assign clusters back to the original data
data_california$cluster <- kmeans_result$cluster

# Evaluate Clustering
ARI <- adjustedRandIndex(data_california$risk_label, data_california$cluster)
VI <- mutinformation(as.factor(data_california$risk_label), as.factor(data_california$cluster), method="variation")

# Output results
cat("Adjusted Rand Index (ARI):", ARI, "\n")
cat("Variation of Information (VI):", VI, "\n")

interpret_ARI <- if (ARI > 0.75) {
  "Strong match"
} else if (ARI > 0.5) {
  "Moderate match"
} else {
  "Weak match"
}

interpret_VI <- if (VI < 1) {
  "Strong separation"
} else if (VI < 2) {
  "Moderate separation"
} else {
  "Weak separation"
}

cat("ARI Interpretation:", interpret_ARI, "\n")
cat("VI Interpretation:", interpret_VI, "\n")

