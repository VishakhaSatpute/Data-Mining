library(dplyr)
library(readr)

# Load the dataset
dataset <- read_csv("C:/Users/juhic/Juhi/SMU/Semester_2/Data_Mining/Projects/Datasets/COVID-19_cases_plus_census.csv")
print(dim(dataset))

# Display initial missing values count
print("Initial missing values per column:")
print(colSums(is.na(dataset)))

# Function to calculate the mode for any column
getMode <- function(v) {
  if(all(is.na(v))) return(NA)  # Return NA if all values are NA
  uniqv <- unique(na.omit(v))
  # Return the mode
  modes <- uniqv[which.max(tabulate(match(v, uniqv)))]
  if (length(modes) == 0) NA else modes
}

# Imputing missing values for all columns appropriately
dataset <- dataset %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(~ !is.numeric(.)), ~ifelse(is.na(.), getMode(.), .)))

# Identify columns that are still all NA after imputation
all_na_columns <- colSums(is.na(dataset)) == nrow(dataset)

# Display columns that are still all NA
if(any(all_na_columns)) {
  print("Columns that are still entirely NA:")
  print(names(dataset)[all_na_columns])
}

# Optionally remove or fill columns that are entirely NA with a placeholder
dataset <- dataset %>%
  mutate(across(which(all_na_columns), ~ifelse(is.na(.), "Placeholder", .)))
print(dim(dataset))
# Display missing values count after imputation
print("Missing values per column after imputation:")
print(colSums(is.na(dataset)))

# Check for any remaining NAs
remaining_na <- sum(is.na(dataset))
if(remaining_na > 0) {
  print(paste("There are still", remaining_na, "missing values after attempted imputation."))
} else {
  print("All missing values have been imputed.")
}

library(caret)  # For PCA
library(corrplot)  # For visualization of the correlation matrix

# Select only numeric columns for correlation analysis
# Remove columns with zero variance before calculating the correlation
numeric_data <- select(dataset, where(is.numeric)) %>%
  select_if(~ sd(.) != 0)

print(dim(dataset))
# Calculate the correlation matrix again
cor_matrix <- cor(numeric_data, use = "complete.obs")


# Visualize the correlation matrix
corrplot(cor_matrix, order = "hclust", addrect = 2)

# Find highly correlated features and remove them
# The function will return indices of features to remove
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.95)
numeric_data_reduced <- numeric_data[,-highly_correlated]

print(dim(numeric_data_reduced))
# Visualize the correlation matrix after removing highly correlated features
cor_matrix_reduced <- cor(numeric_data_reduced, use = "complete.obs")
corrplot(cor_matrix_reduced, order = "hclust", addrect = 2)

# Apply PCA to the reduced numeric dataset to identify principal components


# Now you have the principal components in `numeric_data_pca`.
# You can use these principal components as input features for further modeling.
install.packages("ggfortify")
library(ggfortify)

# Perform PCA using base R's prcomp function
# Make sure to scale and center the data for PCA
pca_result <- prcomp(numeric_data_reduced, center = TRUE, scale. = TRUE)

# Visualize the PCA results
autoplot(pca_result, data = numeric_data_reduced, label = TRUE, label.size = 3)

# Train machine learning models
# Example using Random Forest
# Load necessary libraries
library(dplyr)

dataset <- dataset %>%
  mutate(confirmed_cases = ifelse(is.na(confirmed_cases), median(confirmed_cases, na.rm = TRUE), confirmed_cases),
         deaths = ifelse(is.na(deaths), median(deaths, na.rm = TRUE), deaths))

print(dim(dataset))
# Calculate 'deaths_per_case' by dividing the number of deaths by the number of confirmed cases
dataset <- dataset %>%
  mutate(deaths_per_case = deaths / confirmed_cases)


# Check for missing values in the new 'deaths_per_case' column
if (any(is.na(dataset$deaths_per_case))) {
  print("Warning: There are still missing values in the 'deaths_per_case' column.")
} else {
  print("The 'deaths_per_case' column has been successfully computed.")
}

missing_deaths_per_case <- sum(is.na(dataset$deaths_per_case))

# Print the number of missing values
print(paste("Number of missing values in 'deaths_per_case' column:", missing_deaths_per_case))

# Inspect rows with missing values in 'deaths_per_case' column
print("Rows with missing values in 'deaths_per_case' column:")
print(dataset[is.na(dataset$deaths_per_case), ])

print(dim(dataset))
# dataset <- dataset %>% 
#   filter(!is.na(deaths_per_case))
# Impute missing values in the 'deaths_per_case' column with the median
dataset <- dataset %>%
  mutate(deaths_per_case = ifelse(is.na(deaths_per_case), median(deaths_per_case, na.rm = TRUE), deaths_per_case))



# Confirm that the rows have been removed
print("Number of missing values in 'deaths_per_case' column after removal:")
print(sum(is.na(dataset$deaths_per_case)))
print(dim(dataset))
# Load necessary libraries
library(caret)
library(e1071)  # Required for SVM

# Define the control function for RFE with SVM
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 5)

# Define the SVM function for RFE
svmFuncs <- caretFuncs

# Modify the SVM function to use the svm model
svmFuncs$summary <- twoClassSummary

# Perform RFE with SVM
rfe_results_svm <- rfe(x = dataset[, -which(names(dataset) == "deaths_per_case")], 
                       y = dataset$deaths_per_case, 
                       sizes = c(1:10), 
                       rfeControl = ctrl,
                       method = "svmRadial")

# Print the results
print(rfe_results_svm)

# Get the top 10 features
top_features_svm <- predict(rfe_results_svm, dataset[, -which(names(dataset) == "deaths_per_case")])
print(top_features_svm)
print(head(top_features_svm))

top_features_sorted <- sort(top_features_svm, decreasing = TRUE)

# Get the names of the sorted features
top_feature_names <- names(top_features_sorted)

# Assuming 'dataset' has column names that correspond to the feature names
# and the 'top_features_svm' are indices of the columns in the dataset

# Map the indices to actual feature names
top_feature_names <- names(dataset)[as.numeric(top_feature_names)]

# Print the names of the top 30 features
print(top_feature_names[1:30])

# Check the names of the columns in the dataset
print(names(dataset))

# Assuming 'top_features_indices' contains the indices from the RFE output
top_features_indices <- c(1918, 2540, 2783, 475, 455, 528, 967, 2622, 2586, 1955, 436, 2829, 2645,
                          2559, 471, 442, 1633, 1676, 2398, 2852, 308, 2374, 1260, 2581, 2406, 1267,
                          2556, 2554, 1348, 1970)

# Map the indices to actual feature names (if they are correct and within the number of columns)
top_feature_names <- names(dataset)[top_features_indices]

# Print the names of the top 30 features
print(top_feature_names)

# Print the names of all columns in the dataset
print(names(dataset))

# Print the total number of columns in the dataset
print(length(names(dataset)))

# Check if the necessary columns are present and then compute new features
print(dim(dataset))
# Percentage of Black Population
if("black_pop" %in% names(dataset) && "total_pop" %in% names(dataset)) {
  dataset$percent_black_pop <- (dataset$black_pop / dataset$total_pop) * 100
}

# Percentage of Hispanic Population
if("hispanic_pop" %in% names(dataset) && "total_pop" %in% names(dataset)) {
  dataset$percent_hispanic_pop <- (dataset$hispanic_pop / dataset$total_pop) * 100
}

# Percentage of Asian Population
if("asian_pop" %in% names(dataset) && "total_pop" %in% names(dataset)) {
  dataset$percent_asian_pop <- (dataset$asian_pop / dataset$total_pop) * 100
}

# Population Density
# Note: This requires an 'area' column (e.g., area in square miles)


# Ensure that you replace 'area_sq_mi' with the actual column name for area if available

# Print the head of the dataset to verify new columns
print(head(dataset))

# Assuming you have these columns for individuals aged 65 and older
if(all(c("male_65_to_66", "male_67_to_69", "male_70_to_74", "male_75_to_79", "male_80_to_84", "male_85_and_over",
         "female_65_to_66", "female_67_to_69", "female_70_to_74", "female_75_to_79", "female_80_to_84", "female_85_and_over") %in% names(dataset))) {
  
  dataset$population_over_65 <- dataset$male_65_to_66 + dataset$male_67_to_69 + dataset$male_70_to_74 + 
    dataset$male_75_to_79 + dataset$male_80_to_84 + dataset$male_85_and_over +
    dataset$female_65_to_66 + dataset$female_67_to_69 + dataset$female_70_to_74 +
    dataset$female_75_to_79 + dataset$female_80_to_84 + dataset$female_85_and_over
  
  dataset$percent_population_over_65 <- (dataset$population_over_65 / dataset$total_pop) * 100
  
  # Print the first few rows to check
  print(head(dataset[, c("total_pop", "population_over_65", "percent_population_over_65")]))
} else {
  warning("Not all necessary age group columns are available in the dataset")
}

# Check if the necessary column is available
if("deaths_per_case" %in% names(dataset)) {
  # Create the classification based on defined thresholds
  dataset$deaths_classification <- with(dataset, ifelse(deaths_per_case > 0.02, "High",
                                                        ifelse(deaths_per_case >= 0.01, "Medium", "Low")))
  # Print the first few rows to check the new column
  print(head(dataset[, c("deaths_per_case", "deaths_classification")]))
} else {
  warning("The 'deaths_per_case' column is not available in the dataset")
}

# Check if the necessary columns are available
if(all(c("confirmed_cases", "deaths", "total_pop") %in% names(dataset))) {
  # Calculate cases per 10,000
  dataset$cases_per_10000 <- (dataset$confirmed_cases / dataset$total_pop) * 10000
  
  # Calculate deaths per 10,000
  dataset$deaths_per_10000 <- (dataset$deaths / dataset$total_pop) * 10000
  
  # Print the first few rows to check the new columns
  print(head(dataset[, c("total_pop", "confirmed_cases", "deaths", "cases_per_10000", "deaths_per_10000")]))
} else {
  warning("One or more of the required columns ('confirmed_cases', 'deaths', 'total_pop') are not available in the dataset")
}


library(dplyr)
library(ggplot2)

# Define a function to detect and optionally remove outliers
detect_remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Identify outliers
  outliers <- data[[column]] < lower_bound | data[[column]] > upper_bound
  
  # Optional: Remove outliers and return cleaned data
  # data <- data[!outliers, ]
  
  list(data = data, outliers = outliers)
}

# List of features to check for outliers
features_to_check <- c("median_age", "total_pop", "confirmed_cases", "percent_population_over_65", 
                       "households_no_cars", "median_income", "poverty", "income_per_capita", 
                       "percent_black_pop", "percent_hispanic_pop", "population_density", 
                       "commuters_by_public_transportation", "housing_units_renter_occupied", 
                       "vacant_housing_units", "dwellings_50_or_more_units", "nonfamily_households", 
                       "percent_income_spent_on_rent", "male_pop", "female_pop", 
                       "households_public_asst_or_food_stamps", "percent_asian_pop", 
                       "cases_per_10000", "deaths_per_10000", "deaths_per_case")

# Applying the function and creating boxplots for each feature
for (feature in features_to_check) {
  if (feature %in% names(dataset)) {
    result <- detect_remove_outliers(dataset, feature)
    
    # Plot the boxplot
    p <- ggplot(result$data, aes_string(x = factor(1), y = feature)) +
      geom_boxplot(outlier.colour = "red", fill = "lightblue") +
      labs(title = paste("Boxplot of", feature), y = feature, x = "") +
      theme_minimal()
    print(p)
    
    # Uncomment to use cleaned data
    # dataset <- result$data
    
    # Optionally, report the number of detected outliers
    cat("Number of outliers detected in", feature, ":", sum(result$outliers), "\n")
  }
}

library(ggplot2)
library(tidyr)

# Ensure that you're using the correct column names from your dataset
correct_feature_names <- c("median_age", "total_pop", "confirmed_cases", "median_income", 
                           "poverty", "income_per_capita", "percent_black_pop", "percent_hispanic_pop", 
                           "population_density", "commuters_by_public_transportation", 
                           "housing_units_renter_occupied", "vacant_housing_units", 
                           "dwellings_50_or_more_units", "nonfamily_households", 
                           "percent_income_spent_on_rent", "male_pop", "female_pop", 
                           "households_public_asst_or_food_stamps", "percent_asian_pop", 
                           "cases_per_10000", "deaths_per_10000", "deaths_per_case")

# Now check if all the features exist in the dataset before proceeding
existing_features <- correct_feature_names[correct_feature_names %in% names(dataset)]

# Use only the existing features for the long dataset
long_dataset <- tidyr::pivot_longer(dataset, cols = existing_features, names_to = "feature", values_to = "value")

# Plot the boxplots
p <- ggplot(long_dataset, aes(x = feature, y = value)) +
  geom_boxplot() +
  facet_wrap(~ feature, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = NULL, y = NULL) # Remove axis labels

print(p)

library(ggplot2)
library(dplyr)
library(tidyr)

# Function to remove outliers
remove_outliers <- function(df, feature) {
  Q1 <- quantile(df[[feature]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[feature]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Filter out the outliers
  df <- df %>% filter(df[[feature]] >= lower_bound & df[[feature]] <= upper_bound)
  return(df)
}

# List of continuous features for which to remove outliers
continuous_features <- c("median_age", "total_pop", "confirmed_cases",
                         "median_income", "poverty", "income_per_capita",
                         "percent_black_pop", "percent_hispanic_pop",
                         "commuters_by_public_transportation",
                         "housing_units_renter_occupied", "vacant_housing_units",
                         "dwellings_50_or_more_units", "nonfamily_households",
                         "percent_income_spent_on_rent", "male_pop", "female_pop",
                         "households_public_asst_or_food_stamps", "percent_asian_pop",
                         "cases_per_10000", "deaths_per_10000", "deaths_per_case")

# Remove outliers for each feature
for (feature in continuous_features) {
  if (feature %in% names(dataset)) {
    dataset <- remove_outliers(dataset, feature)
  }
}

# Confirming outlier removal by re-plotting
# Transform the cleaned dataset to long format for plotting
dataset_long <- dataset %>%
  pivot_longer(cols = continuous_features, names_to = "feature", values_to = "value")

# Plot the boxplots
p <- ggplot(dataset_long, aes(x = feature, y = value)) +
  geom_boxplot() +
  facet_wrap(~ feature, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = NULL, y = NULL) # Remove axis labels

# Print the plot
print(p)


# Load necessary libraries
library(dplyr)

# Assuming 'dataset' is your main dataframe and it's already been loaded
# Select only the continuous features of interest
continuous_data <- dataset %>% select(all_of(continuous_features))

# Remove any NA values before calculating the correlation to avoid errors
continuous_data_clean <- na.omit(continuous_data)
print(dim(continuous_data_clean))
# Calculate the correlation matrix for the selected continuous features
correlation_matrix <- cor(continuous_data_clean)

# View the correlation matrix
print(correlation_matrix)

# Optionally, if you want a visual representation, you could use the corrplot package
# Install it if you haven't already
# install.packages("corrplot")

library(corrplot)

# Visualize the correlation matrix
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust",
         tl.cex = 0.6, # Text label size
         tl.col = "black") # Text color

# Assuming 'dataset' is your dataset containing the 'deaths_classification' feature

# Calculate counts of low, medium, and high-risk categories
classification_counts <- table(dataset$deaths_classification)

# Extract counts for each category
low_count <- classification_counts["Low"]
medium_count <- classification_counts["Medium"]
high_count <- classification_counts["High"]

# Print the counts
print("Counts of Death Classification Categories:")
print(paste("Low Risk:", low_count))
print(paste("Medium Risk:", medium_count))
print(paste("High Risk:", high_count))

# print(dataset$deaths_classification)
# cases_sel <- dataset %>% 
#   mutate(bad = as.factor(deaths_per_10000 > 10))

# Split data into training and testing sets based on specified states
cases_sel <- dataset %>% mutate(bad = as.factor(deaths_per_10000 > 10))
cases_sel %>% pull(bad) %>% table()
cases_sel %>% group_by(state) %>% 
  summarize(bad_pct = sum(bad == TRUE)/n()) %>%
  arrange(desc(bad_pct))

cases_train <- cases_sel %>% filter(state %in% c("TX", "CA", "FL", "NY","IL","LA"))
cases_train %>% pull(bad) %>% table()

cases_test <-  cases_sel %>% filter(!(state %in% c("TX", "CA", "FL", "NY","IL","LA")))
cases_test %>% pull(bad) %>% table()

counties <- as_tibble(map_data("county"))
counties <- counties %>% 
  rename(c(county = subregion, state = region)) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  select(state, county, long, lat, group)
counties  

library(stringr)

# Use the mutate function to process and adjust county names
counties_all <- counties %>% 
  left_join(cases_train %>%
              mutate(county = str_to_lower(county_name) %>% 
                       str_replace('\\s+county\\s*$', '')), by = "county")

ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

cases_train <- cases_sel %>%
  filter(state %in% c("TX", "CA", "FL", "NY", "IL", "LA")) %>%
  mutate(deaths_classification = ifelse(deaths_per_case > 0.02, "High",
                                        ifelse(deaths_per_case >= 0.01, "Medium", "Low")))

# Example to clean county names more thoroughly
library(stringr)

# Clean county names in both datasets before the join
cases_train <- cases_train %>%
  mutate(county = str_to_lower(county_name),
         county = str_remove(county, "\\s+county"),
         county = str_trim(county))

counties <- counties %>%
  mutate(county = str_to_lower(county),
         county = str_remove(county, "\\s+county"),
         county = str_trim(county))

# Join the datasets
counties_all <- counties %>%
  left_join(cases_train, by = c("county", "state"))


library(ggplot2)

ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = deaths_classification), color = "black", size = 0.1) +
  coord_quickmap() +
  scale_fill_manual(values = c('High' = 'red', 'Medium' = 'yellow', 'Low' = 'green'),
                    name = "COVID-19 Risk Level") +
  labs(title = "Map of Training Data: COVID-19 Risk Levels by County")


# Check how many entries have non-NA 'deaths_classification'
print(sum(!is.na(counties_all$deaths_classification)))

# Adjust your ggplot code to see unmatched counties
ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = deaths_classification), color = "black", size = 0.1) +
  scale_fill_manual(values = c('High' = 'red', 'Medium' = 'yellow', 'Low' = 'green', 'NA' = 'gray'),
                    name = "COVID-19 Risk Level", na.value = "lightgrey") +
  labs(title = "Map of Training Data: COVID-19 Risk Levels by County") +
  theme_minimal()


# If the number is very low, it indicates a problem with the join or data coverage
install.packages("FSelector")
library(FSelector)
cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head()

cases_train <- cases_train %>% select(-c(deaths_per_case))
cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head()

cases_train <- cases_train %>% select(-c(deaths,cases_per_10000))
cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head()

cases_trained <- cases_train %>% select(-c(deaths_classification))
cases_trained %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head()
# cases_train <- cases_train %>% select(-deaths_per_case, -cases_per_10000)
# 
# cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
#   arrange(desc(attr_importance)) %>% head(n = 10)
library(caret)
library(dplyr)
library(tidyverse)

# Assuming dataset has been loaded and cleaned up until the train-test split
# Let's assume the dataset loading and preliminary cleaning has been done as per your earlier steps

# Removing 'county' and 'state' columns for model training
dataset <- select(dataset, -c(county_name, state))

# Convert deaths_Classification to a factor if not already
dataset$deaths_classification <- as.factor(dataset$deaths_classification)

# Split data into training and testing sets
set.seed(123)  # for reproducibility
index <- createDataPartition(dataset$deaths_classification, p=0.80, list=FALSE)
trainData <- dataset[index, ]
testData <- dataset[-index, ]

nzv <- nearZeroVar(trainData)
trainData <- trainData[, -nzv]
testData <- testData[, -nzv]
# Data normalization

preProcValues <- preProcess(trainData, method = c("center", "scale"))
trainDataTransformed <- predict(preProcValues, trainData)
testDataTransformed <- predict(preProcValues, testData)


# Train the KNN model with hyperparameter tuning
set.seed(123)
# Adjust trainControl to use multiClassSummary
train_control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  classProbs = TRUE,  # Only set this if your model output needs probability scoring
  summaryFunction = multiClassSummary  # Adjusted for multi-class classification
)
install.packages("MLmetrics")
library(MLmetrics)
# Ensure caret and other necessary packages are also loaded
library(caret)
library(dplyr)

# Now retrain the model with the adjusted trainControl
knn_fit <- train(deaths_classification ~ ., data = trainDataTransformed, method = "knn",
                 tuneLength = 10,
                 trControl = train_control,
                 tuneGrid = expand.grid(k = 1:10))

# Output the results
print(knn_fit)

# Access the saved predictions
predictions <- knn_fit$pred

# Count predictions for each class
prediction_counts <- table(predictions$pred)

# Print the counts
print(prediction_counts)

# Check results

# Assuming knn_fit is your trained model object
results <- knn_fit$results
results <- results[, c("k", "Accuracy", "Kappa")]

# Print the table
print(results)

# Generate predictions using the trained KNN model
# Get all unique geo_id levels from both training and testing datasets
all_geo_ids <- unique(c(as.character(trainData$geo_id), as.character(testData$geo_id)))

# Re-factor geo_id in both training and testing datasets with the combined levels
trainData$geo_id <- factor(trainData$geo_id, levels = all_geo_ids)
testData$geo_id <- factor(testData$geo_id, levels = all_geo_ids)
# Apply the preProcess again if used initially
preProcValues <- preProcess(trainData, method = c("center", "scale"))
trainDataTransformed <- predict(preProcValues, trainData)
testDataTransformed <- predict(preProcValues, testData)
# Retrain the model
knn_fit <- train(deaths_classification ~ ., data = trainDataTransformed, method = "knn",
                 tuneLength = 10,
                 trControl = train_control,
                 tuneGrid = expand.grid(k = 1:10))

# Generate predictions on the transformed test data
testDataTransformed$risk_predicted_KNN <- predict(knn_fit, testDataTransformed, type = "raw")

# Optionally, inspect the predictions
print(head(testDataTransformed$risk_predicted_KNN))
# Calculate accuracy
accuracy <- sum(testDataTransformed$risk_predicted_KNN == testData$deaths_classification) / nrow(testDataTransformed)
print(paste("Accuracy:", accuracy))

# Generate and print a confusion matrix
confusionMatrix <- table(Predicted = testDataTransformed$risk_predicted_KNN, Actual = testData$deaths_classification)
print(confusionMatrix)

# Use caret to get more detailed performance metrics
library(caret)
confusionMatrix <- confusionMatrix(as.factor(testDataTransformed$risk_predicted_KNN), as.factor(testData$deaths_classification))
print(confusionMatrix)


print(names(confusionMatrix))



#ROC curve
library(caret)
# Predict probabilities
testDataTransformed$prob_positive <- predict(knn_fit, testDataTransformed, type = "prob")[,2]

roc_knn <- roc(testData$deaths_classification, testDataTransformed$prob_positive)
print(paste("AUC:", auc(roc_knn)))

plot(roc_knn, main="ROC Curve for KNN", col="#1c61b6", lwd=2)
abline(a=0, b=1, col="red", lty=2)



# Map related code but not working right now
# Load necessary libraries
library(ggplot2)
library(maps)
library(dplyr)

# Load US county and state map data
counties <- map_data("county")
us_states <- map_data("state")

# Prepare your testDataTransformed
# Make sure you have 'region' and 'state' columns in the correct format
# Here I assume 'county_name' and 'state' are your identifiers
testDataTransformed <- mutate(testDataTransformed, county_fips_code = tolower(as.character(county_fips_code)), state_fips_code = tolower(as.character(state_fips_code)))


map_data <- counties %>%
  left_join(testDataTransformed, by = c("county_fips_code", "state_fips_code"))


# Random Forest: Start------------------
if (!require("randomForest")) install.packages("randomForest")
if (!require("caret")) install.packages("caret")
if (!require("dplyr")) install.packages("dplyr")
library(randomForest)
library(caret)
library(dplyr)

# Data splitting
set.seed(123)
index <- createDataPartition(dataset$deaths_classification, p = 0.80, list = FALSE)
trainData_RF <- dataset[index, ]
testData_RF <- dataset[-index, ]

# Remove near zero variance predictors
nzv <- nearZeroVar(trainData_RF)
trainData_RF <- trainData_RF[, -nzv]
testData_RF <- testData_RF[, -nzv]

# Data normalization (optional for Random Forest)
preProcValues_RF <- preProcess(trainData_RF, method = c("center", "scale"))
trainDataTransformed_RF <- predict(preProcValues_RF, trainData_RF)
testDataTransformed_RF <- predict(preProcValues, testData_RF)

# Setup training control
train_control_RF <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  classProbs = TRUE,  # Important for ROC curve plotting
  summaryFunction = multiClassSummary
)

# Train the model
rf_fit <- train(deaths_classification ~ ., data = trainDataTransformed_RF, method = "rf",
                trControl = train_control_RF,
                ntree = 100)  # You can adjust the number of trees

# Print the model details
print(rf_fit)

# Make predictions
testDataTransformed_RF$predicted <- predict(rf_fit, newdata = testDataTransformed_RF)


predictions_RF <- rf_fit$pred
prediction_counts_RF <- table(predictions_RF$pred)
print(prediction_counts_RF)

if (!require("pROC")) install.packages("pROC")
library(pROC)
testDataTransformed_RF$probs <- predict(rf_fit, newdata = testDataTransformed_RF, type = "prob")
roc_curve_rf <- roc(response = testData_RF$deaths_classification, predictor = as.numeric(testDataTransformed_RF$probs[,2]))
aucRF <- auc(roc_curve_rf)
print(paste("AUC:", aucRF))

plot(roc_curve_rf, main = "ROC Curve for Random Forest Model", col = "#1c61b6", lwd = 2)
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)  # Line of no discrimination
  

# Calculate accuracy and print confusion matrix
accuracy_RF <- sum(testDataTransformed_RF$predicted == testData_RF$deaths_classification) / nrow(testDataTransformed_RF)
print(paste("Accuracy:", accuracy_RF))


confusionMatrix_RF <- table(Predicted = testDataTransformed_RF$predicted, Actual = testData_RF$deaths_classification)
print(confusionMatrix_RF)

confusionMatrixData_RF <- confusionMatrix(as.factor(testDataTransformed_RF$predicted), as.factor(testData_RF$deaths_classification))
print(confusionMatrixData_RF)



# Calculate ROC and AUC if classProbs = TRUE


# Random Forest: End------------------




# Support Vector Machine: Start -------------------------------
if (!require("e1071")) install.packages("e1071")
library(e1071)

# Data splitting
set.seed(123)
index <- createDataPartition(dataset$deaths_classification, p = 0.80, list = FALSE)
trainData_SVM <- dataset[index, ]
testData_SVM <- dataset[-index, ]

# Remove near zero variance predictors
nzv <- nearZeroVar(trainData_SVM)
trainData_SVM <- trainData_SVM[, -nzv]
testData_SVM <- testData_SVM[, -nzv]

# Data normalization (optional for SVM, but often recommended)
trainDataTransformed_SVM <- scale(trainData_SVM[, -which(names(trainData_SVM) == "deaths_classification")])
testDataTransformed_SVM <- scale(testData_SVM[, -which(names(testData_SVM) == "deaths_classification")])

# Train the SVM model
svm_fit <- svm(deaths_classification ~ ., data = trainData_SVM, kernel = "radial", probability = TRUE)

# Make predictions
testData_SVM$predicted <- predict(svm_fit, newdata = testData_SVM)

prediction_counts_SVM <- table(testData_SVM$predicted)
print(prediction_counts_SVM)


# Calculate accuracy and print confusion matrix
accuracy_SVM <- sum(testData_SVM$predicted == testData_SVM$deaths_classification) / nrow(testData_SVM)
print(paste("Accuracy:", accuracy_SVM))

confusionMatrix_SVM <- table(Predicted = testData_SVM$predicted, Actual = testData_SVM$deaths_classification)
print(confusionMatrix_SVM)

confusionMatrixData_SVM <- confusionMatrix(as.factor(testData_SVM$predicted), as.factor(testData_SVM$deaths_classification))
print(confusionMatrixData_SVM)

if (!require("pROC")) install.packages("pROC")
library(pROC)

# Compute probability estimates
testData_SVM$probs <- predict(svm_fit, newdata = testData_SVM, probability = TRUE)

# Extract probability estimates for the positive class
probs <- attr(predict(svm_fit, newdata = testData_SVM, probability = TRUE), "probabilities")

# Extract probability estimates for the positive class ("High")
probs_high <- as.numeric(probs[, "High"])

# Create a binary indicator for the response variable
response_binary <- ifelse(testData_SVM$deaths_classification == "High", 1, 0)

# Compute ROC curve
roc_curve_SVM <- roc(response = response_binary, predictor = probs_high)

# Plot ROC curve
plot(roc_curve_SVM, main = "ROC Curve for SVM Model", col = "#1c61b6", lwd = 2)
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)


roc_result_SVM = roc(response_binary, probs_high)
auc(roc_result_SVM)

if (!require(tigris)) install.packages("tigris")
if (!require(sf)) install.packages("sf")
library(tigris)
library(sf)

counties <- tigris::counties(cb = TRUE, class = "sf")  # cb = TRUE for coarse boundary (simpler shapes)

# Ensure FIPS codes are treated as characters to avoid leading zero issues
counties$GEOID <- as.character(counties$GEOID)
testData_SVM$county_fips_code <- sprintf("%05d", as.numeric(testData_SVM$county_fips_code))  # format to 5 digits

# Merge data
map_data <- merge(counties, testData_SVM, by.x = "GEOID", by.y = "county_fips_code")

library(ggplot2)
ggplot(data = map_data) +
  geom_sf(aes(fill = predicted), colour = "white", size = 0.1) +
  scale_fill_manual(values = c("low" = "green", "medium" = "yellow", "high" = "red")) +
  labs(title = "Predicted Risk Levels by County", fill = "Risk Level") 


# Support Vector Machine: End


# Comparision of three models ----------------------------------------------

# Comparing ROCs 

library(pROC)
library(ggplot2)

# Assuming you already have roc_obj_knn, roc_curve_svm, and rocCurve_rf calculated
# Let's plot them together
plot(roc_knn, print.auc = TRUE, main = "Comparison of Model ROC Curves", col = "#1c61b6")  # Blue for KNN
lines(roc_curve_SVM, print.auc = TRUE, col = "#FF5733")  # Red for SVM
lines(roc_curve_rf, print.auc = TRUE, col = "#33CFA5")  # Green for Random Forest


legend("bottomright", legend = c("KNN", "SVM", "Random Forest"), 
       col = c("#1c61b6", "#FF5733", "#33CFA5"), lwd = 2, cex = 0.8, bty = "n", bg = "white")


# comparing auc
auc_scores <- c(auc(roc_knn), auc(roc_curve_SVM), auc(roc_curve_rf))
barplot(auc_scores, names.arg = c("KNN", "SVM", "Random Forest"), 
        main = "AUC Scores Comparison", col = c("#1c61b6", "#FF5733", "#33CFA5"))



#confusion matrix visualization comparision

library(caret)
library(reshape2)
library(ggplot2)

# # Convert confusion matrices to data frames if they are not already
df_knn <- as.data.frame(confusionMatrix)
df_rf <- as.data.frame(confusionMatrix_RF)
df_svm <- as.data.frame(confusionMatrix_SVM)


# Add a model column to each
df_knn$model <- "KNN"
df_rf$model <- "Random Forest"
df_svm$model <- "SVM"
# 
# Combine into one data frame
combined_df <- rbind(df_knn, df_rf, df_svm)
print(combined_df)

library(ggplot2)

# Plot the data
library(ggplot2)

# Correct the column names in aes() to match your dataframe
ggplot(combined_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +  # use tiles to create the heatmap
  facet_wrap(~ model, scales = "free") +  # use the correct model variable
  scale_fill_gradient(low = "white", high = "red") +  # gradient fill for visual impact
  labs(title = "Confusion Matrix Comparison", x = "Actual Class", y = "Predicted Class") +
  theme_minimal() +  # minimal theme for cleaner visualization
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # adjust x-axis labels for better readability
