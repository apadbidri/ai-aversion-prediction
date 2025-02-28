# Load libraries 

library(haven)
library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)
library(naniar)
library(VIM)



# Load dataset
data <- read_dta("oxis2019ukda.dta")

# Look at structure - apply numeric conversions
data[] <- lapply(data, function(x) as.numeric(as.character(x)))
str(data)

# Sum of NAs
sum(is.na(data))

# Remove columns with too much missing data 
# Calculate the threshold (50% of total rows)
threshold <- 0.5 * nrow(data)

# Identify columns where the number of NAs exceeds the threshold
cols_to_keep <- colSums(is.na(data)) < threshold

# Subset the dataset to keep only those columns
data_cleaned <- data[, cols_to_keep]

# Check again
sum(is.na(data_cleaned))

# Assessing missingness
mcar_test(data_cleaned)
aggr(data_cleaned, col = c("navy", "yellow"), numbers = TRUE, sortVars = TRUE, labels = names(data_cleaned), cex.axis = 0.7, gap = 3, ylab = c("Missing data", "Pattern"))


# Address NA with median since knn didn't work 
numeric_cols <- sapply(data_cleaned, is.numeric)
# Impute missing values with median for numeric columns
for (col in names(data_cleaned)[numeric_cols]) {
  # Calculate the median of the column (excluding NA)
  median_value <- median(data_cleaned[[col]], na.rm = TRUE)
  
  # Replace NA values with the median
  data_cleaned[[col]][is.na(data_cleaned[[col]])] <- median_value
}

sum(is.na(data_cleaned))  # Should be 0 if all NAs are imputed


# Exploratory data analysis
# cor_matrix <- cor(data_cleaned, use = "complete.obs", method = "pearson")

# Calculate the standard deviation for each variable
#sds <- apply(data_cleaned, 2, sd)

# Identify columns with zero standard deviation (constant variables)
#constant_vars <- names(sds[sds == 0])
#print(constant_vars)

# Remove the one column that has a SD of 0 because it doesn't offer anything to the data 
#data_cleaned <- data_cleaned[, !(names(data_cleaned) %in% c("usenet"))]

# Calculate the correlation matrix (using complete cases only)
cor_matrix <- cor(data_cleaned, use = "complete.obs", method = "pearson")

# Set a threshold for high correlation (e.g., greater than 0.9)
threshold <- 0.9

# Extract the upper triangle (excluding the diagonal)
#upper_tri <- upper.tri(cor_matrix)

# Get the correlation values from the upper triangle
#high_cor_values <- cor_matrix[upper_tri]

# Filter out values greater than the threshold
high_cor_pairs <- high_cor_values[abs(high_cor_values) > threshold]

# Count the number of high correlation pairs
num_high_cor_pairs <- length(high_cor_pairs)
print(num_high_cor_pairs)

# Proxy binary variable creation 

# Reverse code AI averse questions 

# List of columns you want to reverse code
# columns_to_reverse <- c("impinfnews", "relpnews", "agcred", "agpriv", "agtdate", "agpdata", "column3", "column3", "column3", "column3", "column3", "column3", "column3", "column3", "column3", "column3")  # Replace with actual column names

# Reverse code each column
#data_cleaned[columns_to_reverse] <- lapply(data_cleaned[columns_to_reverse], function(x) 6 - x)

# Sum up columns
total_columns <- grep("^(go1)", names(data_cleaned), value = TRUE)
online_behavior_columns <- c("impinftv", "impinfrad", "impinfine", "impenttv","impentrad", "impentine", "reltv", "relrad", "relonews", "relsclm")

combined_columns <- c(grep("^(go1)", names(data_cleaned), value = TRUE), 
                      c("impinftv", "impinfrad", "impinfine", "impenttv", "impentrad", 
                        "impentine", "reltv", "relrad", "relonews", "relsclm"))

print(combined_columns)

data_cleaned$total_online_attitude <- rowSums(data_cleaned[combined_columns], na.rm = TRUE)

# Calculate the quartiles
quantiles <- quantile(data_cleaned$total_online_attitude, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Classify based on quartiles
data_cleaned$AI_aversion <- ifelse(data_cleaned$total_online_attitude <= quantiles[1], "1", # where 1 = averse and 0 = non-averse
                         ifelse(data_cleaned$total_online_attitude <= quantiles[2], "1", "0"))













# Load necessary libraries
library(rpart)
library(rpart.plot)
library(caret)
library(pROC)

set.seed(123)  # For reproducibility

# Split data into training (70%) and testing (30%) sets
sample_index <- sample(1:nrow(data_cleaned), size = 0.7 * nrow(data_cleaned))
train_data <- data_cleaned[sample_index, ]
test_data <- data_cleaned[-sample_index, ]

# Select relevant columns for modeling
selected_columns <- c(
  grep("^(go1)", names(data_cleaned), value = TRUE),
  c("impinftv", "impinfrad", "impinfine", "impenttv", "impentrad", 
    "impentine", "reltv", "relrad", "relonews", "relsclm")
)

# Subset data to include selected features and target variable
data_subset <- data_cleaned[, c(selected_columns, "AI_aversion")]

# Train the decision tree model
model <- rpart(AI_aversion ~ ., data = data_subset, method = "class")

# View model summary
summary(model)

# Plot the decision tree
rpart.plot(model)

# Feature importance using caret
importance <- varImp(model, scale = FALSE)
print(importance)  # Print importance scores

# Optional: Plot feature importance
plot(importance, main = "Feature Importance for Decision Tree")

# Predict on test data
predictions <- predict(model, newdata = test_data, type = "class")

# Confusion matrix
conf_matrix <- table(Predicted = predictions, Actual = test_data$AI_aversion)
print(conf_matrix)

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", round(accuracy, 3)))

# Compute ROC curve
prob_predictions <- predict(model, newdata = test_data, type = "prob")
roc_curve <- roc(test_data$AI_aversion, prob_predictions[, 2])
plot(roc_curve, main = "ROC Curve for Decision Tree")

# Cross-validation setup
train_control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Train the model using cross-validation
cv_model <- train(AI_aversion ~ ., data = train_data, method = "rpart", trControl = train_control)

# Print cross-validation results
print(cv_model)