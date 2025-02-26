library(haven)
library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)

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

# Check the dimensions of the cleaned dataset
dim(data_cleaned)

# Define a threshold (e.g., remove rows with more than 50% missing values)
row_threshold <- 0.5 * ncol(data_cleaned)
data_cleaned <- data_cleaned[rowSums(is.na(data_cleaned)) < row_threshold, ]

# Check again
sum(is.na(data_cleaned))  # Total missing values after row removal

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
cor_matrix <- cor(data_cleaned, use = "complete.obs", method = "pearson")

# Calculate the standard deviation for each variable
sds <- apply(data_cleaned, 2, sd)

# Identify columns with zero standard deviation (constant variables)
constant_vars <- names(sds[sds == 0])
print(constant_vars)

# Remove the one column that has a SD of 0 because it doesn't offer anything to the data 
data_cleaned <- data_cleaned[, !(names(data_cleaned) %in% c("usenet"))]

# Calculate the correlation matrix (using complete cases only)
cor_matrix <- cor(data_cleaned, use = "complete.obs", method = "pearson")

# Set a threshold for high correlation (e.g., greater than 0.9)
threshold <- 0.9

# Extract the upper triangle (excluding the diagonal)
upper_tri <- upper.tri(cor_matrix)

# Get the correlation values from the upper triangle
high_cor_values <- cor_matrix[upper_tri]

# Filter out values greater than the threshold
high_cor_pairs <- high_cor_values[abs(high_cor_values) > threshold]

# Count the number of high correlation pairs
num_high_cor_pairs <- length(high_cor_pairs)
print(num_high_cor_pairs)

# Proxy binary variable creation 
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


# Training a decision tree
set.seed(123)  # For reproducibility
sample_index <- sample(1:nrow(data_cleaned), size = 0.7 * nrow(data_cleaned))
train_data <- data_cleaned[sample_index, ]
test_data <- data_cleaned[-sample_index, ]


selected_columns <- c(
  grep("^(go1)", names(data_cleaned), value = TRUE),
  c("impinftv", "impinfrad", "impinfine", "impenttv", "impentrad", 
    "impentine", "reltv", "relrad", "relonews", "relsclm")
)

# Subset the data to include only selected columns and the target variable
data_subset <- data_cleaned[, c(selected_columns, "AI_aversion")]


# Train the decision tree model
library(rpart)
model <- rpart(AI_aversion ~ ., data = data_subset, method = "class")

# View the model summary
summary(model)

library(rpart.plot)
rpart.plot(model)

# Predict on test data
predictions <- predict(model, newdata = test_data, type = "class")

# Create confusion matrix
table(Predicted = predictions, Actual = test_data$AI_aversion)

# Calculate accuracy
accuracy <- sum(predictions == test_data$AI_aversion) / length(predictions)
print(paste("Accuracy:", accuracy))

library(pROC)

# Predict probabilities for ROC curve
prob_predictions <- predict(model, newdata = test_data, type = "prob")

# Compute ROC curve
roc_curve <- roc(test_data$AI_aversion, prob_predictions[, 2])
plot(roc_curve)


# Confusion matrix for training data
train_preds <- predict(model, newdata = train_data, type = "class")
table(train_data$AI_aversion, train_preds)

# Confusion matrix for test data
test_preds <- predict(model, newdata = test_data, type = "class")
table(test_data$AI_aversion, test_preds)

library(caret)

# Set up cross-validation
train_control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

# Train the model using cross-validation
cv_model <- train(AI_aversion ~ ., data = train_data, method = "rpart", trControl = train_control)

# Check the results
print(cv_model)

