#Installing required libraries
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("caret")
install.packages("readr")
install.packages("pROC")
install.packages("plotrix")
install.packages("corrplot")
install.packages("e1071")
install.packages("class")
install.packages("ROCR")
install.packages("Hmisc")
install.packages("rpart")
install.packages("smotefamily")
install.packages("rpart.plot")

#Required libraries are imported
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(readr)
library(pROC)
library(plotrix)
library(corrplot)
library(e1071)  
library(class)
library(ROCR)
library(Hmisc)
library(rpart)
library(smotefamily)
library(rpart.plot)

#Bank Marketing dataset is read into a variable loan_data
bank_data <- read.csv('bank-direct-marketing-campaigns.csv')

#first 6 rows are printed
print(head(bank_data))

#summary is printed 
print(summary(bank_data))

#Null values are checked
print(colSums(is.na(bank_data)))

# VISUALIZATION
# Target Variable
bank_data$y <- as.factor(bank_data$y)  # Converting to factor

ggplot(bank_data, aes(x = y, fill = y)) + 
  geom_bar() +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-0.5) +  # Updated count notation
  scale_fill_manual(values = c("skyblue", "salmon")) +
  labs(title = 'Distribution of Target Variable (y)', 
       x = 'Subscription to Term Deposit', 
       y = 'Count') +
  theme_minimal()


# histogram: Effect of Age on Subscription to Term Deposit
ggplot(bank_data, aes(x = age, fill = y)) + 
  geom_histogram(position = "identity", alpha = 0.75, bins = 20, aes(y = ..count..)) +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  labs(title = 'Effect of Age on Subscription to Term Deposit', 
       x = 'Age', 
       y = 'Count') +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))

# Calculating counts for each combination of job and subscription status
job_counts <- bank_data %>%
  group_by(job, y) %>%
  summarise(Count = n(), .groups = 'drop')

# Creating a bar plot
ggplot(job_counts, aes(x = job, y = Count, fill = y)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  labs(title = 'Effect of Job Type on Subscription to Term Deposit', 
       x = 'Job Type', 
       y = 'Count') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(reverse = TRUE))  # Adjust legend

# Checking for 'unknown' values in each column
unknown_values <- sapply(bank_data, function(x) sum(x == 'unknown'))

# Filtering to show only columns with 'unknown' values greater than 0
unknown_values[unknown_values > 0]


# Creating a bar plot
ggplot(bank_data, aes(x = default, fill = default)) + 
  geom_bar() +
  labs(title = 'Distribution of Default Variable', 
       x = 'Default', 
       y = 'Count') +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE))

# Dropping the 'default' column
bank_data_updated <- bank_data[, !(names(bank_data) %in% 'default')]


# Using the unmodified version
bank_data_clean <- bank_data_updated

# Getting the dimensions of the original and cleaned data frames
original_shape <- dim(bank_data)
cleaned_shape <- dim(bank_data_clean)

# shapes
original_shape
cleaned_shape

# Encoding categorical labels
categorical_columns <- sapply(bank_data_clean, is.character)
bank_data_clean[categorical_columns] <- lapply(bank_data_clean[categorical_columns], factor)

# Converting factors to integer values
bank_data_clean[categorical_columns] <- lapply(bank_data_clean[categorical_columns], function(x) as.integer(as.factor(x)))
bank_data <- bank_data_clean
str(bank_data)


# Converting 'y' to numeric (0 for "no", 1 for "yes")
bank_data$y <- as.numeric(bank_data$y) - 1


# Calculating the correlation matrix 
correlation_result <- rcorr(as.matrix(bank_data))
correlation_matrix <- correlation_result$r

# Plotting the correlation matrix 
corrplot(correlation_matrix, method = "number", col = colorRampPalette(c("blue", "red"))(100), 
         type = "full", tl.col = "black", tl.srt = 45, tl.cex = 1.0,
         addCoef.col = "black", number.cex = 0.5,  # Increase the size here
         mar = c(0, 0, 1, 0)) 


# Setting x-axis and y-axis labels (feature names)
colnames(correlation_matrix) <- rownames(correlation_matrix) <- colnames(bank_data)

# Checking the correlation of all columns with 'y'
correlation_matrix <- cor(bank_data, use = "complete.obs")
correlation_y <- correlation_matrix[, 'y']

# Sorting the absolute correlation values in descending order
sorted_correlation <- sort(abs(correlation_y), decreasing = TRUE)

# Creating a data frame
correlation_table <- data.frame(Feature = names(sorted_correlation), Correlation = sorted_correlation)

# Printing the data frame
print(correlation_table)

# Dropping columns month, day_of_week, job, housing, loan
bank_data <- bank_data %>% select(-month, -day_of_week, -job, -housing, -loan)
str(bank_data)

# Separating features and target variable
features <- bank_data[, names(bank_data) != "y"]
target <- bank_data$y

# Normalizing features using Min-Max scaling
preprocess_params <- preProcess(features, method = c("range"))
normalized_features <- predict(preprocess_params, features)

# Combining the normalized features with the target variable
normalized_bank_data <- cbind(normalized_features, y = target)
head(normalized_bank_data)


# Splitting the normalized data into train and test sets
set.seed(22200878) 
trainIndex <- createDataPartition(normalized_bank_data$y, p = .8, list = FALSE, times = 1)
train_data <- normalized_bank_data[trainIndex,]
test_data <- normalized_bank_data[-trainIndex,]

# Separating features and target for train and test sets
X_train <- train_data[, names(train_data) != "y"]
y_train <- train_data$y
X_test <- test_data[, names(test_data) != "y"]
y_test <- test_data$y

head(X_train)
str(X_train)
head(X_test)
str(X_test)


y_train <- factor(y_train, levels = c(0, 1))

# Training Decision Tree model
dt_model <- rpart(y ~ ., data = train_data, method = "class")

# Predictions on test data
dt_predictions <- predict(dt_model, X_test, type = "class")

# Defining evaluate model function
evaluate_model <- function(y_true, y_pred, model_name) {
  # Converting y_true and y_pred to factors with the same levels
  levels_union <- union(levels(factor(y_true)), levels(factor(y_pred)))
  y_true <- factor(y_true, levels = levels_union)
  y_pred <- factor(y_pred, levels = levels_union)
  
  # Metrics calculation
  accuracy <- confusionMatrix(y_pred, y_true)$overall["Accuracy"]
  precision <- confusionMatrix(y_pred, y_true)$byClass["Pos Pred Value"]
  recall <- confusionMatrix(y_pred, y_true)$byClass["Sensitivity"]
  f1 <- confusionMatrix(y_pred, y_true)$byClass["F1"]
  
  # Converting y_pred to numeric for ROC analysis
  y_pred_numeric <- as.numeric(y_pred) - 1  # Convert to 0 and 1
  
  # ROC analysis
  roc_result <- roc(response = y_true, predictor = y_pred_numeric)
  roc_auc <- roc_result$auc
  
  # Printing the metrics
  cat(paste("Metrics for", model_name, ":\n"))
  cat(paste("Accuracy:", round(accuracy, 4), "\n"))
  cat(paste("Precision:", round(precision, 4), "\n"))
  cat(paste("Recall:", round(recall, 4), "\n"))
  cat(paste("F1 Score:", round(f1, 4), "\n"))
  cat(paste("ROC AUC Score:", round(roc_auc, 4), "\n"))
  
  # Printing confusion matrix
  confusion <- confusionMatrix(y_pred, y_true)
  cat("Confusion Matrix:\n")
  print(confusion)
}

# Evaluating Decision Tree model
evaluate_model(y_test, dt_predictions, "Decision Tree")

# Applying SMOTE
smote_result <- SMOTE(X_train, y_train, K = 5)

# Checking the structure of the returned value from SMOTE
str(smote_result)

# Extracting the combined data
combined_data <- smote_result$data

# Checking the structure of combined_data
str(combined_data)

# Splitting into features and target
X_train_smote <- combined_data[, !(names(combined_data) %in% "class")]
y_train_smote <- combined_data$class

# Converting y_train_smote to a factor or numeric as needed
y_train_smote <- as.factor(y_train_smote)

# Checking dimensions and class distribution
dim(X_train_smote)
table(y_train_smote)

# Training a decision tree model on SMOTE data
dt_model_smote <- rpart(y_train_smote ~ ., data = data.frame(X_train_smote, y_train_smote), method = "class")

# Predictions on the original test data
dt_predictions_smote <- predict(dt_model_smote, X_test, type = "class")

# Evaluating Decision Tree model after SMOTE
evaluate_model(y_test, dt_predictions_smote, "Decision Tree (SMOTE)")


# Visualizing the decision tree
rpart.plot(dt_model_smote, type = 4, extra = 102)

# Feature Importance for Decision Tree
importance <- as.data.frame(dt_model_smote$variable.importance)
names(importance) <- c("Importance")
print("Decision Tree Feature Importances:")
print(importance)

# Converting prediction of dt model to numeric with level 0 and 1
dt_predictions_numeric <- as.numeric(factor(dt_predictions_smote, levels = c("0", "1")))

# Converting y_test (actual) of dt model to numeric with levels 0 and 1
y_test_numeric <- as.numeric(factor(y_test, levels = c("0", "1")))

# Creating ROC curve for SVM
roc_dt <- roc(response = y_test_numeric, predictor = dt_predictions_numeric)

# Plotting ROC curve for SVM
plot(roc_dt, col = "purple", main = "ROC Curves", col.main = "black", lwd = 2)
