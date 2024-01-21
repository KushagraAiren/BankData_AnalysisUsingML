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
install.packages("smotefamily")

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
library(smotefamily)

#Bank Churn dataset is read into a variable loan_data
churn_data <- read.csv('Bank Customer Churn Prediction.csv')

#first 6 rows are printed
print(head(churn_data))

#summary is printed 
print(summary(churn_data))

#Null values are checked
print(colSums(is.na(churn_data)))

#number of records, variablkes and there data types are printed
print(str(churn_data))

# Checking if all values in the 'churn' column are numeric
all_numeric <- all(suppressWarnings(!is.na(as.numeric(churn_data$churn))))

# Printing the result
if (all_numeric) {
  cat("All values in the 'churn' column are numeric.\n")
} else {
  cat("There are non-numeric values in the 'churn' column.\n")
}

# Identifying Numerical Columns
numerical_variables <- names(churn_data)[sapply(churn_data, is.numeric)]
numerical_variables <- setdiff(numerical_variables, "churn")

# Identifying Categorical Columns
categorical_variables <- names(churn_data)[sapply(churn_data, function(x) !is.numeric(x))]

# Printing the Results
cat("Numerical Features:", numerical_variables, "\n")
cat("Categorical Features:", categorical_variables, "\n")

# Calculating class counts
class_counts <- table(churn_data$churn)

# Calculating percentages
percentages <- round(prop.table(class_counts) * 100, 1)

# Creating labels with category names and percentages
labels <- paste(c('not churned', 'churned'), "\n", percentages, "%")

# Creating a pie chart with explode and percentage labels
pie3D(class_counts, labels = labels, explode = 0.2, 
      main = 'No. of Customers Churned v/s Not Churned',
      col = c('#FF9999', '#66B2FF'), 
      cex = 0.8, 
      theta = 1)


numeric_variables <- sapply(churn_data, is.numeric)
numeric_data <- churn_data[, numeric_variables]


# Calculating the correlation matrix and p-values
correlation_result <- rcorr(as.matrix(numeric_data))

# Extracting the correlation matrix from the result
correlation_matrix <- correlation_result$r
par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
corrplot(correlation_matrix, method = "number", col = colorRampPalette(c("blue", "red"))(100), 
         type = "full", tl.col = "black", tl.srt = 45, tl.cex = 1.0, # Increase text size
         addCoef.col = "black", number.cex = 0.5,  # Increase number size
         mar = c(0, 0, 1, 0)) 

# Setting x-axis and y-axis labels
colnames(correlation_matrix) <- rownames(correlation_matrix) <- colnames(numeric_data)

correlation_churn <- correlation_matrix[, 'churn']

# Sorting the absolute correlation values in descending order
sorted_correlation <- sort(abs(correlation_churn), decreasing = TRUE)

# Creating a data frame
correlation_table <- data.frame(Feature = names(sorted_correlation), Correlation = sorted_correlation)

# Printing the data frame
print(correlation_table)

# Defining legend labels and colors
legend_labels <- c("No", "Yes")
colors <- c("red", "blue")

# Creaing a list of plots
plots <- lapply(numerical_variables, function(col) {
  ggplot(churn_data, aes(x = !!as.name(col), fill = factor(churn))) +
    geom_histogram(color = "black", bins = 30, alpha = 1, position = "identity") +
    scale_fill_manual(values = colors, labels = legend_labels) +
    labs(title = paste("Distribution of", col),
         x = col,
         y = "Count",
         fill = "Churn") +
    theme(legend.position = "top")
})

plots

# Creating a data frame with tenure_churn_count
tenure_churn_count <- churn_data %>%
  group_by(tenure, churn) %>%
  summarise(count = n()) %>%
  filter(churn == 1)

# Plotting the line chart
ggplot(tenure_churn_count, aes(x = tenure, y = count)) +
  geom_line() +
  labs(x = 'Tenure', y = 'Churn Frequency', title = 'Churn Frequency Over Tenure') +
  theme_minimal()

# Visualizing relation between churn and gender and churn and country
for (col in categorical_variables) {
 
  p <- ggplot(churn_data, aes(x = factor(churn), fill = factor(churn_data[[col]]))) +
    geom_bar(position = 'dodge') +
    scale_x_discrete(labels = c('No', 'Yes')) +
    labs(title = paste("Relation between churn and", col),
         x = 'Churn',
         y = 'Frequency') +
    theme_minimal()
  
  # Displaying the plot
  print(p)
}

# Performing one-hot encoding for 'country'
country_encoded <- model.matrix(~ 0 + country, data = churn_data)
country_names <- colnames(country_encoded)
churn_data <- cbind(churn_data, country_encoded)

# Performing one-hot encoding for 'gender'
gender_encoded <- model.matrix(~ 0 + gender, data = churn_data)
gender_names <- colnames(gender_encoded)
churn_data <- cbind(churn_data, gender_encoded)

# Removing the original categorical columns from the dataset
churn_data <- churn_data[, !(names(churn_data) %in% c('country', 'gender'))]
str(churn_data)

# Checking the correlation again of all columns with 'churn'
correlation_matrix <- cor(churn_data, use = "complete.obs")
correlation_churn <- correlation_matrix[, 'churn']

# Sorting the absolute correlation values in descending order
sorted_correlation <- sort(abs(correlation_churn), decreasing = TRUE)

# Creating a data frame
correlation_table <- data.frame(Feature = names(sorted_correlation), Correlation = sorted_correlation)

# Printing the data frame
print(correlation_table)

# dropping customer_id and credit card column based on their weak correlation with churn
churn_data <- churn_data[, !names(churn_data) %in% c('customer_id', 'credit_card')]
str(churn_data)

# normalizing the dataset except target variable
# Separating features and target variable
features <- churn_data[, names(churn_data) != "churn"]
target <- churn_data$churn

# Normalizing features using Min-Max scaling
preprocess_params <- preProcess(features, method = c("range"))
normalized_features <- predict(preprocess_params, features)

# Combining the normalized features with the target variable
normalized_churn_data <- cbind(normalized_features, churn = target)
head(normalized_churn_data)

# Splitting the dataset into training and testing sets
set.seed(123) 
train_index <- createDataPartition(normalized_churn_data$churn, p = 0.8, list = FALSE)
train_data <- normalized_churn_data[train_index, ]
test_data <- normalized_churn_data[-train_index, ]

# Separating features (X) and target variable (Y) for training
X_train <- train_data[, -which(names(train_data) == "churn")]
Y_train <- train_data$churn

# Separating features (X) and target variable (Y) for testing
X_test <- test_data[, -which(names(test_data) == "churn")]
Y_test <- test_data$churn

# Displaying the dimensions of the training and testing sets to verify the split
print(paste("X_train shape:", dim(X_train)[1], "rows and", dim(X_train)[2], "columns"))
print(paste("X_test shape:", dim(X_test)[1], "rows and", dim(X_test)[2], "columns"))
print(paste("Y_train shape:", length(Y_train)))
print(paste("Y_test shape:", length(Y_test)))

# Training SVM model
svm_model <- svm(churn ~ ., data = train_data, type = 'C-classification', kernel = 'linear')

# Predictions on test data
svm_predictions <- predict(svm_model, X_test)

# Training KNN model
k <- 5  # You need to specify the number of neighbors
knn_predictions <- knn(train = X_train, test = X_test, cl = Y_train, k = k)


# Defining evaluate model function
evaluate_model <- function(y_true, y_pred, model_name) {
  # Convert y_true and y_pred to factors with the same levels
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

# Evaluating SVM model
evaluate_model(Y_test, svm_predictions, "SVM")

# Evaluating KNN model
evaluate_model(Y_test, knn_predictions, "KNN")

# Applying SMOTE
smote_data <- SMOTE(X_train, Y_train, K = 5)

# Checking the structure of the returned value from SMOTE
str(smote_data)

# Extracting the combined data
combined_data <- smote_data$data

# Checking the structure of combined_data
str(combined_data)

# Splitting into features and target
X_train_smote <- combined_data[, !(names(combined_data) %in% "class")]
Y_train_smote <- combined_data$class

# Converting Y_train_smote to a factor or numeric as needed
Y_train_smote <- as.numeric(Y_train_smote)

# Checking dimensions and class distribution
dim(X_train_smote)
table(Y_train_smote)

# Training SVM model
svm_model_smote <- svm(x = X_train_smote, y = Y_train_smote,
                       type = "C-classification", kernel = "radial")

# Predictions on test data
svm_predictions_smote <- predict(svm_model_smote, newdata = X_test)
svm_predictions_numeric <- as.numeric(factor(svm_predictions_smote, levels = c("0", "1")))


# Training KNN model on SMOTE data
k <- 5  # Specify the number of neighbors
library(class)
knn_model_smote <- knn(train = X_train_smote, test = X_test, cl = Y_train_smote, k = k)

# Checking for NA values in knn_predictions_smote
if (any(is.na(knn_model_smote))) {
  stop("The KNN model generated NA values in predictions. Check your data and model parameters.")
}

# Converting Y_test to numeric with levels 0 and 1
Y_test_numeric <- as.numeric(factor(Y_test, levels = c("0", "1")))

# Converting knn_predictions_smote to numeric with levels 0 and 1
knn_predictions_numeric <- as.numeric(factor(knn_model_smote, levels = c("0", "1")))

# Evaluating SVM model on SMOTE data
evaluate_model(Y_test, svm_predictions_smote, "SVM with SMOTE")

# Evaluating KNN model on SMOTE data
evaluate_model(Y_test_numeric, knn_predictions_numeric, "KNN with SMOTE")


# Create ROC curve for KNN
roc_knn <- roc(response = Y_test_numeric, predictor = knn_predictions_numeric)


# Creating ROC curve for SVM
roc_svm <- roc(response = Y_test_numeric, predictor = svm_predictions_numeric)

# Plotting ROC curve for SVM
plot(roc_svm, col = "purple", main = "ROC Curves", col.main = "black", lwd = 2)

# Adding ROC curve for KNN to the same plot
lines(roc_knn, col = "green", lwd = 2)

# Adding a legend
legend("bottomright", legend = c("SVM", "KNN"), col = c("purple", "green"), lwd = 2)

