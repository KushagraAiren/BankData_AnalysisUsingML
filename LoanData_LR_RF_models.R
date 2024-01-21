#Installing required libraries
install.packages(c("dplyr", "ggplot2", "tidyr", "caret", "readr", "randomForest", "pROC", "ROCR", "Hmisc", "corrplot", "smotefamily"))


#Required libraries are imported
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(readr)
library(randomForest)
library(pROC)
library(ROCR)
library(Hmisc)
library(corrplot)
library(smotefamily)

#Loan dataset is read into a variable loan_data
loan_data <- read.csv('loan_data.csv')

#first 6 rows are printed
print(head(loan_data))

#summary is printed 
print(summary(loan_data))

#Null values are checked
print(colSums(is.na(loan_data)))

#number of records, variablkes and there data types are printed
print(str(loan_data))

#credit.policy and not.fully.paid variables are converted from integer to logical type
loan_data$credit.policy <- as.logical(loan_data$credit.policy)
loan_data$not.fully.paid <- as.logical(loan_data$not.fully.paid)

(str(loan_data))

#the uniques values of variables inq.last.6mths, delinq.2yrs and pub.rec are grouped and converted into percentage and then sorted into descending order.
columns_to_analyze <- c('inq.last.6mths', 'delinq.2yrs', 'pub.rec')
for (column in columns_to_analyze) {
  # Get unique values and their counts
  unique_values <- unique(loan_data[[column]])
  value_counts <- table(loan_data[[column]])
  
  # Calculating percentage per value
  percentage_per_value <- (value_counts / nrow(loan_data)) * 100
  
  # Sorting the values in descending order by their respective percentage
  sorted_values <- percentage_per_value[order(-percentage_per_value)]
  
  cat(paste("Unique values for '", column, "':\n", sep = ""))
  for (i in seq_along(sorted_values)) {
    cat(paste(names(sorted_values)[i], ": ", sprintf("%.2f%%", sorted_values[i]), "\n"))
  }
  cat("\n")
}

# custom binning method will group the percentage values into the respective bins for 'inq.last.6mths'
custom_binning <- function(value) {
  if (value == 0) {
    return('bin1')
  } else if (value %in% c(1, 2)) {
    return('bin2')
  } else if (value %in% c(3, 4, 5)) {
    return('bin3')
  } else {
    return('bin4')
  }
}

# Applying the custom binning method to variable 'inq.last.6mths' and creating a new column bin_inq
loan_data$bin_inq <- sapply(loan_data$inq.last.6mths, custom_binning)

# Calculating the distribution of the new labels with their percentages
label_distribution <- table(loan_data$bin_inq) / nrow(loan_data) * 100

# Sorting the values in descending order by percentage
sorted_labels <- label_distribution[order(-label_distribution)]

cat("Distribution of New Labels:\n")
for (label in names(sorted_labels)) {
  cat(sprintf("%s: %.2f%%\n", label, sorted_labels[label]))
}

# custom binning method will group the percentage values into the respective bins for 'delinq.2yrs'
custom_binning_delinq <- function(value) {
  if (value == 0) {
    return('bin1')
  } else if (value == 1) {
    return('bin2')
  } else {
    return('bin3')
  }
}
# Applying the custom binning method to variable 'inq.last.6mths' and creating a new column bin_delinq
loan_data$bin_delinq <- sapply(loan_data$delinq.2yrs, custom_binning_delinq)

# Calculating the distribution of the new labels with their percentages
delinq_label_distribution <- table(loan_data$bin_delinq) / nrow(loan_data) * 100

# Sorting the values in descending order by percentage
sorted_delinq_labels <- delinq_label_distribution[order(-delinq_label_distribution)]

cat("Distribution of New Labels for 'delinq.2yrs':\n")
for (label in names(sorted_delinq_labels)) {
  cat(sprintf("%s: %.2f%%\n", label, sorted_delinq_labels[label]))
}

# custom binning method will group the percentage values into the respective bins for 'pub.rec'
custom_binning_pubrec <- function(value) {
  if (value == 0) {
    return('bin1')
  } else {
    return('bin2')
  }
}

# Applying the custom binning method to variable 'inq.last.6mths' and creating a new column bin_pub_rec
loan_data$bin_pub_rec <- sapply(loan_data$pub.rec, custom_binning_pubrec)

# Calculating the distribution of the new labels with their percentages
pub_rec_label_distribution <- table(loan_data$pub_rec_bin) / nrow(loan_data) * 100

# Sorting the values in descending order by percentage
sorted_pub_rec_labels <- pub_rec_label_distribution[order(-pub_rec_label_distribution)]

cat("Distribution of New Labels for 'pub.rec':\n")
for (label in names(sorted_pub_rec_labels)) {
  cat(sprintf("%s: %.2f%%\n", label, sorted_pub_rec_labels[label]))
}

#validating the changes done on 'bin_inq', 'bin_delinq' and 'bin_pub_rec'
print(str(loan_data))

# Visualizations
# Filtering categorical features to plot
categorical_features <- c('purpose', 'credit.policy', 'not.fully.paid', 'bin_inq', 'bin_delinq', 'bin_pub_rec')

# Creating bar charts for each categorical feature
for (feature in categorical_features) {
  feature_counts <- table(loan_data[[feature]])
  
  # Creating a data frame for plotting
  plot_data <- data.frame(value = names(feature_counts), count = as.numeric(feature_counts))
  
  # Creating the bar plot
  p <- ggplot(plot_data, aes(x = value, y = count, fill = value)) +
    geom_bar(stat = "identity") +
    labs(title = paste0(feature, " Distribution"), x = feature, y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

# Filtering records where revol.util is greater than 100
high_utilization_records <- subset(loan_data, revol.util > 100)

# Printing the filtered records
cat("Records with 'revol.util' > 100%:\n")
print(high_utilization_records)

high_utilization_indices <- which(loan_data$revol.util > 100)

# Updating the filtered values to 100
loan_data$revol.util[high_utilization_indices] <- 100

# Printing the updated records
updated_records <- loan_data[high_utilization_indices, ]
cat("Updated Records with 'revol.util' = 100%:\n")
print(updated_records)

# Selectung numerical columns from the data set
numerical_columns <- loan_data[, sapply(loan_data, is.numeric)]


grid_layout <- matrix(1:length(numerical_columns), nrow = length(numerical_columns), byrow = TRUE)
layout(mat = grid_layout, heights = rep(1, length(numerical_columns)))

# Creating histograms for each numerical feature
for (i in 1:length(numerical_columns)) {
  col <- names(numerical_columns)[i]
  
  
  p <- ggplot(loan_data, aes(x = .data[[col]])) +
    geom_histogram(binwidth = 30, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste0("Histogram of ", col), x = col, y = "Frequency") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  print(p)
}


# Applying a logarithmic transformation to revol.bal
loan_data$revol_bal_log <- log1p(loan_data$revol.bal)

# printing the first 6 rows of the dataset
cat(" DataFrame:\n")
print(head(loan_data))

# Selecting numerical columns from the data set
numerical_columns <- loan_data[, sapply(loan_data, is.numeric)]

# Calculating the correlation matrix and p-values
correlation_result <- rcorr(as.matrix(numerical_columns))

# Printing the correlation matrix
cat("\n Correlation Matrix:\n")
print(correlation_result)

# printing the p-values for correlations
cat("\nP-Values for Correlations:\n")
print(correlation_result$P)

# Extracting the correlation matrix from the result
correlation_matrix <- correlation_result$r
par(mfrow = c(1, 1), mar = c(1, 1, 1, 1))
corrplot(correlation_matrix, method = "number", col = colorRampPalette(c("blue", "red"))(100), 
         type = "full", tl.col = "black", tl.srt = 45, tl.cex = 1.0, # Increase text size
         addCoef.col = "black", number.cex = 0.5,  # Increase number size
         mar = c(0, 0, 1, 0)) 

# Setting labels
colnames(correlation_matrix) <- rownames(correlation_matrix) <- colnames(numerical_columns)


# Selecting numerical columns from the data frame
numerical_columns <- loan_data[, sapply(loan_data, is.numeric)]

# Calculating the correlation between each numerical feature and the target feature
correlation_with_target <- sapply(names(numerical_columns), function(col) cor(loan_data[[col]], loan_data$not.fully.paid))

# Calculating the absolute values of correlations
abs_correlation_with_target <- abs(correlation_with_target)

# Creating a data frame to display both correlation and absolute correlation
correlation_summary <- data.frame(Correlation = correlation_with_target, `Absolute Correlation` = abs_correlation_with_target)

# Displaying the correlation summary
cat("Correlation of Numerical Features with 'not.fully.paid':\n")
print(correlation_summary)

# Select numerical columns from the data frame
numerical_columns <- loan_data[, sapply(loan_data, is.numeric)]

# Calculate the correlation between each numerical feature and the target feature
correlation_with_target <- sapply(names(numerical_columns), function(col) cor(loan_data[[col]], loan_data$not.fully.paid))

# Creating a data frame to hold the correlation values
correlation_summary <- data.frame(Numerical_Features = names(correlation_with_target),
                                  Correlation = correlation_with_target)

# Creating a heatmap for the correlation with the target
ggplot(data = correlation_summary, aes(x = "Correlation", y = Numerical_Features , fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", Correlation)), vjust = 0.5, color = "black", size = 4) +  # Add text annotations
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = 'Correlation Heatmap with "not.fully.paid"', x = "Numerical Features", y = "") +
  coord_fixed(ratio = 0.2)  # Adjust the ratio for better visualization


# Applying one-hot encoding to purpose
purpose_encoded <- model.matrix(~factor(purpose) - 1, data = loan_data)

# Converting 0 and 1 to TRUE and FALSE
purpose_encoded <- purpose_encoded == 1

# Renaming the columns
colnames(purpose_encoded) <- gsub("factor\\(purpose\\)", "purpose_", colnames(purpose_encoded))

# Concatenating the encoded variables to the original data set
loan_data <- cbind(loan_data, purpose_encoded)

# Removing the original 'purpose' column
loan_data <- loan_data[, !grepl("^purpose$", colnames(loan_data))]
print(head(loan_data))

# Dropping columns
columns_to_drop <- c('revol.bal', 'inq.last.6mths', 'delinq.2yrs', 'pub.rec')
loan_data <- loan_data[, !(names(loan_data) %in% columns_to_drop)]

# Printing the updated DataFrame
print(head(loan_data))

# Identifing the factor-type columns in the DataFrame
factor_columns <- sapply(loan_data, is.factor)

# Applying label encoding
loan_data[factor_columns] <- lapply(loan_data[factor_columns], function(x) as.integer(as.factor(x)))

# Displaying the updated DataFrame with label encoded columns
print(head(loan_data))

# Setting the seed
set.seed(42)

X <- loan_data[, !(names(loan_data) %in% c('not.fully.paid'))]  # Features (all columns except 'not.fully.paid')
y <- loan_data$not.fully.paid  # Target variable

# Split the data into training and testing sets 
splitIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[splitIndex, ]
X_test <- X[-splitIndex, ]
y_train <- y[splitIndex]
y_test <- y[-splitIndex]

# Display the dimensions of the training and testing sets to verify the split
print(paste("X_train shape:", dim(X_train)[1], "rows and", dim(X_train)[2], "columns"))
print(paste("X_test shape:", dim(X_test)[1], "rows and", dim(X_test)[2], "columns"))
print(paste("y_train shape:", length(y_train)))
print(paste("y_test shape:", length(y_test)))

# Selecting numerical columns in the training set 
numerical_columns_train <- names(X_train)[sapply(X_train, is.numeric)]
preprocess <- preProcess(X_train[, numerical_columns_train], method = c("center", "scale"))

# Applying the preProcess transformations to the training set
X_train[, numerical_columns_train] <- predict(preprocess, newdata = X_train[, numerical_columns_train])

# Applying the preProcess transformations to the test set
X_test[, numerical_columns_train] <- predict(preprocess, newdata = X_test[, numerical_columns_train])

# Displaying X_train and X_test
cat("X_train after normalization:\n")
print(head(X_train))

cat("\nX_test after normalization:\n")
print(head(X_test))

# Model 1: Logistic Regression
logistic_model <- glm(not.fully.paid ~ ., data = cbind(X_train, not.fully.paid = y_train), family = "binomial")
logistic_pred <- predict(logistic_model, newdata = X_test, type = "response") > 0.5

# Ensuring y_test has the same levels as logistic_pred
y_test <- factor(y_test, levels = levels(as.factor(logistic_pred)))

# Converting y_train and y_test to factors
y_train <- factor(y_train)
y_test <- factor(y_test)

# Model 2: Random Forest
rf_model <- randomForest(factor(not.fully.paid) ~ ., data = cbind(X_train, not.fully.paid = y_train), 
                         ntree = 100, random_state = 42)
rf_pred <- predict(rf_model, newdata = X_test)

# Defining evaluate model function
evaluate_model <- function(y_true, y_pred, model_name) {
  # Ensure y_pred has the same levels as y_true
  y_pred <- factor(y_pred, levels = levels(y_true))
  
  accuracy <- confusionMatrix(y_pred, y_true)$overall["Accuracy"]
  precision <- confusionMatrix(y_pred, y_true)$byClass["Pos Pred Value"]
  recall <- confusionMatrix(y_pred, y_true)$byClass["Sensitivity"]
  f1 <- confusionMatrix(y_pred, y_true)$byClass["F1"]
  
  # Convert y_pred to numeric
  y_pred_numeric <- as.numeric(y_pred)
  
  # Ensure that y_true has two levels (binary classification)
  y_true <- factor(y_true)
  
  # Check if the levels are present in y_true
  if (!all(levels(y_pred_numeric) %in% levels(y_true))) {
    stop("Levels in y_pred_numeric are not consistent with y_true.")
  }
  
  # Use roc function from pROC
  roc_result <- roc(y_true, y_pred_numeric)
  roc_auc <- roc_result$auc
  
  cat(paste("Metrics for", model_name, ":\n"))
  cat(paste("Accuracy:", round(accuracy, 4), "\n"))
  cat(paste("Precision:", round(precision, 4), "\n"))
  cat(paste("Recall:", round(recall, 4), "\n"))
  cat(paste("F1 Score:", round(f1, 4), "\n"))
  cat(paste("ROC AUC Score:", round(roc_auc, 4), "\n"))
  
  confusion <- confusionMatrix(y_pred, y_true)
  cat("Confusion Matrix:\n")
  print(confusion)
}

# Evaluate the models
evaluate_model(y_test, logistic_pred, "Logistic Regression")
evaluate_model(y_test, rf_pred, "Random Forest")

# Identify numeric and categorical variables
numeric_vars <- sapply(X_train, is.numeric)
categorical_vars <- sapply(X_train, is.factor)

# Extract numeric features for SMOTE
X_train_numeric <- X_train[, numeric_vars]

# Applying SMOTE sampling technique
smote_data <- SMOTE(X_train_numeric, y_train, K = 5)

# Checking the structure of the returned value from SMOTE
str(smote_data)

# Extracting the combined data
X_train_smote <- smote_data$data[, !(names(smote_data$data) %in% "class")]
y_train_smote <- smote_data$data$class

# Converting y_train_smote to a factor
y_train_smote <- as.factor(y_train_smote)

# Checking dimensions and class distribution
dim(X_train_smote)
table(y_train_smote)

# Model 1: Logistic Regression with SMOTE
logistic_model_smote <- glm(not.fully.paid ~ ., data = cbind(X_train_smote, not.fully.paid = y_train_smote), family = "binomial")
logistic_pred_smote <- predict(logistic_model_smote, newdata = X_test, type = "response") > 0.5

# Model 2: Random Forest with SMOTE
rf_model_smote <- randomForest(factor(not.fully.paid) ~ ., data = cbind(X_train_smote, not.fully.paid = y_train_smote), ntree = 100)
rf_pred_smote <- predict(rf_model_smote, newdata = X_test)

# Reuse your evaluate_model function to evaluate the models
evaluate_model(y_test, logistic_pred_smote, "Logistic Regression (SMOTE)")
evaluate_model(y_test, rf_pred_smote, "Random Forest (SMOTE)")

# ROC curve for Logistic Regression
roc_curve_logistic <- roc(y_test, as.numeric(predict(logistic_model, newdata = X_test, type = "response")))

# ROC curve for Random Forest
roc_curve_rf <- roc(y_test, as.numeric(predict(rf_model, newdata = X_test)))

# Plotting ROC curves
plot(roc_curve_logistic, col = "blue", main = "ROC Curves", col.main = "black", lwd = 2)
lines(roc_curve_rf, col = "red", lwd = 2)

# Adding a legend
legend("bottomright", legend = c("Logistic Regression", "Random Forest"), col = c("blue", "red"), lwd = 2)


# Extracting coefficients to know the important features which affected the target variable
coefficients <- coef(logistic_model_smote)

# Sorting coefficients by their absolute values to see the most influential features
sorted_coefficients <- sort(abs(coefficients), decreasing = TRUE)
print(sorted_coefficients)










