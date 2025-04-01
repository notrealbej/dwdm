# Load required libraries
library(e1071)
library(ggplot2)
library(caret)

# a. Load the dataset from local file
df <- read.csv("C:/Users/Test/Downloads/mushrooms.csv", stringsAsFactors = TRUE)

# Ensure all columns are factors and handle missing values
df <- na.omit(df)
df[] <- lapply(df, as.factor)

# b. Perform a train-test split (80%-20%)
set.seed(123)
trainIndex <- createDataPartition(df$class, p = 0.8, list = FALSE)
train_data <- df[trainIndex, ]
test_data <- df[-trainIndex, ]

# c. Train using Naïve Bayes Classifier
model <- naiveBayes(class ~ ., data = train_data)

# d. Visualize the data using ggplot (Example: Cap Shape Distribution)
ggplot(df, aes(x = cap.shape, fill = class)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Cap Shape by Class", 
       x = "Cap Shape", 
       y = "Count") +
  theme_minimal()

# e. Evaluate model accuracy and precision
predictions <- predict(model, test_data)
conf_matrix <- confusionMatrix(predictions, test_data$class)

cat("Accuracy:", conf_matrix$overall['Accuracy'], "\n")
cat("Precision (for poisonous class):", conf_matrix$byClass['Precision'], "\n")

# f. Tune hyperparameters (Laplace smoothing)
model_tuned <- naiveBayes(class ~ ., data = train_data, laplace = 1)
predictions_tuned <- predict(model_tuned, test_data)
conf_matrix_tuned <- confusionMatrix(predictions_tuned, test_data$class)

cat("Tuned Accuracy:", conf_matrix_tuned$overall['Accuracy'], "\n")

# g. Feature Importance Visualization (Using Conditional Probabilities)
feature_importance <- data.frame(
  Feature = names(train_data)[-1],  # Exclude target column
  Importance = sapply(names(train_data)[-1], function(feature) {
    table_feature <- model_tuned$tables[[feature]]  # Get conditional probability table
    if (is.null(table_feature) || ncol(table_feature) < 2) {
      return(0)  # If there's only one class, importance is zero
    }
    mean(abs(table_feature[,1] - table_feature[,2]))  # Compute importance
  })
)

# Plot feature importance
ggplot(feature_importance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance", 
       x = "Feature", 
       y = "Importance") +
  theme_minimal()
