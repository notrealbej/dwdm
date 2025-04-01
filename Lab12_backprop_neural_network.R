# Install required packages (if not already installed)
# Load necessary libraries
library(neuralnet)
library(C50)
library(caret)
library(dplyr)

# Step 1: Load the iris dataset
print("Loading the iris dataset...")
data(iris)
print(head(iris))

# Step 2: Prepare the data
# Create dummy variables for each species
iris$setosa <- ifelse(iris$Species == "setosa", 1, 0)
iris$versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
iris$virginica <- ifelse(iris$Species == "virginica", 1, 0)

# Show updated dataset
print("Modified iris dataset with dummy variables:")
print(head(iris))

# Step 3: Split the data into training and testing sets (70% training, 30% testing)
set.seed(123)
index <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[index, ]
test_data <- iris[-index, ]

# Show sample training and testing data
print("Training Data Sample:")
print(head(train_data))
print("Testing Data Sample:")
print(head(test_data))

# Step 4: Train the Backpropagation Neural Network (BPNN)
formula_nn <- as.formula("setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width")
nn_model <- neuralnet(formula_nn, data=train_data, hidden=c(5), linear.output=FALSE)

# Step 5: Predict with Neural Network
print("Predicting with Neural Network...")
nn_results <- predict(nn_model, test_data[, 1:4])  # Use only the features for prediction
print("Neural Network raw outputs:")
print(head(nn_results))

# Convert raw outputs to final predictions by selecting the species with the highest output
nn_pred <- apply(nn_results, 1, which.max)
species_levels <- c("setosa", "versicolor", "virginica")
nn_pred <- factor(species_levels[nn_pred], levels=species_levels)
print("Neural Network final predictions:")
print(head(nn_pred))

# Step 6: Train the Decision Tree model
dt_model <- C5.0(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=train_data)

# Step 7: Predict with Decision Tree
dt_pred <- predict(dt_model, test_data)
print("Decision Tree final predictions:")
print(head(dt_pred))

# Step 8: Evaluate both models using confusion matrices
nn_cm <- confusionMatrix(nn_pred, test_data$Species)
dt_cm <- confusionMatrix(dt_pred, test_data$Species)

# Print performance metrics for both models
print("Backpropagation Neural Network Performance:")
print(nn_cm)

print("Decision Tree Performance:")
print(dt_cm)
