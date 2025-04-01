# ==============================
# 📌 Step 1: Import the Dataset
# ==============================
# Load necessary libraries
library(rpart)
library(rpart.plot)
library(caret)

# Load the iris dataset
data(iris)

# ==============================
# 📌 Step 2: Clean the Data
# ==============================
# In this case, the iris dataset is already clean, so no additional cleaning is required.
# But, if needed, we could check for missing values and handle them here:
# sum(is.na(iris))

# ==============================
# 📌 Step 3: Split the Data into Training and Testing Sets
# ==============================
# Set the seed for reproducibility
set.seed(123)

# Split the data into training and testing sets (70% train, 30% test)
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# ==============================
# 📌 Step 4: Build the Decision Tree Model
# ==============================
# Build the initial decision tree model
treeModel <- rpart(Species ~ ., data = trainData, method = "class")

# Visualize the tree
rpart.plot(treeModel, main = "Decision Tree for iris Dataset")

# ==============================
# 📌 Step 5: Make Predictions
# ==============================
# Predict the test set using the decision tree model
predictions <- predict(treeModel, testData, type = "class")

# ==============================
# 📌 Step 6: Evaluate Model Performance
# ==============================
# Confusion Matrix to evaluate performance
confMat <- confusionMatrix(predictions, testData$Species)

# Print the confusion matrix
print(confMat)

# Accuracy of the model
cat("Model Accuracy: ", confMat$overall['Accuracy'], "\n")

# ==============================
# 📌 Step 7: Tune Hyperparameters for Better Accuracy
# ==============================
# Tune hyperparameters such as cp (complexity parameter) for better model performance
tuneGrid <- expand.grid(cp = seq(0.01, 0.1, by = 0.01))  # tuning complexity parameter

# Tune the decision tree model using caret's train function
tunedModel <- train(Species ~ ., data=trainData, method="rpart", trControl=trainControl(method="cv"), tuneGrid=tuneGrid)

# Print the best tuning parameter and model accuracy
cat("Best cp: ", tunedModel$bestTune$cp, "\n")
cat("Tuned Model Accuracy: ", tunedModel$results$Accuracy, "\n")

# Refit the model with the best cp value
bestModel <- tunedModel$finalModel

# Visualize the tuned decision tree
rpart.plot(bestModel, main="Tuned Decision Tree for iris Dataset")

# ==============================
# 📌 Step 8: Evaluate the Tuned Model
# ==============================
# Predict the test set using the tuned decision tree model
predictions_tuned <- predict(bestModel, testData, type = "class")

# Confusion Matrix for tuned model
confMat_tuned <- confusionMatrix(predictions_tuned, testData$Species)

# Print the confusion matrix
print(confMat_tuned)

# Accuracy of the tuned model
cat("Tuned Model Accuracy: ", confMat_tuned$overall['Accuracy'], "\n")

