# Load necessary library
library(datasets)
library(ggplot2)

# a. Load the dataset
data(iris)

# b. Remove column Label (Species)
iris_data <- iris[, -5]

# c. Decide the parameter K using the Elbow method
wss <- sapply(1:10, function(k) {
  kmeans(iris_data, centers = k, nstart = 10)$tot.withinss
})

plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters (K)",
     ylab = "Total Within-Cluster Sum of Squares")

# d. Perform K-Means Clustering (choosing K = 3 based on the Elbow method)
set.seed(42)  # For reproducibility
kmeans_result <- kmeans(iris_data, centers = 3, nstart = 10)

# e. Display the clusters obtained using K-Means
iris$Cluster <- as.factor(kmeans_result$cluster)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Cluster)) +
  geom_point() +
  labs(title = "K-Means Clustering on Iris Dataset")

# f. Check the performance of the model
table(iris$Cluster, iris$Species)

