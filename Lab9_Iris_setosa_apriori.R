# ==============================
# 📌 Load Required Library
# ==============================
library(arules)

# ==============================
# 📌 Step 6: Find the Most Frequent Itemsets in the iris Dataset
# ==============================

# Load iris dataset
data("iris")

# Convert numeric variables to categorical bins (Low, Medium, High)
iris_cat <- as.data.frame(lapply(iris[, 1:4], function(x) cut(x, breaks=3, labels=c("Low", "Medium", "High"))))

# Keep species column
iris_cat$Species <- iris$Species  

# Convert dataset into transactions
iris_trans <- as(iris_cat, "transactions")

# Apply Apriori algorithm to find frequent itemsets
frequent_items_iris <- apriori(iris_trans, parameter=list(support=0.2, confidence=0.7, target="frequent itemsets"))

# Display the most frequent itemsets
inspect(sort(frequent_items_iris, by="support", decreasing=TRUE))

# ==============================
# 📌 Step 7: Find Association Rules for Automatic & Manual Transmission Cars in mtcars
# ==============================

# Load mtcars dataset
data("mtcars")

# Convert transmission type (am) into categorical factors
mtcars$am <- factor(mtcars$am, levels=c(0,1), labels=c("Auto", "Manual"))

# Convert numeric variables (hp, wt) into categorical bins (Low, Medium, High)
mtcars_bin <- as.data.frame(lapply(mtcars[, c("hp", "wt")], function(x) cut(x, breaks=3, labels=c("Low", "Medium", "High"))))

# Keep transmission column
mtcars_bin$am <- mtcars$am  

# Convert dataset into transactions
mtcars_trans <- as(mtcars_bin, "transactions")

# Apply Apriori algorithm to find association rules
rules_mtcars <- apriori(mtcars_trans, parameter=list(support=0.3, confidence=0.7, target="rules"))

# Display association rules related to Auto & Manual cars
inspect(rules_mtcars)

# ==============================
# 📌 Step 8: Find Relationships Between Petal Length, Width & Species in iris
# ==============================

# Select relevant features (Petal Length, Petal Width, and Species)
iris_petal <- iris[, c("Petal.Length", "Petal.Width", "Species")]

# Convert Petal Length & Width into categorical bins (Short, Medium, Long)
iris_petal$Petal.Length <- cut(iris_petal$Petal.Length, breaks=3, labels=c("Short", "Medium", "Long"))
iris_petal$Petal.Width <- cut(iris_petal$Petal.Width, breaks=3, labels=c("Narrow", "Medium", "Wide"))

# Convert dataset into transactions
iris_petal_trans <- as(iris_petal, "transactions")

# Apply Apriori algorithm for species-based associations
rules_petal <- apriori(iris_petal_trans, parameter=list(support=0.2, confidence=0.7, target="rules"))

# Display rules for petal characteristics
inspect(rules_petal)

# ==============================
# 📌 Step 9: Identify Common Feature Combinations for Setosa Flowers in iris
# ==============================

# Filter rules where RHS contains "setosa"
rules_setosa <- subset(rules_petal, subset = rhs %pin% "setosa")

# Display sorted rules for setosa species
inspect(sort(rules_setosa, by="support", decreasing=TRUE))

# ==============================
# 📌 Step 10: Find Relationships Between Species and Sepal/Petal Characteristics in iris
# ==============================

# Convert all numerical attributes to categorical bins (Low, Medium, High)
iris_all <- as.data.frame(lapply(iris[, 1:4], function(x) cut(x, breaks=3, labels=c("Low", "Medium", "High"))))

# Keep species column
iris_all$Species <- iris$Species  

# Convert dataset into transactions
iris_all_trans <- as(iris_all, "transactions")

# Apply Apriori algorithm to find frequent itemsets & rules
frequent_itemsets <- apriori(iris_all_trans, parameter=list(support=0.2, target="frequent itemsets"))
rules_iris <- apriori(iris_all_trans, parameter=list(support=0.2, confidence=0.7, target="rules"))

# Display sorted frequent itemsets and rules
inspect(sort(frequent_itemsets, by="support", decreasing=TRUE))
inspect(sort(rules_iris, by="confidence", decreasing=TRUE))

