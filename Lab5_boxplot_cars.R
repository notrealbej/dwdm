# Load necessary libraries
library(ggplot2)  # For visualization
library(dplyr)    # For data manipulation

# Load the built-in mtcars dataset
data("mtcars")

# ------------------------------
# a) Create a box plot for mpg to analyze distribution
# ------------------------------
ggplot(mtcars, aes(y = mpg)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box Plot of Miles Per Gallon (mpg)", y = "Miles Per Gallon") +
  theme_minimal()

# ------------------------------
# b) Compare the distribution of horsepower (hp) for different cylinders (cyl)
# ------------------------------
ggplot(mtcars, aes(x = as.factor(cyl), y = hp, fill = as.factor(cyl))) +
  geom_boxplot() +
  labs(title = "Horsepower Distribution by Cylinders",
       x = "Number of Cylinders", y = "Horsepower (hp)") +
  scale_fill_manual(values = c("red", "blue", "green")) +
  theme_minimal()

# ------------------------------
# c) Analyze weight (wt) distribution based on transmission type (am)
# ------------------------------
ggplot(mtcars, aes(x = as.factor(am), y = wt, fill = as.factor(am))) +
  geom_boxplot() +
  labs(title = "Weight Distribution by Transmission Type",
       x = "Transmission Type (0 = Automatic, 1 = Manual)", y = "Weight (wt)") +
  scale_fill_manual(values = c("purple", "orange")) +
  theme_minimal()

# ------------------------------
# d) Add mean to a box plot of wt (weight) for all cars
# ------------------------------
ggplot(mtcars, aes(x = "", y = wt)) +  # Add x = "" to avoid missing aesthetics
  geom_boxplot(fill = "lightgreen", color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, color = "red") +  # Adding mean
  labs(title = "Box Plot of Weight (wt) with Mean", y = "Weight (wt)", x = "") + 
  theme_minimal()

