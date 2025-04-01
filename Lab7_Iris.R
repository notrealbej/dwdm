# Load required libraries
library(dplyr)
library(wavelets)

# Load the Iris dataset from CSV
iris <- read.csv("D:/Sem6/DataWarehousing/labInputs/Iris.csv")

# Display first few rows and summary
head(iris)
summary(iris)
plot(iris)

# Check for missing values and replace them with column means
if (any(is.na(iris))) {
  for (i in 2:5) {  # Only numerical columns
    if (any(is.na(iris[, i]))) {
      iris[is.na(iris[, i]), i] <- mean(iris[, i], na.rm = TRUE)
    }
  }
}

# Remove duplicate rows
iris <- iris[!duplicated(iris), ]

# Identify and remove outliers in SepalLengthCm using IQR
Q1 <- quantile(iris$SepalLengthCm, 0.25)
Q3 <- quantile(iris$SepalLengthCm, 0.75)
IQR <- Q3 - Q1
iris <- iris %>% filter(SepalLengthCm >= (Q1 - 1.5 * IQR) & SepalLengthCm <= (Q3 + 1.5 * IQR))

# Convert Species column to numeric
iris$Species <- as.numeric(factor(iris$Species))

# Normalize numerical columns (0-1 scale)
normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }
iris[, 2:5] <- as.data.frame(lapply(iris[, 2:5], normalize))

# Sort dataset by SepalLengthCm in descending order
iris <- iris %>% arrange(desc(SepalLengthCm))

# Filter dataset where SepalLengthCm > 6.0
filtered_iris <- iris %>% filter(SepalLengthCm > 6.0)

# Create a new column Sepal.Area
iris$Sepal.Area <- iris$SepalLengthCm * iris$SepalWidthCm

# Rename columns for better readability
colnames(iris) <- c("ID", "Length_Sepal", "Width_Sepal", "Length_Petal", "Width_Petal", "Flower_Type", "Sepal_Area")

# Apply Discrete Wavelet Transform (DWT) on Petal.Length
petal_dwt <- dwt(iris$Length_Petal, filter = "haar", n.levels = 3)

# Display Approximation & Detail Coefficients
approx_coeff <- petal_dwt@W[[1]]
detail_coeff <- petal_dwt@W[[2]]
print(approx_coeff)
print(detail_coeff)

# Reconstruct Petal.Length using IDWT
reconstructed_petal <- idwt(petal_dwt)

# Plot Original vs Reconstructed Petal.Length
plot(iris$Length_Petal, type = "l", col = "blue", main = "Original vs Reconstructed Petal.Length")
lines(reconstructed_petal, col = "red")
legend("topright", legend = c("Original", "Reconstructed"), col = c("blue", "red"), lty = 1)

# Apply Wavelet Denoising on Petal.Width
dwt_pw <- dwt(iris$Width_Petal, filter="haar")

# Apply thresholding for noise removal
threshold_value <- mean(unlist(dwt_pw@W)) + 2 * sd(unlist(dwt_pw@W))
dwt_pw@W <- lapply(dwt_pw@W, function(x) ifelse(abs(x) < threshold_value, 0, x))

# Apply Inverse DWT (IDWT) to reconstruct the denoised signal
denoised_petal_width <- idwt(dwt_pw)

# Plot Original vs Denoised Petal.Width
plot(iris$Width_Petal, type="l", col="blue", main="Original vs Denoised Petal.Width")
lines(denoised_petal_width, col="red")
legend("topright", legend=c("Original", "Denoised"), col=c("blue", "red"), lty=1)

# Multi-Resolution Analysis (MRA) on Sepal.Length
sepal_dwt <- dwt(iris$Length_Sepal, filter = "haar", n.levels = 3)

# Plot different resolution levels
plot(sepal_dwt@W[[1]], type = "l", main = "MRA Level 1")
plot(sepal_dwt@W[[2]], type = "l", main = "MRA Level 2")
plot(sepal_dwt@W[[3]], type = "l", main = "MRA Level 3")

# Extract statistical features from Wavelet Coefficients
sepal_width_dwt <- dwt(iris$Width_Sepal, filter = "haar", n.levels = 3)

# Extract Approximation Coefficients (Last level)
approx_coeff <- sepal_width_dwt@V[[length(sepal_width_dwt@V)]]

# Extract All Detail Coefficients (Concatenating Levels)
detail_coeffs <- unlist(sepal_width_dwt@W)

# Compute Mean & Standard Deviation of Approximation & Detail Coefficients
mean_approx <- mean(approx_coeff, na.rm = TRUE)
std_approx <- sd(approx_coeff, na.rm = TRUE)

mean_detail <- mean(detail_coeffs, na.rm = TRUE)
std_detail <- sd(detail_coeffs, na.rm = TRUE)

# Display Extracted Features
cat("Extracted Features from DWT of Sepal.Width:\n")
cat("---------------------------------------------------\n")
cat(sprintf("Approximation Coefficients - Mean: %.4f | SD: %.4f\n", mean_approx, std_approx))
cat(sprintf("Detail Coefficients - Mean: %.4f | SD: %.4f\n", mean_detail, std_detail))
cat("---------------------------------------------------\n")

# Detect Outliers in Petal.Length using Wavelet Coefficients
mean_coeff <- mean(petal_dwt@W[[1]])
std_coeff <- sd(petal_dwt@W[[1]])

# Identify Outliers (Mean ± 2*SD Threshold)
outliers <- which(abs(petal_dwt@W[[1]]) > (mean_coeff + 2 * std_coeff))
print(outliers)
