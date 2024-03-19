# ----- B1705 Week 10 | Un-supervised ML: Dimensionality Reduction in ML | 20.03.2024 -----
# ----- Pre-Lecture Work -----
# ----- 1. Principal Component Analysis (PCA) -----
##### 1.1. Load and clean environment -----
rm(list=ls())
library(ggplot2)

# Create synthetic dataset with more dimensions
set.seed(123)
V1 <- rnorm(100, mean = 50, sd = 10)
V2 <- V1 + rnorm(100, mean = 0, sd = 5)  # Correlated with V1
V3 <- V1 - rnorm(100, mean = 0, sd = 5)  # Correlated with V1
V4 <- rnorm(100, mean = 20, sd = 5)  # Not correlated with V1
V5 <- V4 + rnorm(100, mean = 0, sd = 2)  # Correlated with V4
V6 <- V4 - rnorm(100, mean = 0, sd = 2)  # Correlated with V4
V7 <- rnorm(100, mean = -10, sd = 5)  # Independent
V8 <- rnorm(100, mean = 5, sd = 3)    # Independent
V9 <- V2 * rnorm(100, mean = 1, sd = 0.1) + V6  # Composite, correlated with V2 and V6
V10 <- rnorm(100, mean = 0, sd = 4)  # Independent

data <- data.frame(V1, V2, V3, V4, V5, V6, V7, V8, V9, V10)

##### 1.2. Perform PCA -----
# Perform PCA on that dataset
pca_result <- prcomp(data, scale. = TRUE)

# Print the summary to see the proportion of variance explained by each principal component
print(summary(pca_result))

##### 1.3. Visualisation -----
library(ggplot2)

# Create a data frame of the PCA scores
pca_scores <- as.data.frame(pca_result$x)

# Plot the first two principal components
ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point() +
  xlab("Principal Component 1") +
  ylab("Principal Component 2") +
  ggtitle("PCA: First two principal components") +
  theme_minimal()

##### 1.4. Determining dimensions to retain -----
# Print the summary of PCA results to see the proportion of variance explained by each component
print(summary(pca_result))

# Plot the cumulative proportion of variance explained
plot(pca_result$sdev^2 / sum(pca_result$sdev^2), type = 'o', main = "Proportion of Variance Explained", xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0, 1))
cumulative_variance <- cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2)
points(cumulative_variance, type = 'o', col = "red")

# ----- 2. Scree Plot -----
summary_pca <- summary(pca_result)

# importance matrix holds the variance explained values
var_explained <- summary_pca$importance[2,]  # Proportion of Variance Explained

# Create scree plot
plot(var_explained, xlab = "Principal Component", ylab = "Proportion of Variance Explained",
     type = 'b', pch = 19, col = "blue", main = "Scree Plot")
abline(h = 0.01, col = "red", lty = 2)  # Optional: Line to indicate cutoff for smaller eigenvalues

# THIS IS THE BIT THAT DETERMINES HOW MANY COMPONENTS TO RETAIN
# The eigenvalues can be obtained by squaring the singular values (standard deviations in pca_result)
eigenvalues <- pca_result$sdev^2
# Apply Kaiser criterion
num_components_kaiser <- sum(eigenvalues > 1)

cat("Number of components to retain according to Kaiser criterion:", num_components_kaiser, "\n")

# ----- 3. t-SNE ----- 
# Loading libraries
library(Rtsne)

# Set seed for reproducibility
set.seed(42)

# Execute t-SNE
tsne_result <- Rtsne(data, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 1000)

# The output, tsne_result, contains Y which is the embedding in 2 dimensions

# Create scatterplot
plot(tsne_result$Y, main = "t-SNE results", xlab = "Dimension 1", ylab = "Dimension 2")























