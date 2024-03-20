# ----- B1705 Week 10 | Un-supervised ML: Clustering Techniques | 20.03.2024 -----
# ----- Pre-Lecture Work -----
# ----- 1. K-Means Clustering -----
##### 1.1 Generating Data and Clearing Environment -----
# code to generate dataset
rm(list=ls())

# Set seed for reproducibility
set.seed(123)

# Generate sample data
data <- rbind(
  matrix(rnorm(100, mean = 0.5), ncol = 2),
  matrix(rnorm(100, mean = 3), ncol = 2)
)

# Plot data
plot(data, col = "blue", main = "Raw Data", xlab = "Feature 1", ylab = "Feature 2", pch = 20)


##### 1.2. Applying K-Means Clustering -----
# Apply k-means with k = 2
set.seed(123)  # Setting seed for reproducibility
kmeans_result <- kmeans(data, centers = 2)

# Plot the results
plot(data, col = kmeans_result$cluster, main = "K-Means Clustering Results", xlab = "Feature 1", ylab = "Feature 2", pch = 20)
points(kmeans_result$centers, col = 1:2, pch = 8, cex = 2)

# ----- 2. Hierarchical Clustering -----
##### 2.1. Creating Example Hierarchical Dendrogram -----
# Compute the distance matrix
dist_matrix <- dist(data)

# Perform hierarchical clustering using complete linkage
hc_result <- hclust(dist_matrix, method = "complete")

# Plot the dendrogram
plot(hc_result, main = "Hierarchical Clustering Dendrogram", xlab = "Points", ylab = "Distance")

##### 2.2. Cutting Tree -----
# Cut the tree into 2 clusters
clusters <- cutree(hc_result, k = 2)

# Plot the original data colored by clusters
plot(data, col = clusters, main = "Hierarchical Clustering Results", xlab = "Feature 1", ylab = "Feature 2", pch = 20)

##### 2.3. Elbow Method -----
library(cluster)
set.seed(123)

wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(data, centers = i, nstart = 20)$withinss)
}

plot(1:15, wss, type = "b", main = "Plot of SSE", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

##### 2.4. Silhouette Score -----
library(cluster)
sil_width <- numeric(15)
for (k in 2:15) {
  km.res <- kmeans(data, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data))
  sil_width[k] <- mean(ss[, 3])
}
plot(2:15, sil_width[2:15], type = 'b', main = "Plot of Silhouette Score", xlab = "Number of Clusters", ylab = "Average Silhouette Width")

##### 2.5. Gap Statistic -----
set.seed(123)
gap_stat <- clusGap(data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
plot(gap_stat, main = "Plot of Gap Statistic")









