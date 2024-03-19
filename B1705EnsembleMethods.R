# ----- B1705 Week 10 | Un-supervised ML: Ensemble Methods in ML | 20.03.2024 -----
# ----- Pre-Lecture Work -----
# ----- 1. Bagging -----
library(randomForest)

# Using the iris dataset
data(iris)
x <- iris[, -5]  # Feature variables (all columns except the species)
y <- iris[, 5]   # Target variable (species column)

# Train the random forest model
set.seed(42)  # For reproducibility
rf_model <- randomForest(x = x, y = y, ntree = 100)

# Print the model summary
print(rf_model)

# Plot variable importance
importance <- importance(rf_model)
varImpPlot(rf_model)

# ----- 2. Boosting -----
library(gbm)

# Load the iris dataset
data(iris)

# Convert the Species to a binary classification task
# create a binary variable indicating whether the species is Setosa or not
iris$IsSetosa <- ifelse(iris$Species == "setosa", 1, 0)

# Split the dataset into training and testing sets
set.seed(42)  # Ensure reproducibility
index <- sample(1:nrow(iris), 0.7 * nrow(iris))  # 70% for training
train_data <- iris[index, ]
test_data <- iris[-index, ]

# Train the GBM model
# predict 'IsSetosa' using the other features
gbm_model <- gbm(IsSetosa ~ ., data = train_data, distribution = "bernoulli", 
                 n.trees = 100, interaction.depth = 1, shrinkage = 0.01, 
                 n.minobsinnode = 10, verbose = FALSE)

# Model summary
summary(gbm_model)

# Prediction and evaluation
# Note: We use the test data without the 'IsSetosa' column for prediction
predicted_probs <- predict(gbm_model, newdata = test_data, n.trees = 100, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Calculate the accuracy
accuracy <- mean(predicted_classes == test_data$IsSetosa)
cat("Accuracy:", accuracy, "\n")






