install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tidyr")
library(tidyverse)
library(ggplot2)
library(tidyr)
dataset <- read.csv("dataset.csv")

# View the first few rows and structure
head(dataset)
str(dataset)
summary(dataset)

# Drop the 'Unnamed: 0' column
dataset <- dataset %>% select(-Unnamed..0)

# Check for missing values
colSums(is.na(dataset))

# Drop rows with missing values (small proportion)
dataset <- dataset %>% drop_na(artists, album_name, track_name)

# Check for duplicate rows
sum(duplicated(dataset))

# Remove duplicates
dataset <- dataset %>% distinct()

# Check the range of 'duration_ms'
summary(dataset$duration_ms)

# Filter to keep only realistic song durations (e.g., 30 seconds to 10 minutes)
dataset <- dataset %>% filter(duration_ms > 30000 & duration_ms < 600000)

# Select numerical columns to scale
num_cols <- c("danceability", "energy", "loudness", "speechiness", 
              "acousticness", "instrumentalness", "liveness", "valence", "tempo")

# Scale the numerical columns
dataset[num_cols] <- scale(dataset[num_cols])

# Check total number of missing values in the dataset
sum(is.na(data))

# Example column: popularity
popularity <- dataset$popularity

# Calculate Q1, Q3, and IQR
Q1 <- quantile(popularity, 0.25, na.rm = TRUE)
Q3 <- quantile(popularity, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define outlier boundaries
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- which(popularity < lower_bound | popularity > upper_bound)
print(length(outliers)) # Number of outliers

# Remove rows with outliers
data_no_outliers <- dataset[!(dataset$popularity < lower_bound | dataset$popularity > upper_bound), ]

# Cap outliers to the bounds
dataset$popularity <- ifelse(popularity < lower_bound, lower_bound,
                             ifelse(popularity > upper_bound, upper_bound, popularity))

summary(dataset)  # Summary statistics for all columns

# Select key features for summary statistics
key_features <- c("danceability", "energy", "loudness", "tempo", 
                  "speechiness", "acousticness", "instrumentalness", 
                  "liveness", "valence")

# Generate summary statistics
summary_statistics <- dataset %>% 
  select(all_of(key_features)) %>% 
  summary()

# Display the summary statistics
print(summary_statistics)

# Create summary statistics as a data frame
summary_df <- dataset %>% 
  select(all_of(key_features)) %>% 
  summarise_all(list(
    Count = ~sum(!is.na(.)),
    Mean = ~mean(., na.rm = TRUE),
    SD = ~sd(., na.rm = TRUE),
    Min = ~min(., na.rm = TRUE),
    Q1 = ~quantile(., 0.25, na.rm = TRUE),
    Median = ~median(., na.rm = TRUE),
    Q3 = ~quantile(., 0.75, na.rm = TRUE),
    Max = ~max(., na.rm = TRUE)
  ))

# Visualize danceability before outlier treatment
hist(dataset$danceability, breaks = 30, main = "Danceability Distribution (Before)",
     xlab = "Danceability", col = "skyblue", border = "white")
boxplot(dataset$danceability, main = "Danceability Boxplot (Before)",
        ylab = "Danceability", col = "skyblue")

# Calculate Q1, Q3, and IQR for danceability
Q1 <- quantile(dataset$danceability, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$danceability, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Print the bounds
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")

# Treat outliers by capping values
dataset$danceability <- ifelse(dataset$danceability < lower_bound, lower_bound,
                               ifelse(dataset$danceability > upper_bound, upper_bound, dataset$danceability))

# Visualize danceability after outlier treatment
hist(dataset$danceability, breaks = 30, main = "Danceability Distribution (After)",
     xlab = "Danceability", col = "lightgreen", border = "white")
boxplot(dataset$danceability, main = "Danceability Boxplot (After)",
        ylab = "Danceability", col = "lightgreen")

# Visualize energy before outlier treatment
hist(dataset$energy, breaks = 30, main = "Energy Distribution (Before)",
     xlab = "Energy", col = "skyblue", border = "white")
boxplot(dataset$energy, main = "Energy Boxplot (Before)",
        ylab = "Energy", col = "skyblue")

# Calculate Q1, Q3, and IQR for energy
Q1 <- quantile(dataset$energy, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$energy, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Print the bounds
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")

# Treat outliers by capping values
dataset$energy <- ifelse(dataset$energy < lower_bound, lower_bound,
                         ifelse(dataset$energy > upper_bound, upper_bound, dataset$energy))

# Visualize energy after outlier treatment
hist(dataset$energy, breaks = 30, main = "Energy Distribution (After)",
     xlab = "Energy", col = "lightgreen", border = "white")
boxplot(dataset$energy, main = "Energy Boxplot (After)",
        ylab = "Energy", col = "lightgreen")


# Visualize tempo before outlier treatment
hist(dataset$tempo, breaks = 30, main = "Tempo Distribution (Before)",
     xlab = "Tempo", col = "skyblue", border = "white")
boxplot(dataset$tempo, main = "Tempo Boxplot (Before)",
        ylab = "Tempo", col = "skyblue")

# Calculate Q1, Q3, and IQR for tempo
Q1 <- quantile(dataset$tempo, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$tempo, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Print the bounds
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")

# Treat outliers by capping values
dataset$tempo <- ifelse(dataset$tempo < lower_bound, lower_bound,
                        ifelse(dataset$tempo > upper_bound, upper_bound, dataset$tempo))

# Visualize tempo after outlier treatment
hist(dataset$tempo, breaks = 30, main = "Tempo Distribution (After)",
     xlab = "Tempo", col = "lightgreen", border = "white")
boxplot(dataset$tempo, main = "Tempo Boxplot (After)",
        ylab = "Tempo", col = "lightgreen")

# Visualize speechiness before outlier treatment
hist(dataset$speechiness, breaks = 30, main = "Speechiness Distribution (Before)",
     xlab = "Speechiness", col = "skyblue", border = "white")
boxplot(dataset$speechiness, main = "Speechiness Boxplot (Before)",
        ylab = "Speechiness", col = "skyblue")

# Calculate Q1, Q3, and IQR for speechiness
Q1 <- quantile(dataset$speechiness, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$speechiness, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Print the bounds
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")

# Treat outliers by capping values
dataset$speechiness <- ifelse(dataset$speechiness < lower_bound, lower_bound,
                              ifelse(dataset$speechiness > upper_bound, upper_bound, dataset$speechiness))

# Visualize speechiness after outlier treatment
hist(dataset$speechiness, breaks = 30, main = "Speechiness Distribution (After)",
     xlab = "Speechiness", col = "lightgreen", border = "white")
boxplot(dataset$speechiness, main = "Speechiness Boxplot (After)",
        ylab = "Speechiness", col = "lightgreen")


# Visualize valence before outlier treatment
hist(dataset$valence, breaks = 30, main = "Valence Distribution (Before)",
     xlab = "Valence", col = "skyblue", border = "white")
boxplot(dataset$valence, main = "Valence Boxplot (Before)",
        ylab = "Valence", col = "skyblue")

# Calculate Q1, Q3, and IQR for valence
Q1 <- quantile(dataset$valence, 0.25, na.rm = TRUE)
Q3 <- quantile(dataset$valence, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Define lower and upper bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Print the bounds
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")

# Treat outliers by capping values
dataset$valence <- ifelse(dataset$valence < lower_bound, lower_bound,
                          ifelse(dataset$valence > upper_bound, upper_bound, dataset$valence))

# Visualize valence after outlier treatment
hist(dataset$valence, breaks = 30, main = "Valence Distribution (After)",
     xlab = "Valence", col = "lightgreen", border = "white")
boxplot(dataset$valence, main = "Valence Boxplot (After)",
        ylab = "Valence", col = "lightgreen")

# Select only numeric columns
numeric_cols <- dataset %>%
  select(where(is.numeric))

# Check the columns being used
colnames(numeric_cols)

# Calculate correlations with popularity
correlations <- cor(numeric_cols, use = "complete.obs")

# Extract correlation values with popularity
cor_with_popularity <- correlations["popularity", ]
cor_with_popularity <- cor_with_popularity[order(-abs(cor_with_popularity))]  # Sort by absolute correlation
print(cor_with_popularity)

# Convert correlations to a data frame for plotting
cor_df <- data.frame(Feature = names(cor_with_popularity),
                     Correlation = cor_with_popularity)

# List of selected features
selected_features <- c("loudness", "danceability", "time_signature", 
                       "speechiness", "valence", "tempo", "instrumentalness")


# Scatter plot for danceability vs popularity
plot(dataset$danceability, dataset$popularity,
     main = "Scatter Plot: Danceability vs Popularity",
     xlab = "Danceability", ylab = "Popularity",
     col = "blue", pch = 16)

abline(lm(dataset$popularity ~ dataset$danceability), col = "red", lwd = 2)

# Scatter plot for loudness vs popularity
plot(dataset$loudness, dataset$popularity,
     main = "Scatter Plot: Loudness vs Popularity",
     xlab = "Loudness", ylab = "Popularity",
     col = "blue", pch = 16)

# Add a regression line
abline(lm(dataset$popularity ~ dataset$loudness), col = "red", lwd = 2)

# Scatter plot for danceability vs popularity
plot(dataset$danceability, dataset$popularity,
     main = "Scatter Plot: Danceability vs Popularity",
     xlab = "Danceability", ylab = "Popularity",
     col = "blue", pch = 16)

# Add a regression line
abline(lm(dataset$popularity ~ dataset$danceability), col = "red", lwd = 2)

# Scatter plot for time_signature vs popularity
plot(dataset$time_signature, dataset$popularity,
     main = "Scatter Plot: Time Signature vs Popularity",
     xlab = "Time Signature", ylab = "Popularity",
     col = "blue", pch = 16)

# Add a regression line
abline(lm(dataset$popularity ~ dataset$time_signature), col = "red", lwd = 2)

# Scatter plot for speechiness vs popularity
plot(dataset$speechiness, dataset$popularity,
     main = "Scatter Plot: Speechiness vs Popularity",
     xlab = "Speechiness", ylab = "Popularity",
     col = "blue", pch = 16)

# Add a regression line
abline(lm(dataset$popularity ~ dataset$speechiness), col = "red", lwd = 2)

# Scatter plot for valence vs popularity
plot(dataset$valence, dataset$popularity,
     main = "Scatter Plot: Valence vs Popularity",
     xlab = "Valence", ylab = "Popularity",
     col = "blue", pch = 16)

# Add a regression line
abline(lm(dataset$popularity ~ dataset$valence), col = "red", lwd = 2)

# Scatter plot for tempo vs popularity
plot(dataset$tempo, dataset$popularity,
     main = "Scatter Plot: Tempo vs Popularity",
     xlab = "Tempo", ylab = "Popularity",
     col = "blue", pch = 16)

# Add a regression line
abline(lm(dataset$popularity ~ dataset$tempo), col = "red", lwd = 2)

# Scatter plot for instrumentalness vs popularity
plot(dataset$instrumentalness, dataset$popularity,
     main = "Scatter Plot: Instrumentalness vs Popularity",
     xlab = "Instrumentalness", ylab = "Popularity",
     col = "blue", pch = 16)

# Add a regression line
abline(lm(dataset$popularity ~ dataset$instrumentalness), col = "red", lwd = 2)

# List of selected features
selected_features <- c("popularity", "danceability", "time_signature", 
                       "tempo", "speechiness", "valence", "instrumentalness")

install.packages("moments")

library(moments)

# List of selected features
selected_features <- c("popularity", "danceability", "time_signature", 
                       "tempo", "speechiness", "valence", "instrumentalness")

# Calculate skewness for each feature
for (feature in selected_features) {
  skewness_value <- skewness(dataset[[feature]], na.rm = TRUE)
  cat("Skewness of", feature, ":", skewness_value, "\n")
}

# Histogram for popularity
hist(dataset$popularity, breaks = 30, main = "Distribution of Popularity",
     xlab = "Popularity", col = "skyblue", border = "white")

# Histogram for Danceability
hist(dataset$danceability, breaks = 30, main = "Distribution of Danceability",
     xlab = "Danceability", col = "lightgreen", border = "white")

# Histogram for Time Signature
hist(dataset$time_signature, breaks = 30, main = "Distribution of Time Signature",
     xlab = "Time Signature", col = "salmon", border = "white")

# Histogram for Tempo
hist(dataset$tempo, breaks = 30, main = "Distribution of Tempo",
     xlab = "Tempo", col = "plum", border = "white")

# Histogram for Speechiness
hist(dataset$speechiness, breaks = 30, main = "Distribution of Speechiness",
     xlab = "Speechiness", col = "gold", border = "white")

# Histogram for Valence
hist(dataset$valence, breaks = 30, main = "Distribution of Valence",
     xlab = "Valence", col = "lightpink", border = "white")

# Histogram for Instrumentalness
hist(dataset$instrumentalness, breaks = 30, main = "Distribution of Instrumentalness",
     xlab = "Instrumentalness", col = "lightcyan", border = "white")

# Histogram for loudness
hist(dataset$loudness, breaks = 30, main = "Distribution of loudness",
     xlab = "loudness", col = "lightcyan", border = "white")

dataset$log_speechiness <- log(dataset$speechiness + 1)

dataset$log_instrumentalness <- log(dataset$instrumentalness + 1)

# After log transformation
transformed_speechiness_skewness <- skewness(dataset$log_speechiness, na.rm = TRUE)

# Print results
cat("Transformed Speechiness Skewness:", transformed_speechiness_skewness, "\n")

# After log transformation
transformed_instrumentalness_skewness <- skewness(dataset$log_instrumentalness, na.rm = TRUE)

cat("Transformed instrumentalness Skewness:", transformed_instrumentalness_skewness, "\n")

install.packages("caTools")
library(caTools)

# Select relevant numeric features and the target variable
linear_data <- dataset %>% 
  select(popularity, danceability, time_signature, 
         tempo, speechiness, valence, instrumentalness )

# Verify the structure
str(linear_data)

set.seed(123)  # For reproducibility

# Split the data (70% train, 30% test)

split <- sample.split(linear_data$popularity, SplitRatio = 0.7)

train_data <- subset(linear_data, split == TRUE)
test_data <- subset(linear_data, split == FALSE)

# Check dimensions
print(dim(train_data))
print(dim(test_data))

# Train the Linear Regression model
lm_model <- lm(popularity ~ ., data = train_data)

# View the model summary
summary(lm_model)

# Predict on the test set
test_data$predicted_popularity <- predict(lm_model, newdata = test_data)

# View actual vs predicted values
head(test_data[, c("popularity", "predicted_popularity")])

# Load the metrics library
install.packages("Metrics")
library(Metrics)
# Calculate RMSE, MAE, and R-squared
rmse_value <- rmse(test_data$popularity, test_data$predicted_popularity)
mae_value <- mae(test_data$popularity, test_data$predicted_popularity)
r_squared <- cor(test_data$popularity, test_data$predicted_popularity)^2

# Print the metrics
print(paste("RMSE:", round(rmse_value, 2)))
print(paste("MAE:", round(mae_value, 2)))
print(paste("R-squared:", round(r_squared, 2)))

# Visualize actual vs predicted values
plot(test_data$popularity, test_data$predicted_popularity,
     main = "Actual vs Predicted Values",
     xlab = "Actual Popularity",
     ylab = "Predicted Popularity",
     col = "blue", pch = 16)
abline(0, 1, col = "red", lwd = 2)  # Add diagonal line

# Correlation matrix for numeric predictors
numeric_data <- linear_data %>% select(-popularity)  # Exclude the target variable
cor_matrix <- cor(numeric_data)

# Print the correlation matrix
print(cor_matrix)

install.packages("randomForest")
install.packages("caret")
library(randomForest)
library(caret)

preProc <- preProcess(dataset[, sapply(dataset, is.numeric)], method = c("center", "scale"))
scaled_data <- predict(preProc, dataset)

numeric_data <- dataset %>% select(where(is.numeric))  # Select all numeric columns

set.seed(123)


# Split the data
split <- sample.split(numeric_data$popularity, SplitRatio = 0.7)
train_data <- subset(numeric_data, split == TRUE)
test_data <- subset(numeric_data, split == FALSE)

# Train Random Forest model
f_model <- randomForest(popularity ~ ., 
                        data = train_data, 
                        ntree = 500, 
                        importance = TRUE, 
                        do.trace = 10)  # Show progress every 10 trees

# View model summary
print(f_model)

# Predict on the test set
test_data$predicted_popularity <- predict(f_model, newdata = test_data)

# Calculate evaluation metrics

rmse_value <- rmse(test_data$popularity, test_data$predicted_popularity)
mae_value <- mae(test_data$popularity, test_data$predicted_popularity)
r_squared <- cor(test_data$popularity, test_data$predicted_popularity)^2

# Print metrics
cat("Random Forest Metrics:\n")
cat("RMSE:", round(rmse_value, 2), "\n")
cat("MAE:", round(mae_value, 2), "\n")
cat("R-squared:", round(r_squared, 2), "\n")

# Print the metrics
print(paste("RMSE:", round(rmse_value, 2)))
print(paste("MAE:", round(mae_value, 2)))
print(paste("R-squared:", round(r_squared, 2)))

# View variable importance
importance(f_model)

# Plot feature importance
varImpPlot(f_model)

plot(test_data$popularity, predict(f_model, newdata = test_data), main = "Actual vs Predicted Values", xlab = "Actual Popularity", ylab = "Predicted Popularity", col = "blue", pch = 16); abline(0, 1, col = "red", lwd = 2)
