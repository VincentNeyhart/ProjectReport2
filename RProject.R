# Load libraries I may or may not need
library(readr)
library(ggplot2)
library(cluster)
library(caret)
library(corrplot)

# Load data
yelp_data <- read_csv("C:/Users/Chenz/Desktop/r/OA 11.6 - yelp_academic_dataset_user.json.csv")
View(yelp_data)
# Print column names + basic structure of the dataset
print(colnames(yelp_data))
str(yelp_data)  # (Basic structure of data)


summary(yelp_data)

# Calculate Pearson Corr between cool votes, funny votes, and useful votes
cor_cool_funny <- cor(yelp_data$cool_votes, yelp_data$funny_votes, use = "complete.obs")
cor_cool_useful <- cor(yelp_data$cool_votes, yelp_data$useful_votes, use = "complete.obs")
cor_funny_useful <- cor(yelp_data$funny_votes, yelp_data$useful_votes, use = "complete.obs")

# Correlation results
cat("Correlation between Cool and Funny Votes:", cor_cool_funny, "\n")
cat("Correlation between Cool and Useful Votes:", cor_cool_useful, "\n")
cat("Correlation between Funny and Useful Votes:", cor_funny_useful, "\n")

# Visualize: Cool vs Funny Votes
ggplot(yelp_data, aes(x = funny_votes, y = cool_votes)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  labs(title = "Relationship Between Funny Votes and Cool Votes",
       x = "Funny Votes", y = "Cool Votes")

# Linear regression: Review Count vs Fans
lm_review_fans <- lm(fans ~ review_count, data = yelp_data)
summary(lm_review_fans)
ggplot(yelp_data, aes(x = review_count, y = fans)) +
  geom_point(color = "green", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  labs(title = "Linear Regression: Review Count vs Fans",
       x = "Review Count", y = "Fans")

# Another variable to predict fans (Average Stars)
lm_stars_fans <- lm(fans ~ average_stars, data = yelp_data)
summary(lm_stars_fans)
ggplot(yelp_data, aes(x = average_stars, y = fans)) +
  geom_point(color = "red", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Linear Regression: Average Stars vs Fans",
       x = "Average Stars", y = "Fans")

# K-means clustering for Review Count & Fans
kmeans_data <- yelp_data[, c("review_count", "fans")]
kmeans_data <- na.omit(kmeans_data)  # Remove NA values
kmeans_result <- kmeans(kmeans_data, centers = 3, nstart = 20)

# Add cluster info to dataset
kmeans_data$cluster <- as.factor(kmeans_result$cluster)

# Actually see the clustering
ggplot(kmeans_data, aes(x = review_count, y = fans, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "K-means Clustering: Review Count and Fans",
       x = "Review Count", y = "Fans")

# K-means clustering - Average Stars and Fans
kmeans_data2 <- yelp_data[, c("average_stars", "fans")]
kmeans_data2 <- na.omit(kmeans_data2)
kmeans_result2 <- kmeans(kmeans_data2, centers = 3, nstart = 20)

# Add cluster info to dataset
kmeans_data2$cluster <- as.factor(kmeans_result2$cluster)

# Actually see clustering
ggplot(kmeans_data2, aes(x = average_stars, y = fans, color = cluster)) +
  geom_point(size = 2) +
  labs(title = "K-means Clustering: Average Stars and Fans",
       x = "Average Stars", y = "Fans")


