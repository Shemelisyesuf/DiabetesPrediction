# Load Required Libraries
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("randomForest", quietly = TRUE)) install.packages("randomForest")
if (!requireNamespace("GGally", quietly = TRUE)) install.packages("GGally")
if (!requireNamespace("pROC", quietly = TRUE)) install.packages("pROC")
if (!requireNamespace("conflicted", quietly = TRUE)) install.packages("conflicted")

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(randomForest)
library(GGally)
library(pROC)
library(conflicted)

# Set package preferences to avoid conflicts
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# Load the Cleaned Data
cleaned_sampled_data <- read_csv("cleaned_sampled_data.csv")
View(cleaned_sampled_data)

# Prepare Data for Visualization
Clean_data <- cleaned_sampled_data %>%
  pivot_longer(cols = -Outcome, names_to = "variable", values_to = "value") %>%
  mutate(
    value = as.numeric(value),
    Outcome = as.factor(Outcome))

# 1. Visualize Data: Line Graphs
ggplot(Clean_data, aes(x = value, y = as.numeric(as.character(Outcome)), color = Outcome, group = Outcome)) +
  geom_line(alpha = 0.6, size = 0.8) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(
    title = "Line Graphs of Variables against Outcome",
    x = "Value",
    y = "Outcome",
    color = "Outcome")

# 2. Enhanced Scatter Plot with Bubble-like Dots
ggplot(Clean_data, aes(x = value, y = Outcome, size = abs(value), color = Outcome)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(
    title = "Bubble Plot of Variables by Outcome",
    x = "Value",
    y = "Outcome",
    size = "Value Magnitude",
    color = "Outcome")

# Summary Statistics by Variable
grouped_summary_stats <- Clean_data %>%
  group_by(variable) %>%
  summarize(
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    count = n(),
    .groups = "drop")
print(grouped_summary_stats)

# Correlation Heatmap
correlation_matrix <- cor(cleaned_sampled_data %>% select(where(is.numeric)), use = "complete.obs")
corrplot(correlation_matrix, method = "circle", type = "lower", tl.cex = 0.8, main = "Correlation Heatmap")


# Pair Plot
ggpairs(
  cleaned_sampled_data %>% mutate(Outcome = as.factor(Outcome)),
  columns = 1:6,
  aes(color = Outcome))

# Logistic Regression
log_model <- glm(Outcome ~ ., data = cleaned_sampled_data, family = binomial)
summary(log_model)

# Extract and Visualize Coefficients
coefficients_df <- as.data.frame(summary(log_model)$coefficients)
coefficients_df <- coefficients_df[order(-abs(coefficients_df[, "Estimate"])), ]
colnames(coefficients_df) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")

ggplot(coefficients_df, aes(x = reorder(row.names(coefficients_df), Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Logistic Regression Coefficients", x = "Predictors", y = "Coefficient Estimate") +
  theme_minimal()

print(colnames(cleaned_sampled_data))


# Random Forest Model
rf_model <- randomForest(Outcome ~ ., data = Clean_data, ntree = 100)

# Feature Importance
importance <- as.data.frame(rf_model$importance)
ggplot(importance, aes(x = reorder(rownames(importance), MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  coord_flip() +
  labs(title = "Random Forest Feature Importance", x = "Features", y = "Importance") +
  theme_minimal()

# ROC Curve
predicted_probabilities <- predict(log_model, newdata = cleaned_sampled_data, type = "response")
roc_curve <- roc(cleaned_sampled_data$Outcome, predicted_probabilities)
plot(roc_curve, col = "blue", main = "ROC Curve")
print(paste("AUC:", auc(roc_curve)))
