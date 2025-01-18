# Load necessary libraries
install.packages("corrplot")
install.packages("gglot2")
install.packages("reshape2")
install.packages("corrplot")
install.packages("randomForest")
install.packages("broom")
install.packages("dplyr")
library(ggplot2)
library(corrplot)
library(ggplot2)
library(reshape2)
library(randomForest)
library(dplyr)

# Load cleaned dataset
library(readr)
cleaned_sampled_data <- read_csv("cleaned_sampled_data.csv")
View(cleaned_sampled_data)

library(ggplot2)

# Melt the dataset for visualization
library(reshape2)
Clean_data <- cleaned_sampled_data %>%
  melt(id.vars = "Outcome")

# Convert Outcome to numeric with 1 for "Positive" and 0 for "Negative"
Clean_data <- Clean_data %>%
  mutate(Outcome = ifelse(Outcome == "Positive", 1, 0))

# Convert value to numeric explicitly (if needed)
Clean_data <- Clean_data %>%
  mutate(value = as.numeric(value))

# Plot distributions by Outcome
ggplot(Clean_data, aes(x = value, fill = as.factor(Outcome))) +
  geom_histogram(position = "dodge", bins = 30) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(title = "Distributions of Variables by Outcome",
       x = "Value",
       y = "Count")

# Summary statistics by Outcome
Clean_data %>%
  group_by(Outcome, variable) %>%
  summarize(
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    .groups = "drop")

# Convert Outcome to binary numeric (if not already done)
Clean_data <- Clean_data %>%
  mutate(Outcome = ifelse(Outcome == "Positive", 1, 0))

# Ensure Outcome is a factor for classification
Clean_data <- Final_data %>%
  mutate(Outcome = as.factor(Outcome))
