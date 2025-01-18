# Load necessary libraries
install.packages("corrplot")
install.packages("gglot2")
library(ggplot2)
library(corrplot)

# Load cleaned dataset
library(readr)
cleaned_dataset <- read_csv("cleaned_dataset.csv")
View(cleaned_dataset)

# Select only numeric columns
numeric_data <- cleaned_dataset %>% select_if(is.numeric)

# Step 1: Correlation Matrix
cor_matrix <- cor(numeric_data %>% select(-Outcome))
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

# Step 2: Visualize distributions
data %>%
  gather(key = "Feature", value = "Value", -Outcome) %>%
  ggplot(aes(x = Value, fill = as.factor(Outcome))) +
  geom_histogram(bins = 30, position = "dodge") +
  facet_wrap(~ Feature, scales = "free") +
  labs(title = "Feature Distributions by Outcome", fill = "Outcome") +
  theme_minimal()
