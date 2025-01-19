# Load the Cleaned Sampled Dataset
library(readr)
clean_data <- read_csv("cleaned_sampled_data.csv")
View(c)
install.packages("reshape2")
library(reshape2)
# Prepare Data for Visualization
# Load tidyr for pivot_longer

ed

# Melt the dataset for visualization
Clean_data <- clean_data %>%
  melt(id.vars = "Outcome")

# Convert Outcome to binary numeric (1 for Positive, 0 for Negative)
Clean_data <- Clean_data %>%
  mutate(Outcome = ifelse(Outcome == "Positive", 1, 0))

# Convert value column to numeric explicitly
Clean_data <- Clean_data %>%
  mutate(value = as.numeric(value))

#  Visualize the Data
# Plot distributions of variables by Outcome
ggplot(Clean_data, aes(x = value, fill = as.factor(Outcome))) +
  geom_histogram(position = "dodge", bins = 30) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(
    title = "Distributions of Variables by Outcome",
    x = "Value",
    y = "Count",
    fill = "Outcome")

# Summary Statistics
# Calculate summary statistics grouped by Outcome and Variable
summary_stats <- Clean_data %>%
  group_by(Outcome, variable) %>%
  summarize(
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    .groups = "drop")
print(summary_stats)

# Boxplot of Glucose levels by Outcome
ggplot(clean_data, aes(y = as.factor(Outcome), x = Glucose, BMI, fill = as.factor(Outcome))) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Boxplot of Glucose Levels by Outcome",
    y = "Outcome",
    x = "Glucose", "BMI", "skin thickness", "glucos", "blood pressure", "insulin", "diabetes pedigree function",
    "age", "pregnancies", fill = "Outcome")

# ================================
# Step 8: Correlation Heatmap
# ================================
library(corrplot)
# Calculate correlation matrix for numeric columns
correlation_matrix <- cor(cleaned_sampled_data %>% select(where(is.numeric)))
# Plot correlation heatmap
corrplot(correlation_matrix, method = "circle", type = "lower", tl.cex = 0.8)

# ================================
# Step 9: Pair Plot
# ================================
library(GGally)
# Generate pair plot for selected variables and Outcome
ggpairs(cleaned_sampled_data, columns = 1:6, aes(color = as.factor(Outcome)))