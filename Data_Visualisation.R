# Load required libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(corrplot)

# Load the cleaned dataset
library(readr)
diabetes_clean <- read_csv("diabetes_clean.csv")

# Summary of the cleaned dataset
print(dim(diabetes_clean))
print(head(diabetes_clean))
print(summary(diabetes_clean))

# Correlation heatmap
correlation_matrix <- cor(diabetes_clean[, sapply(diabetes_clean, is.numeric)])
corrplot(correlation_matrix, method = "color", addCoef.col = "black", 
         title = "Correlation Heatmap", mar = c(0, 0, 1, 0))

# Scatter plot of Glucose vs Blood Pressure colored by Outcome
ggplot(diabetes_clean, aes(x = Glucose, y = Blood_Pressure, color = factor(Outcome))) +
  geom_point(size = 2, alpha = 0.7) +
  labs(title = "Scatter Plot: Glucose vs Blood Pressure", color = "Outcome") +
  theme_minimal()

# Histogram of BMI grouped by Outcome
ggplot(diabetes_clean, aes(x = BMI, fill = factor(Outcome))) +
  geom_histogram(binwidth = 2, position = "dodge", alpha = 0.7) +
  labs(title = "Distribution of BMI by Outcome", fill = "Outcome") +
  theme_minimal()

# Boxplot of Age by Outcome
ggplot(diabetes_clean, aes(x = factor(Outcome), y = Age, fill = factor(Outcome))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplot of Age by Outcome", x = "Outcome", y = "Age", fill = "Outcome") +
  theme_minimal()

# Pairwise scatter plots for selected features
selected_columns <- c("Pregnancies", "Glucose", "Blood_Pressure", "BMI")
pairs_data <- diabetes_clean[, selected_columns]
pairs(pairs_data, main = "Pairwise Scatter Plots")

# Interactive scatter plot using Plotly
plot <- plot_ly(data = diabetes, 
                x = ~Glucose, 
                y = ~Blood_Pressure, 
                color = ~factor(Outcome), 
                size = ~BMI, 
                type = "scatter", 
                mode = "markers", 
                hoverinfo = "text",
                text = ~paste("Pregnancies:", Pregnancies, 
                              "<br>Age:", Age,
                              "<br>Outcome:", Outcome)) %>%
  layout(title = "Interactive Scatter Plot: Glucose vs Blood Pressure",
         xaxis = list(title = "Glucose"),
         yaxis = list(title = "Blood Pressure"))

# Save static plots to files
ggsave("scatter_plot_glucose_bp.png")
ggsave("bmi_histogram_by_outcome.png")
ggsave("age_boxplot_by_outcome.png")
