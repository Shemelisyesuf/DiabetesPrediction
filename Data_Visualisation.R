# Load required libraries
library(tidyverse)
library(ggplot2)
library(plotly)
library(GGally)

# Load the dataset
library(readr)
diabetes_clean <- read_csv("diabetes_clean.csv")

# Summary of the dataset
print(dim(diabetes_clean))
print(summary(diabetes_clean))

# -------------------- Static Visualizations --------------------

# Combined boxplot for all variables against Outcome
melted_data <- diabetes_clean %>% 
  pivot_longer(cols = -Outcome, names_to = "Variable", values_to = "Value")

ggplot(melted_data, aes(x = Variable, y = Value, fill = factor(Outcome))) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  labs(title = "Distribution of Variables by Outcome", x = "Variable", y = "Value", fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Correlation Heatmap
correlation_matrix <- cor(diabetes_clean[, sapply(diabetes_clean, is.numeric)])
corrplot::corrplot(correlation_matrix, method = "color", addCoef.col = "black", 
                   title = "Correlation Heatmap", mar = c(0, 0, 1, 0))

# Pairwise Scatter Plots (Static)
selected_columns <- c("Pregnancies", "Glucose", "Blood_Pressure", "BMI", "Age","Skin_Thickness","Insulin","Diabetes_Pedigree_Function")
diabetes_selected <- diabetes_clean %>% select(all_of(selected_columns), Outcome)

ggpairs(diabetes_selected, aes(color = factor(Outcome), alpha = 0.5)) +
  ggtitle("Pairwise Scatter Plots") +
  theme_minimal()

# -------------------- Interactive Visualizations --------------------

# 3D Scatter Plot: Glucose, BMI, Age
plot_ly(data = diabetes_clean, 
        x = ~Glucose, y = ~BMI, z = ~Age, 
        color = ~factor(Outcome), 
        size = ~Blood_Pressure, 
        type = "scatter3d", 
        mode = "markers",
        marker = list(opacity = 0.7)) %>%
  layout(title = "3D Scatter Plot: Glucose vs BMI vs Age by Outcome",
         scene = list(xaxis = list(title = "Glucose"),
                      yaxis = list(title = "BMI"),
                      zaxis = list(title = "Age")))

# Parallel Coordinates Plot
plot_ly(data = diabetes_clean, type = "parcoords",
        line = list(color = ~Outcome,
                    colorscale = list(c(0, 1), c("blue", "red"))),
        dimensions = list(
          list(label = "Pregnancies", values = ~Pregnancies),
          list(label = "Glucose", values = ~Glucose),
          list(label = "Blood Pressure", values = ~Blood_Pressure),
          list(label = "Skin Thickness", values = ~Skin_Thickness),
          list(label = "Insulin", values = ~Insulin),
          list(label = "BMI", values = ~BMI),
          list(label = "Diabetes Pedigree Function", values = ~Diabetes_Pedigree_Function),
          list(label = "Age", values = ~Age))) %>%
  layout(title = "Parallel Coordinates Plot: All Variables by Outcome")

# Scatter Matrix for Key Variables
plot_ly(data = diabetes_clean, type = "splom",
        dimensions = list(
          list(label = "Glucose", values = ~Glucose),
          list(label = "BMI", values = ~BMI),
          list(label = "Age", values = ~Age),
          list(label = "Blood Pressure", values = ~Blood_Pressure)),
        color = ~factor(Outcome),
        marker = list(opacity = 0.7)) %>%
  layout(title = "Scatter Matrix: Key Variables by Outcome")

# -------------------- Saving Static Visualizations --------------------
ggsave("boxplot_variables_outcome.png", height = 6, width = 12)
