# Install and Load necessary libraries
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caret")
install.packages("randomForest")
install.packages("shiny")

library(dplyr)  
library(ggplot2) 
library(caret)  
library(randomForest)  
library(shiny)

#import and explore the dataset
library(readr)
diabetes <- read_csv("diabetes.csv")
print(dim(diabetes))
print(head(diabetes))
print(summary(diabetes))
print(str(diabetes))

# Rename columns with underscores
colnames(diabetes) <- c("Pregnancies", "Glucose", "Blood_Pressure", "Skin_Thickness", 
                        "Insulin", "BMI", "Diabetes_Pedigree_Function", "Age", "Outcome")

# Remove rows with zero values in specific columns
columns_to_check <- c("Glucose", "Blood_Pressure", "Skin_Thickness", "Insulin", "BMI")
diabetes_clean <- diabetes %>% 
  filter(if_all(all_of(columns_to_check), ~ . != 0))

# Check the cleaned data
print(dim(diabetes_clean))
print(summary(diabetes_clean))

# Save the cleaned dataset
write.csv(diabetes_clean, "diabetes_clean.csv", row.names = FALSE)
