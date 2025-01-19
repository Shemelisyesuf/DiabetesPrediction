# Import Required Libraries
library(readr)
library(dplyr)

# Import and View the Dataset
diabetes <- read_csv("diabetes.csv")
View(diabetes)

# Data Cleaning
## Remove rows with zero values in key variables
diabetes_cleaned <- diabetes %>%
  filter(
    Glucose != 0,
    BloodPressure != 0,
    SkinThickness != 0,
    Insulin != 0,
    BMI != 0,
    DiabetesPedigreeFunction != 0,
    Age != 0) %>%
  distinct()  

# Remove duplicate rows
## Check for Remaining Duplicates
any_duplicates <- diabetes_cleaned %>%
  distinct() %>%
  nrow() != nrow(diabetes_cleaned)
print(any_duplicates)

# Rename Variables
diabetes_cleaned <- diabetes_cleaned %>%
  rename(
    "Blood Pressure" = BloodPressure,
    "Skin Thickness" = SkinThickness,
    "Diabetes Pedigree Function" = DiabetesPedigreeFunction)

## View the Updated Column Names
names(diabetes_cleaned)

# Random Sampling
## Randomly Select 10 Observations from the Cleaned Dataset
set.seed(123)
sampled_data <- diabetes_cleaned %>%
  sample_n(10)

## Save the Sampled Data as a New CSV File
write.csv(sampled_data, "cleaned_sampled_data.csv", row.names = FALSE)

## Display the Sampled Data
sampled_data
