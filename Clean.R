#import and view data set

library(readr)
diabetes <- read_csv("diabetes.csv")
View(diabetes)

library(dplyr)

# Clean dataset and  Removes duplicate rows if any

diabetes_cleaned <- diabetes %>%
  filter(
    Glucose != 0,
    BloodPressure != 0,
    SkinThickness != 0,
    Insulin != 0,
    BMI != 0,
    DiabetesPedigreeFunction != 0,
    Age != 0
  ) %>%
  distinct()

# Check for remaining duplicates

any_duplicates <- diabetes_cleaned %>%
  distinct() %>%
  nrow() != nrow(diabetes_cleaned)

print(any_duplicates) 

#check data type and clean

str(diabetes_cleaned)

# Fix Age as integer
diabetes_cleaned <- diabetes_cleaned %>%
  mutate(Age = as.integer(Age))


# Verify changes
str(diabetes_cleaned)

# Rename variables to include spaces
diabetes_cleaned <- diabetes_cleaned %>%
  rename(
    "Blood Pressure" = BloodPressure,
    "Skin Thickness" = SkinThickness,
    "Diabetes Pedigree Function" = DiabetesPedigreeFunction
  )

# View the updated column names
names(diabetes_cleaned)

# Randomly select 10 observations from the cleaned dataset
set.seed(123)  # Set a seed for reproducibility
sampled_data <- diabetes_cleaned %>%
  sample_n(10)

# Save the sampled data as a new dataset
write.csv(sampled_data, "cleaned_sampled_data.csv", row.names = FALSE)

# Display the sampled data
sampled_data
