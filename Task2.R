# Titanic Dataset: Data Cleaning & Exploratory Data Analysis (EDA)

# Load Required Libraries
library(dplyr)        # Data manipulation
library(ggplot2)      # Data visualization
library(tidyr)        # Data cleaning
library(DataExplorer) # Automated EDA

# Load the Dataset
titanic <- read.csv("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")

# Display Initial Data Overview
cat("\nInitial Data Overview:\n")
print(str(titanic))
print(summary(titanic))
print(dim(titanic))
print(head(titanic))

# Handle Missing Values
cat("\nHandling Missing Values:\n")
print(colSums(is.na(titanic)))  # Check missing values

titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)  # Impute Age with median
titanic$Embarked[is.na(titanic$Embarked)] <- "S"  # Fill missing Embarked values with most common port
titanic <- titanic %>% select(-Cabin)  # Drop Cabin due to excessive missing values

# Convert Categorical Variables
titanic <- titanic %>% mutate(
  Survived = as.factor(Survived),
  Pclass = as.factor(Pclass),
  Sex = as.factor(Sex),
  Embarked = as.factor(Embarked)
)

# Exploratory Data Analysis (EDA)
cat("\nGenerating Visualizations:\n")

# Survival Count
ggplot(titanic, aes(x = Survived)) + 
  geom_bar(fill = "#2E86C1") + 
  ggtitle("Survival Count") + 
  theme_minimal()

# Survival by Gender
ggplot(titanic, aes(x = Sex, fill = Survived)) + 
  geom_bar(position = "dodge") + 
  ggtitle("Survival by Gender") + 
  theme_minimal()

# Age Distribution
ggplot(titanic, aes(x = Age)) + 
  geom_histogram(bins = 30, fill = "#1ABC9C", color = "black") + 
  ggtitle("Age Distribution") + 
  theme_minimal()

# Survival by Passenger Class
ggplot(titanic, aes(x = Pclass, fill = Survived)) + 
  geom_bar(position = "dodge") + 
  ggtitle("Survival by Passenger Class") + 
  theme_minimal()

# Correlation Analysis Between Numerical Variables
numeric_vars <- titanic %>% select_if(is.numeric)
cor_matrix <- cor(numeric_vars, use = "complete.obs")
cat("\nCorrelation Matrix:\n")
print(cor_matrix)

# Generate Automated EDA Report
cat("\nGenerating Automated EDA Report:\n")
create_report(titanic)
