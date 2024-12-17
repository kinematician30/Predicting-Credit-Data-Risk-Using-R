# German Credit Risk Classification Analysis

# Required Libraries
library(tidyverse)   # Data manipulation
library(caret)       # Machine learning workflows
library(rpart)       # Decision Tree algorithm
library(rpart.plot)  # Decision Tree visualization
library(e1071)       # Additional model evaluation tools

# 1. Exploratory Data Analysis (EDA)
# Load the dataset - Aleady Loaded
# credit_data <- read.csv("credit_data_recode.csv", stringsAsFactors = TRUE)


# Distribution of target variable
table(credit_data_recode$assessment)
prop.table(table(credit_data_recode$assessment))

# Visualize categorical variable distributions
categorical_vars <- c("status", "credit_history", "purpose", "savings_account", 
                      "employment", "personal_status", "housing", "job")

plot_categorical_distribution <- function(data, var) {
  ggplot(data, aes(x = !!sym(var), fill = assessment)) +
    geom_bar(position = "fill") +
    theme_minimal() +
    labs(title = paste("Distribution of", var, "by Credit Assessment"),
         y = "Proportion") +
    coord_flip()
}

# Create plots for each categorical variable
categorical_plots <- lapply(categorical_vars, function(var) {
  plot_categorical_distribution(credit_data_recode, var)
})


# 2. Feature Engineering
# Handle missing values (if any)
credit_data_recode <- credit_data_recode %>%
  mutate(across(where(is.character), ~replace_na(., "unknown")))

# Create age groups
credit_data_recode <- credit_data_recode %>%
  mutate(age_group = cut(age, 
                         breaks = c(0, 25, 35, 45, 55, Inf), 
                         labels = c("18-25", "26-35", "36-45", "46-55", "55+")
                         )
         )

# Encoding categorical variables
credit_data_recode <- credit_data_recode %>%
  mutate(
    status_encoded = as.numeric(factor(status)),
    credit_history_encoded = as.numeric(factor(credit_history)),
    purpose_encoded = as.numeric(factor(purpose)),
    savings_account_encoded = as.numeric(factor(savings_account)),
    employment_encoded = as.numeric(factor(employment))
  )


# 3. Model Building
# Set seed for reproducibility
set.seed(123)

# Shuffle data before splitting
credit_data_recode <- credit_data_recode[sample(nrow(credit_data_recode)), ]

# Split data into training and testing sets
index <- createDataPartition(credit_data_recode$assessment, p = 0.7, list = FALSE)
train_data <- credit_data_recode[index, ]
test_data <- credit_data_recode[-index, ]

# Prepare features and target
features <- c("duration", "credit_amount", "installment_rate", "age", 
              "num_credits", "status_encoded", "credit_history_encoded", 
              "purpose_encoded", "savings_account_encoded", "employment_encoded")

# Decision Tree Model
dt_model <- rpart(
  formula = as.factor(assessment) ~ ., 
  data = train_data[, c(features, "assessment")],
  method = "class",
  control = rpart.control(maxdepth = 5)
)

# Plot decision tree
rpart.plot(dt_model, 
           main = "Credit Risk Decision Tree", 
           fallen.leaves = TRUE)

# 4. Model Evaluation
# Predictions
predictions <- predict(dt_model, test_data[, features], type = "class")

# Confusion Matrix
conf_matrix <- confusionMatrix(predictions, as.factor(test_data$assessment))
print(conf_matrix)

# Additional Performance Metricss 
accuracy <- conf_matrix$overall['Accuracy']
precision <- conf_matrix$byClass['Precision']
recall <- conf_matrix$byClass['Recall']
f1_score <- conf_matrix$byClass['F1']

# Performance Summary
performance_summary <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Value = c(accuracy, precision, recall, f1_score)
)
print(performance_summary)