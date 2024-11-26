# Load necessary Libraries
library(dplyr) # Data Manipulation & Pre processing
library(ggplot2) # Data Visualization
library(readr) # Data Collection

# Data Collection
# Reading the data as is; through a .data file format; No column name was given, refer to the data description.
credit_data <- read_delim('./german_data/german.data', delim = " ", col_names = FALSE)

# Column names
col_names <- c("status", "duration", "credit_history", "purpose", "credit_amount", "savings_account", "employment", "installment_rate", "personal_status", "guarantors", "residence_since", "property", "age", "installment_plans", "housing", "num_credits", "job", "dependents", "telephone", "foreign_worker", 'assessment')

# Attaching column names to dataframe
colnames(credit_data) <- col_names

# Type Conversion
chars_col <- sapply(credit_data, is.character) # find all the characters columns

credit_data[, chars_col] <- lapply(credit_data[, chars_col], as.factor)


# Information on the credit data
# Structure of the dataset
print(str(credit_data))

# dimension of the dataset
print(dim(credit_data))

# Summary
print(summary(credit_data))

# Check for missing value
lapply(credit_data, is.null)
