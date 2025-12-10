# Load required libraries
library(ggplot2)   # For visualizations
library(dplyr)     # For data manipulation
library(lubridate) # For date handling
library(nortest)   # For normality tests

# Step 1: Load the dataset
data <- read.csv("dataset female footballers.csv")

# Step 2: Explore the data structure and summary
str(data)          # Check data types and structure
summary(data)      # Basic summary statistics
head(data, 10)     # View first 10 rows

