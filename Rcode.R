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

# Step 3: Clean and preprocess the data
# Convert date to Date format
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# Extract year from date for potential time-based analysis
data$year <- year(data$date)

# Determine if the goal is for home or away team (for additional insights)
data$goal_for <- ifelse(data$team == data$home_team, "Home", "Away")

# Filter out invalid or extreme minutes (assuming matches up to 120 mins incl. extra time)
data <- data %>% filter(minute >= 1 & minute <= 120)

# Convert logical columns to factors for better plotting
data$own_goal <- as.factor(data$own_goal)
data$penalty <- as.factor(data$penalty)

# Handle any missing values (if any; dataset seems clean)
data <- na.omit(data)

# Step 4: Summary statistics for RQ (minutes by own_goal)
own_goal_stats <- data %>%
  group_by(own_goal) %>%
  summarise(
    count = n(),
    mean_minute = mean(minute),
    sd_minute = sd(minute),
    median_minute = median(minute),
    min_minute = min(minute),
    max_minute = max(minute)
  )
print(own_goal_stats)

