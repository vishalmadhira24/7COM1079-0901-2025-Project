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

# Step 5: Check normality for t-test assumption (using Anderson-Darling test)
own_minutes <- data %>% filter(own_goal == TRUE) %>% pull(minute)
regular_minutes <- data %>% filter(own_goal == FALSE) %>% pull(minute)

ad_own <- ad.test(own_minutes)
ad_regular <- ad.test(regular_minutes)
print(ad_own)    # p < 0.05 means not normal
print(ad_regular)

# Step 6: Visualizations
# Main plot for RQ: Boxplot of minutes by own_goal status
# (Informative title, labels, legend; output of R script)
boxplot_plot <- ggplot(data, aes(x = own_goal, y = minute, fill = own_goal)) +
  geom_boxplot() +
  labs(title = "Boxplot of Goal Minutes by Own Goal Status in Women's International Football",
       x = "Own Goal (TRUE/FALSE)",
       y = "Minute of Goal",
       fill = "Own Goal") +
  theme_minimal() +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red"))  # Custom colors for clarity
print(boxplot_plot)  # Displays in RStudio Plots pane

# Supplementary: Histograms for distribution of minutes (one combined, overlaid)
histogram_combined <- ggplot(data, aes(x = minute, fill = own_goal)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Histogram of Goal Minutes by Own Goal Status",
       x = "Minute",
       y = "Count",
       fill = "Own Goal") +
  theme_minimal() +
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red"))
print(histogram_combined)

# Additional optional plot: Separate histograms for clarity
histogram_regular <- ggplot(data %>% filter(own_goal == FALSE), aes(x = minute)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Histogram of Minutes for Regular Goals",
       x = "Minute", y = "Count") +
  theme_minimal()
print(histogram_regular)

histogram_own <- ggplot(data %>% filter(own_goal == TRUE), aes(x = minute)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.7) +
  labs(title = "Histogram of Minutes for Own Goals",
       x = "Minute", y = "Count") +
  theme_minimal()
print(histogram_own)

# Additional insights: Density plot for smoothed distribution
density_plot <- ggplot(data, aes(x = minute, color = own_goal)) +
  geom_density() +
  labs(title = "Density Plot of Goal Minutes by Own Goal Status",
       x = "Minute", y = "Density",
       color = "Own Goal") +
  theme_minimal()
print(density_plot)

# Step 7: Statistical Test for Hypotheses
# Two-sample t-test (assuming normality; if not, use Wilcoxon below)
t_test_result <- t.test(minute ~ own_goal, data = data, alternative = "two.sided")
print(t_test_result)  # p-value to decide reject/fail to reject H0

# If normality failed: Non-parametric alternative (Wilcoxon rank-sum test)
wilcox_result <- wilcox.test(minute ~ own_goal, data = data, alternative = "two.sided")
print(wilcox_result)

# Step 8: Additional data understanding (optional section)
# Proportion of own goals overall
prop_own <- mean(data$own_goal == TRUE)
print(paste("Overall proportion of own goals:", round(prop_own, 4)))

# Own goals proportion over years (line plot for trends)
own_by_year <- data %>%
  group_by(year) %>%
  summarise(
    total_goals = n(),
    own_goals = sum(own_goal == TRUE),
    prop_own = own_goals / total_goals
  )
print(own_by_year)

own_by_year_plot <- ggplot(own_by_year, aes(x = year, y = prop_own)) +
  geom_line(color = "purple") +
  labs(title = "Proportion of Own Goals Over Years in Women's Football",
       x = "Year", y = "Proportion of Own Goals") +
  theme_minimal()
print(own_by_year_plot)

# Home vs Away additional stats (for deeper insights)
home_away_stats <- data %>%
  group_by(goal_for) %>%
  summarise(
    count = n(),
    mean_minute = mean(minute),
    prop_own = mean(own_goal == TRUE),
    prop_penalty = mean(penalty == TRUE)
  )
print(home_away_stats)

# Contingency table for own_goal and penalty (if exploring proportions)
cont_table <- table(data$own_goal, data$penalty)
print(cont_table)

# Chi-square test (optional, for association between own_goal and penalty)
chi_test <- chisq.test(cont_table)
print(chi_test)

# Goals per match aggregation (for overall data understanding)
matches <- data %>%
  group_by(date, home_team, away_team) %>%
  summarise(goals_per_match = n(), .groups = "drop")

summary_matches <- summary(matches$goals_per_match)
print(summary_matches)
