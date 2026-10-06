# Read data
data <- read.csv("roadrace.csv")

table(data$Maine)

# Create the bar plot
barplot(
  table(data$Maine),
  main = "Runners from Maine vs. Away",
  xlab = "Runner Location",
  ylab = "Number of Runners"
)

maine_times <- data$Time..minutes.[data$Maine == "Maine"]
away_times <- data$Time..minutes.[data$Maine == "Away"]

breaks <- seq(20, 160, by = 10)

# Histogram for Maine
hist(maine_times,
     breaks = breaks,
     freq = FALSE,
     xlim = range(data$Time..minutes.),
     main = "Maine Runners",
     xlab = "Time (minutes)")

# Histogram for away
hist(away_times,
     breaks = breaks,
     freq = FALSE,
     xlim = range(data$Time..minutes.),
     main = "Away Runners",
     xlab = "Time (minutes)")

# Summary for away and Maine
maine_stats <- c(
  mean = mean(maine_times),
  sd = sd(maine_times),
  min = min(maine_times),
  max = max(maine_times),
  median = median(maine_times),
  IQR = IQR(maine_times)
)

away_stats <- c(
  mean = mean(away_times),
  sd = sd(away_times),
  min = min(away_times),
  max = max(away_times),
  median = median(away_times),
  IQR = IQR(away_times)
)

maine_stats
away_stats

# Box plots for runner times
# Time data are from part a & b
boxplot(
  maine_times,
  ylab = "Maine Runners' Finishing Times (minutes)",
  col = "#3a86d4"
)

boxplot(
  away_times,
  ylab = "Away Runners' Finishing Times (minutes)",
  col = "#da452b"
)

# Storing & printing data moments
box_values <- boxplot(maine_times, plot = FALSE)
outliers <- box_values$out
min_outlier <- min(outliers)    # if > min, N/A
max_outlier <- max(outliers)    # if < max, N/A

maine_stats <- c(
  mean = mean(maine_times),
  sd = sd(maine_times),
  min = box_values$stats[1, 1],
  min_out = min_outlier,
  Q1 = box_values$stats[2, 1],
  Q2 = box_values$stats[3, 1],
  Q3 = box_values$stats[4, 1],
  max = box_values$stats[5, 1],
  max_out = max_outlier
)
maine_stats

box_values <- boxplot(away_times, plot = FALSE)
outliers <- box_values$out
min_outlier <- min(outliers)    # if > min, N/A
max_outlier <- max(outliers)    # if < max, N/A

away_stats <- c(
  mean = mean(away_times),
  sd = sd(away_times),
  min = box_values$stats[1, 1],
  min_out = min_outlier,
  Q1 = box_values$stats[2, 1],
  Q2 = box_values$stats[3, 1],
  Q3 = box_values$stats[4, 1],
  max = box_values$stats[5, 1],
  max_out = max_outlier
)
away_stats

# Get age data
female_ages <- na.omit(as.numeric(data$Age[data$Sex == "F"]))
male_ages <- na.omit(as.numeric(data$Age[data$Sex == "M"]))

# Box plots for runner ages
boxplot(
  female_ages,
  ylab = "Female Runners' Ages (Years)",
  col = "#3a86d4"
)

boxplot(
  male_ages,
  ylab = "Male Runners' Ages (Years)",
  col = "#da452b"
)

# Storing & printing data moments
box_values <- boxplot(female_ages, plot = FALSE)
outliers <- box_values$out
min_outlier <- min(outliers)    # if > min, N/A
max_outlier <- max(outliers)    # if < max, N/A

female_stats <- c(
  mean = mean(female_ages),
  sd = sd(female_ages),
  min = box_values$stats[1, 1],
  min_out = min_outlier,
  Q1 = box_values$stats[2, 1],
  Q2 = box_values$stats[3, 1],
  Q3 = box_values$stats[4, 1],
  max = box_values$stats[5, 1],
  max_out = max_outlier
)
female_stats

box_values <- boxplot(male_ages, plot = FALSE)
outliers <- box_values$out
min_outlier <- min(outliers)    # if > min, N/A
max_outlier <- max(outliers)    # if < max, N/A
male_stats <- c(
  mean = mean(male_ages),
  sd = sd(male_ages),
  min = box_values$stats[1, 1],
  min_out = min_outlier,
  Q1 = box_values$stats[2, 1],
  Q2 = box_values$stats[3, 1],
  Q3 = box_values$stats[4, 1],
  max = box_values$stats[5, 1],
  max_out = max_outlier
)
male_stats