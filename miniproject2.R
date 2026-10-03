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
maine_stats = c(
  mean = mean(maine_times),
  sd = sd(maine_times),
  min = min(maine_times),
  max = max(maine_times),
  median = median(maine_times),
  IQR = IQR(maine_times)
)
  
away_stats = c(
  mean = mean(away_times),
  sd = sd(away_times),
  min = min(away_times),
  max = max(away_times),
  median = median(away_times),
  IQR = IQR(away_times)
)

maine_stats

away_stats

