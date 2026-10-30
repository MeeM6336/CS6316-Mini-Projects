# PART A

# Read data from CSV
data <- read.csv("VOLTAGE.csv")

# Create boxplot to compare two distributions
boxplot(
  voltage ~ location,
  data = data,
  main="Voltage by Location", 
  xlab = "Location",
  ylab = "Voltage"
)

# Check to see if distribution is normal
remote <- data$voltage[data$location == 0]
local <- data$voltage[data$location == 1]

qqnorm(remote, main = "Remote Voltage Q-Q Plot")
qqline(remote)

qqnorm(local, main = "Local Voltage Q-Q Plot")
qqline(local)

# PART B

# Conduct z test with 95% confidence interval
n1 <- length(remote)
n2 <- length(local)

mean1 <- mean(remote)
mean2 <- mean(local)

var1 <- var(remote)
var2 <- var(local)

# Find the Z critical value
alpha <- 0.05
z_critical <- qnorm(1 - alpha/2) 

se <- sqrt(var1/n1 + var2/n2)
margin_of_error <- z_critical * se

# Construct the Confidence Interval
point_estimate <- mean1 - mean2
lower_bound <- point_estimate - margin_of_error
upper_bound <- point_estimate + margin_of_error
cat("95% Z-Confidence Interval: [", lower_bound, ",", upper_bound, "]\n")