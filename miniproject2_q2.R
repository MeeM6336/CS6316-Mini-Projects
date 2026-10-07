# Read motorcycle accident data
motorcycle <- read.csv(file.choose(), header = TRUE)

# Store fatal motorcycle accident counts
accidents <- motorcycle$Fatal.Motorcycle.Accidents

# Calculate relevant summary statistics
accidentsStats <- c(
  mean = mean(accidents),
  sd = sd(accidents),
  min = min(accidents),
  q1 = quantile(accidents, 0.25),
  median = median(accidents),
  q3 = quantile(accidents, 0.75),
  max = max(accidents),
  range = max(accidents) - min(accidents),
  IQR = IQR(accidents)
)

"Summary Statistics"
accidentsStats

# Create boxplot of fatal motorcycle accidents
boxplot(
  accidents,
  main = "Fatal Motorcycle Accidents by County",
  ylab = "Number of Fatal Motorcycle Accidents"
)

# Calculate boundaries for possible outliers using 1.5 * IQR rule
q1 <- quantile(accidents, 0.25)
q3 <- quantile(accidents, 0.75)
iqr <- IQR(accidents)

lowerFence <- q1 - 1.5 * iqr
upperFence <- q3 + 1.5 * iqr

"Lower Outlier Boundary"
lowerFence

"Upper Outlier Boundary"
upperFence

# Identify counties that may be considered outliers
outlierCounties <- motorcycle[
  accidents < lowerFence | accidents > upperFence,
]

"Possible Outlier Counties"
outlierCounties
