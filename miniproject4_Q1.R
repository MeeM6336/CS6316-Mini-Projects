# Read gpa.csv
data <- read.csv("gpa.csv")

# Plot gpa vs. act scatterplot
plot(
    data$act, data$gpa,
    xlab = "ACT",
    ylab = "GPA",
    main = "GPA vs ACT scores",
    pch = 19, # plot char = closed circle
    col = "lightblue"
)

# Get point estimate of population correlation
r <- cor(data$act, data$gpa)
cat(sprintf("Correlation estimator r: %.4f\n", r))

# Set up bootstraps
set.seed(0) # for reproducibility
B <- 1000
boot_corrs <- numeric(B)
n <- nrow(data)

for (i in 1:B) {
    boot_indices <- sample(1:n, size = n, replace = TRUE) # range, number of samples, w/ replacement
    boot_samples <- data[boot_indices, ] # [rows, _] -> grab all columns on rows
    boot_corrs[i] <- cor(boot_samples$act, boot_samples$gpa)
}

# Get bootstrap bias (quality of samples)
boot_bias <- mean(boot_corrs) - r

# Get bootstrap SE (size of samples)
boot_se <- sd(boot_corrs)

# Get 95% CI (95 out of 100 samplings have the population correlation in this interval)
alpha <- 0.05
boot_ci <- quantile(boot_corrs, probs = c(alpha / 2, 1 - alpha / 2))

# Print results
cat(sprintf("Bootstrap Bias of r: %.4f\n", boot_bias))
cat(sprintf("Bootstrap SE of r: %.4f\n", boot_se))
cat(sprintf("Bootstrap 95%% CI of r: [%.4f, %.4f]\n", boot_ci[1], boot_ci[2]))
