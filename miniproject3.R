samples <- c(1, 2, 3, 5, 10, 30)
thetas <- c(1, 5, 50, 100)
N <- 1000

results <- data.frame(
  n = numeric(),
  theta = numeric(),
  mse_mle = numeric(),
  mse_mom = numeric()
)

# Iterate through all combinations of sample size and theta to find config w/ lowest MSE
for (n in samples) {
  for (theta in thetas) {
    sq_error_mle <- numeric(N)
    sq_error_mom <- numeric(N)
    
    for (i in 1:N) { # 1000 Monte Carlo sims
      x <- runif(n, min = 0, max = theta)
      
      theta_mle <- max(x)
      theta_mom <- 2 * mean(x)
      
      sq_error_mle[i] <- (theta_mle - theta)^2
      sq_error_mom[i] <- (theta_mom - theta)^2
    }
    
    mse_mle <- mean(sq_error_mle)
    mse_mom <- mean(sq_error_mom)
    
    results <- rbind(results, data.frame(
      n = n,
      theta = theta,
      mse_mle = mse_mle,
      mse_mom = mse_mom
    ))
  }
}

results

# Plot a line graph of MSE for MLE and MOM
library(ggplot2)

plot_data <- rbind(
  data.frame(
    n = results$n,
    theta = results$theta,
    estimator = "MLE",
    mse = results$mse_mle
  ),
  data.frame(
    n = results$n,
    theta = results$theta,
    estimator = "MOM",
    mse = results$mse_mom
  )
)

ggplot(plot_data, aes(x = n, y = mse, color = estimator)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ theta, scales = "free_y") +
  labs(
    title = "MSE Comparison: MLE vs MOM",
    x = "Sample Size (n)",
    y = "Mean Squared Error (MSE)",
    color = "Estimator"
  )
