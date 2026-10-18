# Store sample data points
x <- c(21.72, 14.65, 50.42, 28.78, 11.23)

# Define log-likelihood function
# optim() minimizes by default, so we use negative log-likelihood instead
# min(-log()) = max(log())
neg_log_lik <- function(theta, x_data) {
  
  if (theta <= 0) return(Inf) # theta must be positive for this PDF

  n <- length(x_data)
  log_lik <- n * log(theta) - (theta + 1) * sum(log(x_data))

  return(-log_lik)
}

# Optimize and print MLE value
fit <- optim(
  par = 1.0,               # initial guess for theta
  fn = neg_log_lik,        # function to minimize
  x_data = x,              # argument for neg_log_lik
  method = "Brent"         # for single parameter
)
fit$par