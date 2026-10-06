# Simulate 1 draw of XA, XB, and T
XA <- rexp(1, rate = 0.1)

XB <- rexp(1, rate = 0.1)

T <- max(XA, XB)

"Drawing of T"
T

# Draw T 10,000 times
T_draws <- replicate(10000, max(rexp(2, rate = 0.1)))

# Plot histogram of draws of T and apply exponential PDF curve to historgram
hist(T_draws,
     probability = TRUE,
     breaks = 30,
     main = "Monte Carlo Simulation of T",
     xlab = "Lifetime T (years)")

curve(0.2 * exp(-0.1 * x) - 0.2 * exp(-0.2 * x),
      from = 0,
      to = max(T_draws),
      add = TRUE,
      lwd = 2)

# Estimating E(T)
mean_T <- mean(T_draws)
"Estimation of E(T)"
mean_T

# Estimate P(T > 15) and compare with analytical approach
prob_T_MC <- mean(T_draws > 15)
"Estimation of P(T > 15)"
prob_T_MC

prob_T_A <- 1 - (1 - exp(-0.1 * 15))^2
"Computed P(T > 15)"
prob_T_A

# Repeat 5 times with N = 10,000
results_10000 <- replicate(5, {
  T_draws <- replicate(10000, max(rexp(2, rate = 0.1)))

  c(
    mean_T = mean(T_draws),
    prob_T_15 = mean(T_draws > 15)
  )
})

t(results_10000)

# N = 1,000
results_1000 <- replicate(5, {
  T_draws <- replicate(1000, max(rexp(2, rate = 0.1)))

  c(
    mean_T = mean(T_draws),
    prob_T_15 = mean(T_draws > 15)
  )
})


# N = 100,000
results_100000 <- replicate(5, {
  T_draws <- replicate(100000, max(rexp(2, rate = 0.1)))

  c(
    mean_T = mean(T_draws),
    prob_T_15 = mean(T_draws > 15)
  )
})

t(results_1000)
t(results_100000)


################ Q2 ###############
# Diep Doan

# Simulate 10,000 (x, y) coordinate pairs, uniform on [0,1]
# set.seed(0) for reproducibility (optional)
x <- runif(10000, min = 0, max = 1)
y <- runif(10000, min = 0, max = 1)

# Check if each point lies inside the circle (within 0.5 units of (0.5, 0.5))
dist_sq <- (x - 0.5)^2 + (y - 0.5)^2
inside <- dist_sq <= 0.5^2

# Count how many points fall inside
count_inside <- sum(inside)

# Estimte π
cat("Number of points inside circle (within 0.5 units of (0.5, 0.5)):", count_inside, "\n")
cat("Estimation of π (π = P(point inside circle) * 4):", count_inside / 10000 * 4, "\n")

# Plot, coloring points by whether they're inside or outside
plot(x, y,
     main = "π Simulation",
     xlab = "x",
     ylab = "y",
     pch = 20,
     cex = 0.5,
     col = ifelse(inside, "red", "grey70"))

# Draw the circle boundary for reference
theta <- seq(0, 2 * pi, length.out = 200)
circle_x <- 0.5 + 0.5 * cos(theta)
circle_y <- 0.5 + 0.5 * sin(theta)
lines(circle_x, circle_y, col = "blue", lwd = 2)