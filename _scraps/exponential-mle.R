set.seed(123)  # for reproducibility

# Parameters
lambda_true <- 2          # true rate parameter
n_sim <- 1000             # number of simulations per sample size
sample_sizes <- 2 * 2^(0:5)

# Container for results
mle_estimates <- vector("list", length(sample_sizes))
names(mle_estimates) <- paste0("n=", sample_sizes)

# Simulate MLEs
for (i in seq_along(sample_sizes)) {
  n <- sample_sizes[i]
  estimates <- numeric(n_sim)
  for (j in 1:n_sim) {
    sample <- rexp(n, rate = lambda_true)
    estimates[j] <- 1 / mean(sample)  # MLE of lambda
  }
  mle_estimates[[i]] <- estimates
}

# Combine into data frame for plotting
mle_data <- stack(mle_estimates)

# Box plot
boxplot(values ~ ind, data = mle_data,
        main = "Sampling Distribution of MLE for Exponential(λ)",
        ylab = "Estimated λ (MLE)",
        xlab = "Sample Size",
        col = "lightblue",
        las = 2, 
        ylim = c(0, 8),
        outline=FALSE)
abline(h = lambda_true, col = "red", lty = 2)  # true value of lambda
