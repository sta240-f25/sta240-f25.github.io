set.seed(123)
true_theta <- 3.14      # true value of parameter
n <- 10                 # sample size
M <- 2500               # how many repetitions
estimates <- numeric(M) # preallocate storage for the sample of estimates

for(m in 1:M){
  X <- (1-runif(n))^(-1/true_theta) - 1
  estimates[m] <- n / sum(log(X + 1))
}

hist(estimates, breaks = "Scott", freq = FALSE, , xlim = c(0, 10))
curve(dgamma(1 / x, shape = n, rate = n * true_theta) / (x^2), 
      from = 0, to = max(estimates), n = 1000, col = "red", 
      lwd = 2, add = TRUE)
#abline(v = true_theta)