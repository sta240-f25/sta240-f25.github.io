set.seed(123456)

Nsim <- 2000
n <- 2

a <- .1
b <- .1

true_mean <- a / b
true_var <- a / b^2

Z <- numeric(Nsim)

for(i in 1:Nsim){
  X <- rgamma(n, shape = a, rate = b)
  Z[i] = (mean(X) - true_mean) / sqrt(true_var / n)
}

hist(Z, freq = FALSE, breaks = "Scott", xlim = c(-3, 3))
curve(dnorm(x), from = -3, to = 3, n = 100, col = "red", add = TRUE, lwd = 3)