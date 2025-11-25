set.seed(123456)

Nsim <- 2000
n <- 5

true_mean <- 40
true_var <- true_mean

Z <- numeric(Nsim)

for(i in 1:Nsim){
  X <- rpois(n, true_mean)
  Z[i] = (mean(X) - true_mean) / sqrt(true_var / n)
}

hist(Z, freq = FALSE, breaks = "Scott", xlim = c(-3, 3))
curve(dnorm(x), from = -3, to = 3, n = 100, col = "red", add = TRUE, lwd = 3)