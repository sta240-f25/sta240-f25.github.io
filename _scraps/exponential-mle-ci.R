set.seed(12345)

true_lambda <- 4
n <- 10
M <- 5000
in_or_out_of_ci <- numeric(M)
lowers = numeric(M)
uppers = numeric(M)
alpha = 0.1

for(m in 1:M){
  X = rexp(n, true_lambda)
  xbar = mean(X)
  L = qgamma(alpha/2, shape = n, rate = n) / xbar
  U = qgamma(1 - alpha/2, shape = n, rate = n) / xbar
  in_or_out_of_ci[m] = (L < true_lambda) & (true_lambda < U)
  lowers[m] = L
  uppers[m] = U
}

b <- seq(0, max(uppers, lowers), length.out = 250)

hist(lowers, breaks = b, freq = FALSE, xlim = c(0, 15),
     col = rgb(1, 0, 0, alpha = 0.5),
     main = "Sampling distribution of interval bounds",
     xlab = "")
hist(uppers, breaks = b, freq = FALSE, add = TRUE,
     col = rgb(0, 0, 1, alpha = 0.5))
abline(v = true_lambda, lwd = 2)