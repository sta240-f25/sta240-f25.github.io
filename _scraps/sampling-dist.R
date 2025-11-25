n = 200
M = 1000000
true_p = 0.5
estimates = numeric(M)
set.seed(8675309)
for (m in 1:M) {
  data = rbinom(n, 1, true_p)
  estimates[m] = mean(data)
}
hist(estimates, freq = FALSE, xlim = c(0, 1), col = "lightblue", 
     main = "Sampling distribution of the sample proportion",
     breaks = "Scott")
abline(v = true_p, col = "red", lwd = 5)




