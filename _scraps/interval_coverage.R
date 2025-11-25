sample_sizes = seq(10, 200, by = 20)
K = length(sample_sizes)
M = 1000
p0 = 0.01
alpha = 0.05
zscore = qnorm(1 - (alpha / 2))

coverage = numeric(K)

for (i in 1:K){
  n = sample_sizes[i]
  for (m in 1:M){
    data = rbinom(n, 1, p0)
    phat = mean(data)
    se = sqrt(phat * (1 - phat) / n)
    
    L = phat - zscore * se
    U = phat + zscore * se
    
    coverage[i] = coverage[i] + (L < p0 & p0 < U) / M
  }
}

plot(sample_sizes, coverage, type = "l", ylim = c(0, 1),
     xlab = "sample sizes", ylab = "",
     main = paste("Coverage of large-sample interval; p0 = ", p0, sep = ""),
     col = "blue")
abline(h = 1 - alpha)