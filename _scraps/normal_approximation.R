`n = 500
M = 5000
true_p = 0.4
stdestimates = numeric(M)

for (m in 1:M) {
  
  data = rbinom(n, 1, true_p)
  phat = mean(data)
  stdestimates[m] = (phat - true_p) / sqrt(phat * (1 - phat) / n)
  
}

hist(stdestimates, freq = FALSE,
     breaks = "Scott",
     col = "lightblue", 
     main = "Standardized sampling distribution of the sample proportion")
curve(dnorm, from = -4, to = 4, lwd = 3, col = "red", add = TRUE)
`