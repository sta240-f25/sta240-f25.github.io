n = 100000
eps = 0.05
mu = 4
sigsq = 6
x = rnorm(n, mu, sqrt(sigsq))
xbars = cumsum(x) / 1:n
plot(1:n, xbars, type = "l", xlab = "n", ylab = "Sample average", main = "WLLN for N(4, 6)",
     ylim = c(mu - 2*eps, mu + 2*eps))
abline(h = c(mu + eps, mu - eps, mu), lty = 2, col = c("red", "red", "blue"))