n = 20000
eps = 0.2
nu = 1.8
x = rt(n, nu)
xbars = cumsum(x) / 1:n
plot(1:n, xbars, type = "l", xlab = "n", ylab = "Sample average", main = "WLLN for t(1.8)",
     ylim = c(0 - 5*eps, 0 + 5*eps))
abline(h = c(eps, - eps, 0), lty = 2, col = c("red", "red", "blue"))