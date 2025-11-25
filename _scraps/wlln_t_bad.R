n = 50000
eps = 1
nu = 0.75
x = rt(n, nu)
xbars = cumsum(x) / 1:n
plot(1:n, xbars, type = "l", xlab = "n", ylab = "Sample average", main = "WLLN for t(0.75)")
abline(h = c(eps, - eps, 0), lty = 2, col = c("red", "red", "blue"))