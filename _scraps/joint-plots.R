par(mfrow = c(1, 2))

curve(2*x, from = 0, to = 1, n = 1000, ylim = c(0, 3), col = "red", lwd = 3,
      main = "Marginal PDF of X", xlab = "x", ylab = expression(f[X]))
curve(3*x^2, from = 0, to = 1, n = 1000, , ylim = c(0, 3), col = "red", lwd = 3,
      main = "Marginal PDF of Y", xlab = "y", ylab = expression(f[Y]))



par(mfrow = c(1, 1))

curve(-log(x), from = 0, to = 1, n = 1000, ylab = "-ln y", xlab = "y", main = "Marginal PDF of Y")