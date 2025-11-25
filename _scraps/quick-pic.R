par(mfrow = c(1, 2), mar = c(4, 0.5, 4, 0.5))
curve(log(x), from = 0.1, to = 10, n = 1000, xlim = c(-3, 8),
      ylim = c(-2, 2), main = "ln(x)", ylab = "",
      xaxt = "n", yaxt = "n", bty = "n", col = "red", lwd = 2)
axis(1, pos = 0)
axis(2, pos = 0)

curve(log(x+1), from = -1.1, to = 10, n = 1000, xlim = c(-3, 8),
      ylim = c(-2, 2), main = "ln(x + 1)", ylab = "",
      xaxt = "n", yaxt = "n", bty = "n", col = "red", lwd = 2)
axis(1, pos = 0)
axis(2, pos = 0)

par(mfrow = c(1, 1), mar = c(2, 2, 2, 2))

plot(0, col = 'white', 
     bty = "n", 
     xlim = c(-2, 2), ylim = c(-2, 2), xaxt = "n", yaxt = "n")
polygon(c(0, 2, 2, 0), c(0, 0, 2, 2), border = NA, col = "salmon")
axis(1, pos = 0, lwd = 2)
axis(2, pos = 0, lwd = 2)
mtext("R", side = 4, at = 0, las = 2, line = 0, cex = 2)
mtext("S", side = 3, at = 0, las = 0, line = 0, cex = 2)