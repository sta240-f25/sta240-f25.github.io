m = 1
s = 2
par(mar = c(4, 1, 4, 1))
curve(exp(-0.5 * ((x - m) / s)^2), 
      from = -6, to = 8, n = 500,
      ylim = c(0, 1.2),
      xlab = "x", ylab = "",
      xaxt = "n", yaxt = "n",
      bty = "n", col = "red", lwd = 2,
      main = "Gaussian density",
      panel.first = c(polygon(x = c(-10, m-s, m-s, -10),
                              y = c(0, 0, 100, 100),
                              col = rgb(1, 0.5, 0, 0.1),
                              border = NA),
                      polygon(x = c(m-s, m+s, m+s, m-s),
                              y = c(0, 0, 100, 100),
                              col = rgb(0, 0, 1, 0.1),
                              border = NA),
                      polygon(x = c(m+s, 10, 10, m+s),
                              y = c(0, 0, 100, 100),
                              col = rgb(1, 0.5, 0, 0.1),
                              border = NA))
      )
abline(h = 0)
segments(m, 0, m, exp(-0.5 * ((m - m) / s)^2), lty = 2)
abline(v = c(m - s, m + s), lty = c(1, 1))
axis(1, at = c(m - s, m, m + s), pos = 0,
     labels = c(expression(mu-sigma), 
                expression(mu), 
                expression(mu+sigma)),
     las = 1,
     line = 2,
     cex.axis = 1.5)
legend("topright", "concave up", bty = "n", text.col = "orange")
legend("topleft", "concave up", bty = "n", text.col = "orange")
legend("top", "concave down", bty = "n", text.col = "blue")