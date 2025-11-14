library(animation)


plot(0, col = "white", xlim = c(-10, 10), ylim = c(0, 1),
     bty = "n",
     yaxt = "n",
     xaxt = "n",
     ylab = "", xlab = "")
abline(h = c(0, 1), lwd = 3)


n <- 100

end_prob <- 0.3
end_m1 <- -2
end_m2 <- 7

probs <- seq(1, end_prob, length.out = n)
mean1 <- seq(-8.5, end_m1, length.out = n)
mean2 <- seq(-8.5, end_m2, length.out = n)


for(i in c(1, round(100 - 100 * 0.75^(1:20)))){
  p <- probs[i]
  m1 <- mean1[i]
  m2 <- mean2[i]
  curve(p * pnorm(x, mean = m1) + (1-p) * pnorm(x, mean = m2), 
        from = -11, to = 11, n = 1000, col = "blue", lty = 2, lwd = 3,
        add = TRUE)
}

curve(end_prob * pnorm(x, mean = end_m1) + (1 - end_prob) * pnorm(x, mean = end_m2), 
      from = -11, to = 11, n = 1000, col = "red", lwd = 3,
      add = TRUE)
mtext(c("0", "1"), side = 2, at = c(0, 1), las = 2, line = 2,
      cex = 2)




saveGIF(
  {
    for(i in unique(c(1, round(n - n * 0.75^(1:20))))){
      plot(0, col = "white", xlim = c(-10, 10), ylim = c(0, 1),
           bty = "n",
           yaxt = "n",
           xaxt = "n",
           ylab = "", xlab = "")
      abline(h = c(0, 1), lwd = 3)
      p <- probs[i]
      m1 <- mean1[i]
      m2 <- mean2[i]
      curve(p * pnorm(x, mean = m1) + (1-p) * pnorm(x, mean = m2), 
            from = -11, to = 11, n = 1000, col = "blue", lty = 2, lwd = 3,
            add = TRUE)
      curve(end_prob * pnorm(x, mean = end_m1) + (1 - end_prob) * pnorm(x, mean = end_m2), 
            from = -11, to = 11, n = 1000, col = "red", lwd = 3,
            add = TRUE)
      mtext(c("0", "1"), side = 2, at = c(0, 1), las = 2, line = 2,
            cex = 2)
      legend("bottomright", c(expression(F[n]), expression(F)), 
             lty = c(2, 1), col = c("blue", "red"), lwd = 3, bty = "n",
             cex = 3)
    }

  },
  movie.name = "test.gif",
  interval = 0.2,
  ani.width = 1000 * 0.75,
  ani.height = 625 * 0.75,
  outdir = getwd()
)
