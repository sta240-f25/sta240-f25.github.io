discrete_pmf <- function(x, p, xlim = c(min(x) - 1, max(x) + 1), label = "", add_mean = FALSE){
  plot(x, p,
       pch = 19,
       cex = 1,
       xlab = "",
       ylab = "",
       main = "",
       ylim = c(0, 0.5),
       yaxs = "i",
       yaxt = "n",
       xlim = xlim,
       xaxt = "n",
       bty = "n"
  )
  segments(x,
           rep(0, length(x) + 1),
           x1 = x,
           y1 = p,
           lwd = 3
  )
  axis(1, at = floor(xlim[1]):ceiling(xlim[2]), cex.axis = 1)
  axis(2, at = seq(0, 1, length.out = 11), las = 1, cex.axis = 1.5)
  legend("topright", label, bty = "n", cex = 3)
  if(add_mean == TRUE){
    mtext("E(X)", side = 1, at = sum(x * p), col = "red", line = 2)
  }
}

discrete_cdf <- function(x, p, xlim = c(min(x) - 1, max(x) + 1), label = ""){
  closeddot = cumsum(p)
  opencircle = c(0, closeddot[1:length(x)-1])
  plot(x, closeddot, pch = 19, cex = 1,
       ylim = c(0, 1),
       ylab = "", main = "", xlab = "",
       yaxt = "n",
       xlim = xlim,
       xaxt = "n",
       #yaxs = "i", 
       bty = "n")
  points(x, opencircle, cex = 1)
  segments(c(xlim[1], x), c(0, closeddot), c(x, xlim[2]), c(0, closeddot), lwd = 1)
  axis(1, at = floor(xlim[1]):ceiling(xlim[2]), cex.axis = 1)
  axis(2, at = seq(0, 1, length.out = 11), las = 1, cex.axis = 1.5)
  legend("bottomright", label, bty = "n", cex = 3)
}


par(mfrow = c(1, 2), mar = c(2, 4, 2, 1))

discrete_pmf(1:4, c(7, 5, 3, 1) / 16)
discrete_cdf(1:4, c(7, 5, 3, 1) / 16)

