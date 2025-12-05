true_p <- 0.8
prior_a <- 1.1
prior_b <- 4
ymax <- 20
set.seed(8675309)
sample_sizes <- c(1, 2, 5, 10, 20, 30, 50, 70, 100, 200, 400)
plot_sampling <- TRUE

for(n in rev(sample_sizes)){
  X <- rbinom(n, 1, true_p)
  mle <- mean(X)
  curve(dbeta(x, prior_a, prior_b), from = 0, to = 1, 
        n = 1000, ylim = c(0, ymax), col = "blue", lwd = 2,
        main = paste("n = ", n, sep = ""), ylab = "")
  curve(dbeta(x, prior_a + sum(X), prior_b + n - sum(X)), from = 0, to = 1, 
        n = 1000, add = TRUE, col = "purple", lwd = 2)
  if(plot_sampling){
    curve(dnorm(x, mle, sqrt(mle*(1-mle) / n)), from = 0, to = 1, 
          n = 1000, add = TRUE, col = "black", lwd = 2, lty = 2)
  }
  
  abline(v = true_p, col = "red", lwd = 2)
  abline(v = mle, lwd = 2, lty = 2)
  
  legend("topleft", c("prior", "posterior", "sampling\ndistribution\nof mle",
                      "mle", "true value"), 
         lty = c(1, 1, 2, 2, 1), lwd = 2,
         col = c("blue", "purple", "black", "black", "red"), bty = "n")
}