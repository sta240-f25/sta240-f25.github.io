plot(0, 0, type = "n", xlim = c(-1, 2), ylim = c(-2, 5),
     xlab = "X", ylab = "Y", main = "Joint range: 0 < x < 1 and 0 < y", 
     bty = "n", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")


polygon(c(0, 1, 1, 0), 
        c(0, 0, 6, 6), col = rgb(1, 0, 0, alpha = 0.5), border = NA)

axis(1, pos = 0)
axis(2, pos = 0)