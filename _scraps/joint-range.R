# Parameters for the plotting rectangle
xmax <- 5
ymax <- 5

# helper: diagonal cut point
L <- min(xmax, ymax)

# Set up empty plot
plot(0, 0, type = "n", xlim = c(0, xmax), ylim = c(0, ymax),
     xlab = "X", ylab = "Y", main = "Joint range: region where 0 < x < y", 
     bty = "n", xaxs = "i", yaxs = "i")

# Polygon vertices that trace the region {0 <= x <= xmax, 0 <= y <= ymax, y > x}
px <- c(-1, -1, xmax, L, 0)
py <- c(0, ymax, ymax, L, 0)

# Shade the region above the line y = x (within the rectangle)
polygon(px, py, col = adjustcolor("skyblue", alpha.f = 0.4), border = NA)

# Draw the diagonal boundary y = x
abline(a = 0, b = 1, col = "red", lwd = 2)

# Add a label
#text(x = 0.75 * xmax, y = 0.9 * ymax, "y > x", col = "blue", cex = 1.1)
