library(rgl)

n <- 1000
x <- seq(-2, 2, length.out = n)
y <- seq(-2, 2, length.out = n)
z_top  <- outer(x, y, function(x, y) ifelse(x^2 + y^2 <= 1, 1/pi, NA))
z_base <- outer(x, y, function(x, y) ifelse(x^2 + y^2 > 1, 0, NA))

open3d()
par3d(windowRect = c(100, 100, 800, 800))

# flat shading
material3d(lit = FALSE)

# Draw surfaces
# Draw surfaces scaled in the z-direction only
surface3d(x, y, z_base, color = "salmon", scale = c(1, 1, 3))
surface3d(x, y, z_top,  color = "red", scale = c(1, 1, 3))


# Axes and title
axes3d()
title3d(xlab = "x", ylab = "y", zlab = "f(x, y)",
        main = "Uniform on the Unit Disc (Correct Scale)")

# Adjust camera so the disc looks taller
rgl.viewpoint(theta = 45, phi = 25, zoom = 1.5)
