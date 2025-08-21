# https://phys.libretexts.org/Bookshelves/University_Physics/University_Physics_(OpenStax)/University_Physics_III_-_Optics_and_Modern_Physics_(OpenStax)/07%3A_Quantum_Mechanics/7.05%3A_The_Quantum_Particle_in_a_Box

library(animation)

L = 1
nmax = 10
energy = c(1:nmax, nmax:1)

saveGIF(
  {
    
    for(n in energy){
      
      par(mfrow = c(2, 1))
      
      par(mar = c(0.5, 3, 3, 0.5))
      curve(sqrt(2/L)*sin(n*pi*x/L), 
            from = 0, to = L, n = 1000, 
            ylim = c(-2, 2),
            main = "Wave function", 
            ylab = "", xaxt = "n",
            xlab = "",
            col = "red", lwd = 2, bty = "n",
            cex.main = 1.5)
      axis(1, pos = 0)
      
      par(mar = c(4, 3, 3, 0.5))
      curve((sqrt(2/L)*sin(n*pi*x/L))^2, 
            from = 0, to = L, 
            n = 1000,
            yaxs = "i",
            main = "Probability density",
            ylab = "",
            ylim = c(0, 2.25),
            xlab = "Position",
            col = "blue", lwd = 2, bty = "n",
            cex.main = 1.5)
      
    }
  },
  movie.name = "wavefunction.gif",
  interval = 0.2,
  ani.width = 400,
  ani.height = 500,
  outdir = getwd()
)

