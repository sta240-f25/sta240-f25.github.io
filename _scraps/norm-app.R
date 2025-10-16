library(shiny)

ui <- fluidPage(
  titlePanel("Normal Distribution Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu", "Mean (μ):", 
                  min = -5, max = 5, value = 0, step = 0.1),
      sliderInput("sigma", "Standard Deviation (σ):", 
                  min = 0.5, max = 3, value = 1, step = 0.1)
    ),
    
    mainPanel(
      plotOutput("distPlot", height = "600px")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    mu <- input$mu
    sigma <- input$sigma
    
    # Fixed x range
    x <- seq(-10, 10, length.out = 1000)
    
    # Compute values
    pdf_vals <- dnorm(x, mean = mu, sd = sigma)
    cdf_vals <- pnorm(x, mean = mu, sd = sigma)
    
    # Inflection points at mu ± sigma
    inflect_left <- mu - sigma
    inflect_right <- mu + sigma
    
    # Fixed y limits
    pdf_ylim <- c(0, 0.8)
    
    par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
    
    # --- CDF Plot ---
    plot(x, cdf_vals, type = "l", lwd = 2, col = "blue",
         xlim = c(-10, 10), ylim = c(0, 1),
         main = "Cumulative Distribution Function (CDF)",
         xlab = "", ylab = "F(x)")
    abline(h = c(0, 1), col = "gray80", lty = 2)
    abline(v = mu, col = "gray60", lty = 3)
    
    # --- PDF Plot ---
    plot(x, pdf_vals, type = "l", lwd = 2, col = "darkred",
         xlim = c(-10, 10), ylim = pdf_ylim,
         main = "Probability Density Function (PDF)",
         xlab = "x", ylab = "f(x)")
    
    # Vertical lines at mean and inflection points
    abline(v = mu, col = "gray60", lty = 3)
    abline(v = c(inflect_left, inflect_right), col = "gray70", lty = 2)
    
    # Arrows showing sigma distance
    y_arrow <- 0.05
    arrows(mu, y_arrow, inflect_right, y_arrow, code = 3, angle = 10, length = 0.1)
    arrows(mu, y_arrow, inflect_left, y_arrow, code = 3, angle = 10, length = 0.1)
    
    # Label σ between mean and inflection points
    text(mu + sigma / 2, y_arrow + 0.03, expression(sigma), cex = 1.1)
    text(mu - sigma / 2, y_arrow + 0.03, expression(sigma), cex = 1.1)
    
    # Label μ in the margin below the x-axis
    mtext(expression(mu), side = 1, line = 2.2, at = mu, cex = 1.2)
  })
}

shinyApp(ui = ui, server = server)
