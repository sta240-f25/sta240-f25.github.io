library(shiny)

ui <- fluidPage(
  titlePanel("Gamma distribution CDF and PDF"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha", "Shape (α):",
                  min = 0.5, max = 5, value = 2, step = 0.1),
      sliderInput("beta", "Rate (β):",
                  min = 0.2, max = 2, value = 0.5, step = 0.05),
      hr(),
      verbatimTextOutput("moments")
    ),
    
    mainPanel(
      plotOutput("gammaPlot", height = "600px")
    )
  )
)

server <- function(input, output) {
  output$moments <- renderText({
    alpha <- input$alpha
    beta  <- input$beta
    mu <- alpha / beta
    sd <- sqrt(alpha) / beta
    paste0("E(X) = ", round(mu, 3),
           "\nSD(X) = ", round(sd, 3))
  })
  
  output$gammaPlot <- renderPlot({
    alpha <- input$alpha
    beta  <- input$beta
    
    # Fixed plotting range
    x_min <- -2
    x_max <- 25
    x <- seq(x_min, x_max, length.out = 2000)
    
    # PDF and CDF
    pdf_vals <- ifelse(x >= 0, dgamma(x, shape = alpha, rate = beta), 0)
    cdf_vals <- ifelse(x >= 0, pgamma(x, shape = alpha, rate = beta), 0)
    
    mu <- alpha / beta
    
    # Fixed y-limits
    pdf_ylim <- c(0, 0.8)
    cdf_ylim <- c(0, 1)
    
    par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
    
    # --- CDF ---
    plot(x, cdf_vals, type = "l", lwd = 2, col = "blue",
         xlim = c(x_min, x_max), ylim = cdf_ylim,
         main = "Cumulative Distribution Function (CDF)",
         xlab = "", ylab = "F(x)")
    abline(h = c(0, 1), col = "gray80", lty = 2)
    #abline(v = mu, col = "gray60", lty = 3)
    
    # --- PDF ---
    plot(x, pdf_vals, type = "l", lwd = 2, col = "darkred",
         xlim = c(x_min, x_max), ylim = pdf_ylim,
         main = "Probability Density Function (PDF)",
         xlab = "x", ylab = "f(x)")
    #abline(v = mu, col = "gray60", lty = 3)
    
    # Label E(X) with mtext along x-axis
    if (mu >= x_min && mu <= x_max) {
      mtext("E(X)",
            side = 1, line = 2.2, at = mu, cex = 1.0)
    }
  })
}

shinyApp(ui = ui, server = server)
