library(shiny)

ui <- fluidPage(
  titlePanel("Shrinkage Behavior of the Poisson-Gamma Posterior Mean"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha", "Prior shape α:",
                  min = 0.1, max = 100, value = 2, step = 0.1),
      sliderInput("n", "Sample size n:",
                  min = 1, max = 200, value = 10, step = 1),
      sliderInput("lambda", "True Poisson mean λ:",
                  min = 0.1, max = 10, value = 3, step = 0.1)
    ),
    
    mainPanel(
      plotOutput("riskPlot", height = "600px")
    )
  )
)

server <- function(input, output) {
  
  output$riskPlot <- renderPlot({
    
    alpha <- input$alpha
    n <- input$n
    lambda_true <- input$lambda
    
    # Range of beta values for the x-axis
    beta <- seq(0.01, 50, length.out = 500)
    
    # Prior mean
    mu0 <- alpha / beta
    
    # Shrinkage weight (data weight)
    w <- n / (n + beta)
    
    # Posterior mean estimator (assuming observed mean = lambda_true)
    post_mean <- w * lambda_true + (1 - w) * mu0
    
    # Variance and squared bias of the estimator
    variance <- w^2 * lambda_true / n
    bias2 <- (post_mean - lambda_true)^2
    
    mse <- variance + bias2
    
    # ---- Plot ----
    plot(beta, mse, type = "l", lwd = 3, col = "red",
         xlab = "Prior rate β",
         ylab = "Value",
         ylim = range(c(mse, variance, bias2)),
         main = "Posterior Mean: Mean, Variance, and MSE")
    
    lines(beta, variance, col = "blue", lwd = 2)
    lines(beta, bias2, col = "darkgreen", lwd = 2)
    #lines(beta, post_mean, col = "purple", lwd = 2, lty = 2)
    
    legend("topright",
           legend = c("MSE", "Variance", "Bias^2"),
           col = c("red", "blue", "darkgreen"),
           lwd = c(3,2,2), lty = c(1,1,1))
  })
}

shinyApp(ui = ui, server = server)
