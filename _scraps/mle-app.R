library(shiny)

ui <- fluidPage(
  titlePanel("Maximum likelihood by eye"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose a distribution family:",
                  choices = c("Normal (unknown mean)", 
                              "Normal (unknown variance)", 
                              "Exponential")),
      
      # The sliders will appear here:
      uiOutput("param_ui")
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamically choose which sliders to show
  output$param_ui <- renderUI({
    req(input$dist)
    
    if (input$dist == "Normal (unknown mean)") {
      tagList(
        sliderInput("theta", "Take a guess at θ", min = -5, max = 5, value = 0, step = 0.01)
      )
    } else if (input$dist == "Normal (unknown variance)") {
      tagList(
        sliderInput("theta", "Take a guess at θ", min = 0, max = 10, value = 1, step = 0.01)
      )
    } else if (input$dist == "Exponential") {
      tagList(
        sliderInput("theta", "Take a guess at θ", min = 0, max = 6, value = 1, step = 0.01)
      )
    }
  })
  
  # Generate a plot from the chosen distribution
  output$distPlot <- renderPlot({
    if (input$dist == "Normal (unknown mean)") {
      
      # =============================================
      # get the guess
      # =============================================
      
      theta <- input$theta
      
      # =============================================
      # create some fake data
      # =============================================
      
      n <- 10
      set.seed(123)
      X <- rnorm(n, mean = sqrt(pi) * 3 / 5, sd = 1.5)
      
      # =============================================
      # plot data density
      # =============================================
      
      par(mfrow = c(1, 2))
      
      curve(dnorm(x, mean = theta, sd = 1), 
            from = -6, 
            to = 6, 
            n = 2000, 
            ylab = "f(x | θ)", 
            main = "Density of N(θ, 1)",
            bty = "n",
            yaxs = "i", 
            xaxt = "n",
            yaxt = "n",
            lwd = 2,
            xlim = c(-5, 5),
            ylim = c(-0.05, 1.25 * dnorm(theta, mean = theta, sd = 1)))
      axis(1, pos = 0)
      axis(2, at = seq(-0.5, 0.5, by = 0.1))
      mtext("θ", side = 1, at = theta, line = 1, col = "blue")
      points(theta, dnorm(theta, mean = theta, sd = 1), type = "h", col = "blue", lwd = 2, lty = 2)
      points(X, numeric(n), col = "red", pch = 19, cex = 0.75)
      points(X, dnorm(X, mean = theta, sd = 1), type = "h", lty = 3, col = "darkgrey")
      points(X, dnorm(X, mean = theta, sd = 1), col = "red", pch = 19, cex = 0.75)
      
      # =============================================
      # plot the likelihood function
      # =============================================
      
      M <- 2000
      theta_grid <- seq(-5, 5, length.out = M)
      L_grid <- numeric(M)
      for(i in 1:M){
        L_grid[i] <- prod(dnorm(X, mean = theta_grid[i], sd = 1))
      }
      plot(theta_grid, L_grid, 
           type = "l", 
           main = "Likelihood function", 
           xlab = expression(theta), 
           ylab = "L(θ)",
           col = "salmon",
           bty = "n", lwd = 2)
      points(theta, prod(dnorm(X, mean = theta, sd = 1)), col = "red", pch = 19)
      points(theta, prod(dnorm(X, mean = theta, sd = 1)), type = "h", lty = 3, "darkgrey")
      
    } else if (input$dist == "Normal (unknown variance)"){
      
      # =============================================
      # get the guess
      # =============================================
      
      theta <- input$theta
      
      # =============================================
      # create some fake data
      # =============================================
      
      n <- 10
      set.seed(123)
      X <- rnorm(n, mean = 0, sd = sqrt(pi))
      
      # =============================================
      # plot data density
      # =============================================
      
      par(mfrow = c(1, 2))
      
      curve(dnorm(x, mean = 0, sd = sqrt(theta)), 
            from = -6, 
            to = 6, 
            n = 2000, 
            ylab = "f(x | θ)", 
            main = "Density of N(0, θ)",
            bty = "n",
            yaxs = "i", 
            xaxt = "n",
            yaxt = "n",
            lwd = 2,
            xlim = c(-5, 5),
            ylim = c(-0.05, 1))
      axis(1, pos = 0)
      axis(2, at = seq(-0.5, 1, by = 0.1))
      points(X, numeric(n), col = "red", pch = 19, cex = 0.75)
      points(X, dnorm(X, mean = 0, sd = sqrt(theta)), type = "h", lty = 3, col = "darkgrey")
      points(X, dnorm(X, mean = 0, sd = sqrt(theta)), col = "red", pch = 19, cex = 0.75)
      
      # =============================================
      # plot the likelihood function
      # =============================================
      
      M <- 3000
      theta_grid <- seq(0, 10, length.out = M)
      L_grid <- numeric(M)
      for(i in 1:M){
        L_grid[i] <- prod(dnorm(X, mean = 0, sd = sqrt(theta_grid[i])))
      }
      plot(theta_grid, L_grid, 
           type = "l", 
           main = "Likelihood function", 
           xlab = expression(theta), 
           ylab = "L(θ)",
           col = "salmon",
           bty = "n", lwd = 2)
      points(theta, prod(dnorm(X, mean = 0, sd = sqrt(theta))), col = "red", pch = 19)
      points(theta, prod(dnorm(X, mean = 0, sd = sqrt(theta))), type = "h", lty = 3, "darkgrey")

    } else if (input$dist == "Exponential"){
      
      # =============================================
      # get the guess
      # =============================================
      
      theta <- input$theta
      
      # =============================================
      # create some fake data
      # =============================================
      
      n <- 10
      set.seed(123)
      X <- rexp(n, rate = sqrt(pi))
      
      # =============================================
      # plot data density
      # =============================================
      
      par(mfrow = c(1, 2))
      
      curve(dexp(x, rate = theta), 
            from = 0, 
            to = 6, 
            n = 2000, 
            ylab = "f(x | θ)", 
            main = "Density of Exponential(θ)",
            bty = "n",
            yaxs = "i", 
            xaxt = "n",
            yaxt = "n",
            lwd = 2,
            xlim = c(0, 4),
            ylim = c(-0.05, 6))
      axis(1, pos = 0)
      axis(2, at = seq(0, 6, by = 1))
      mtext("θ", side = 2, at = theta, line = 2, col = "blue")
      #points(theta, dnorm(theta, mean = theta, sd = 1), type = "h", col = "blue", lwd = 2, lty = 2)
      points(X, numeric(n), col = "red", pch = 19, cex = 0.75)
      points(X, dexp(X, rate = theta), type = "h", lty = 3, col = "darkgrey")
      points(X, dexp(X, rate = theta), col = "red", pch = 19, cex = 0.75)
      
      # =============================================
      # plot the likelihood function
      # =============================================
      
      M <- 2000
      theta_grid <- seq(0, 6, length.out = M)
      L_grid <- numeric(M)
      for(i in 1:M){
        L_grid[i] <- prod(dexp(X, rate = theta_grid[i]))
      }
      plot(theta_grid, L_grid, 
           type = "l", 
           main = "Likelihood function", 
           xlab = expression(theta), 
           ylab = "L(θ)",
           col = "salmon",
           bty = "n", lwd = 2)
      points(theta, prod(dexp(X, rate = theta)), col = "red", pch = 19)
      points(theta, prod(dexp(X, rate = theta)), type = "h", lty = 3, "darkgrey")
      
    }
    
  })
}

shinyApp(ui, server)