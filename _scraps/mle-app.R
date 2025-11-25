library(shiny)

ui <- fluidPage(
  titlePanel("Maximum likelihood by eye"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose an distribution family:",
                  choices = c("Normal", "Exponential")),
      
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
    
    if (input$dist == "Normal") {
      tagList(
        sliderInput("theta", "Take a guess", min = -5, max = 5, value = 0, step = 0.01)
      )
    } else if (input$dist == "Exponential") {
      tagList(
        sliderInput("theta", "Take a guess", min = 0, max = 5, value = 1, step = 0.01)
      )
    }
  })
  
  # Generate a plot from the chosen distribution
  output$distPlot <- renderPlot({
    if (input$dist == "Normal") {
      theta <- input$theta
      n <- 10
      set.seed(123)
      X <- rnorm(n, mean = sqrt(pi)*3/5, sd = 1.5)
      par(mfrow = c(1, 2))
      curve(dnorm(x, mean = theta, sd = 1), from = -5, to = 5, n = 1000, ylab = "f(x | θ)", main = "Data density")
      points(X, dnorm(X, mean = theta, sd = 1), type = "h", lty = 3, col = "darkgrey")
      points(X, dnorm(X, mean = theta, sd = 1), col = "red", pch = 19)
      points(X, numeric(n), col = "red", pch = 19)
      M <- 1000
      theta_grid <- seq(-5, 5, length.out = M)
      L_grid <- numeric(M)
      for(i in 1:M){
        L_grid[i] <- prod(dnorm(X, mean = theta_grid[i], sd = 1))
      }
      plot(theta_grid, L_grid, type = "l", main = "Likelihood function", xlab = expression(theta), 
           ylab = "L(θ)")
      #curve(-(n/2)*(x-mean(X))^2, from = -5, to = 5, n = 1000)
      points(theta, prod(dnorm(X, mean = theta, sd = 1)), col = "red", pch = 19)
      points(theta, prod(dnorm(X, mean = theta, sd = 1)), type = "h", lty = 3, "darkgrey")
    } else if (input$dist == "Exponential"){
      
      theta <- input$theta
      n <- 10
      set.seed(123)
      X <- rexp(n, rate = sqrt(pi)*3/5)
      par(mfrow = c(1, 2))
      curve(dexp(x, rate = theta), from = 0, to = 5, n = 1000, ylim = c(0, 2))
      points(X, dexp(X, rate = theta), type = "h", lty = 3, col = "darkgrey")
      points(X, numeric(n), col = "red", pch = 19)
      M <- 1000
      theta_grid <- seq(0, 5, length.out = M)
      L_grid <- numeric(M)
      for(i in 1:M){
        L_grid[i] <- prod(dexp(X, rate = theta_grid[i]))
      }
      plot(theta_grid, L_grid, type = "l", main = "Likelihood function", xlab = expression(theta))
      #curve(-(n/2)*(x-mean(X))^2, from = -5, to = 5, n = 1000)
      abline(v = theta)
    } 
    
  })
}

shinyApp(ui, server)