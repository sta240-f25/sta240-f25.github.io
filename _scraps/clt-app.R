library(shiny)

ui <- fluidPage(
  titlePanel("What happens as you average more and more iid random variables?"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Choose an underlying distribution P:",
                  choices = c("Bernoulli", "Poisson", "Gamma")),
      
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
    
    if (input$dist == "Bernoulli") {
      tagList(
        sliderInput("n", "Sample size (n):", min = 1, max = 200, value = 1, step = 1),
        sliderInput("p", "Probability of success (p):", min = 0, max = 1, value = 0.5, step = 0.01)
      )
    } else if (input$dist == "Poisson") {
      tagList(
        sliderInput("n", "Sample size (n):", min = 1, max = 100, value = 1, step = 1),
        sliderInput("rate", "Rate parameter (λ):", min = 0, max = 5, value = 1, step = 0.1)
      )
    } else if (input$dist == "Gamma") {
      tagList(
        sliderInput("n", "Sample size (n):", min = 1, max = 100, value = 1, step = 1),
        sliderInput("a", "Shape parameter (α):", min = 0, max = 5, value = 1, step = 0.1),
        sliderInput("b", "Rate parameter (β):", min = 0, max = 5, value = 1, step = 0.1)
      )
    }
  })
  
  # Generate a plot from the chosen distribution
  output$distPlot <- renderPlot({
    if (input$dist == "Bernoulli") {
      n <- input$n
      p <- input$p
      #par(mfrow = c(1, 2))
      #plot(0:n, dbinom(0:n, n, p), type = "h")
      plot((0:n) / n, dbinom(0:n, n, p), type = "h", xlab = expression(bar(x)),
           ylab = expression("P(" ~ bar(X)[n] ~ " = " ~ bar(x) ~ ")"))
    } else if (input$dist == "Poisson") {
      n <- input$n
      rate <- input$rate
      Kmax <- ceiling(qpois(0.99, 200 * rate))
      #par(mfrow = c(1, 2))
      plot(0:Kmax / n, dpois(0:Kmax, n * rate), type = "h", xlab = expression(bar(x)),
           ylab = expression("P(" ~ bar(X)[n] ~ " = " ~ bar(x) ~ ")"),
           xlim = c(0, qpois(0.99, rate)))
      #plot((0:n) / n, dbinom(0:n, n, p), type = "h")
      
    } else if (input$dist == "Gamma") {
      n <- input$n
      a <- input$a
      b <- input$b
      #par(mfrow = c(1, 2))
      #curve(dgamma(x, shape = n * a, rate = b), from = 0, to = 10, n = 1000)
      curve(dgamma(x, shape = n * a, rate = n * b), from = 0, to = qgamma(0.9, shape = a, rate = b), n = 1000)
    }
    
  })
}

shinyApp(ui, server)
