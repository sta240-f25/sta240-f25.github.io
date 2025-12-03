library(shiny)

ui <- fluidPage(
  titlePanel("Beta Distribution Viewer"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha", "Alpha (shape1):",
                  min = 0.5, max = 10, value = 2, step = 0.1),
      sliderInput("beta", "Beta (shape2):",
                  min = 0.5, max = 10, value = 2, step = 0.1)
    ),
    
    mainPanel(
      plotOutput("betaPlot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  
  output$betaPlot <- renderPlot({
    a <- input$alpha
    b <- input$beta
    x <- seq(0, 1, length.out = 500)
    
    par(mfrow = c(2,1), mar = c(4,4,2,1))
    
    ## --- CDF ---
    plot(x, pbeta(x, a, b), type = "l", lwd = 2,
         xlab = "", ylab = "CDF", main = "Beta CDF",
         ylim = c(0,1))
    
    ## --- PDF ---
    plot(x, dbeta(x, a, b), type = "l", lwd = 2,
         xlab = "x", ylab = "PDF", main = "Beta PDF",
         ylim = c(0,7))   # fixed y-axis range
  })
}

shinyApp(ui, server)
