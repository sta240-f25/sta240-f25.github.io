

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("The Student's t distribution"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("df",
                  "Degrees of freedom:",
                  min = 0,
                  max = 25,
                  value = 1,
                  step = 0.1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", height = "600px")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    df = input$df
    
    par(mfrow = c(2, 1), mar = c(4, 1, 1, 1))
    
    curve(pnorm(x), from = -5, to = 5, n = 5000, col = "black", lwd = 3, bty = "n", 
          yaxt = "n", ylab = "")
    curve(pt(x, df), from = -5, to = 5, n = 5000, col = "red", add = TRUE, lwd = 2)
    abline(h = c(0, 1))
    
    curve(dnorm(x), from = -5, to = 5, n = 5000, col = "black", lwd = 3, bty = "n",
          yaxt = "n", ylab = "")
    curve(dt(x, df), from = -5, to = 5, n = 5000, col = "red", add = TRUE, lwd = 2)
    
    legend("topright", c("N(0, 1) density", "Student's t density"), bty = "n",
           lty = 1, col = c("black", "red"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
