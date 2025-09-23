library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("The sample space of a simple diagnostic test"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("TPR",
                  "Sensitivity (TPR):",
                  min = 0,
                  max = 1,
                  value = 0.5),
      sliderInput("TNR",
                  "Specificity (TNR):",
                  min = 0,
                  max = 1,
                  value = 0.5),
      sliderInput("p",
                  "Prevalence:",
                  min = 0,
                  max = 1,
                  value = 0.5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    p <- input$p
    TNR <-input$TNR
    TPR <- input$TPR
    
    FNR <- 1 - TPR
    FPR <- 1 - TNR
    
    TpDp <- TPR * p
    TpDn <- FPR * (1 - p)
    TnDp <- FNR * p
    TnDn <- TNR * (1 - p)
    
    par(mar = c(8, 8, 1, 8))
    
    plot(0, col = "white", xlim = c(0, 1), ylim = c(0, 1), xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
    polygon(c(0, p, p, 0), c(FNR, FNR, 1.0, 1.0), col = rgb(1, 0, 0, alpha = 0.25), border = NA)
    polygon(c(0, p, p, 0), c(0, 0, FNR, FNR), col = rgb(0, 1, 0, alpha = 0.25), border = NA)
    polygon(c(p, 1, 1, p), c(0, 0, TNR, TNR), col = rgb(0, 0, 1, alpha = 0.25), border = NA)
    polygon(c(p, 1, 1, p), c(TNR, TNR, 1, 1), col = rgb(1, 0.647, 0, alpha = 0.25), border = NA)
    
    mtext("TNR", side = 4, at = TNR / 2, las = 1)
    mtext("FPR", side = 4, at = (1 + TNR) / 2, las = 1)
    
    mtext("TPR", side = 2, at = (1 + FNR) / 2, las = 1, line = 2)
    mtext("FNR", side = 2, at = FNR / 2, las = 1, line = 2)
    
    mtext("p(D = +)", side = 1, at = p / 2, las = 1, line = 2)
    mtext("P(D = -)", side = 1, at = (1 + p) / 2, las = 1, line = 2)
    
    mtext(c("0", "1"), side = 1, at = c(0, 1), las = 1)
    mtext(c("0", "1"), side = 2, at = c(0, 1), las = 1)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)