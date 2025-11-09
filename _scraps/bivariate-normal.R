fxy = function(x, y, mu, Sig, sd1, sd2, rho) {
  
  if(missing(mu)) mu=c(0,0)
  
  if(!missing(Sig)) {
    sd1 = sqrt(Sig[1,1])
    sd2 = sqrt(Sig[2,2])
    if(Sig[1,2] != Sig[2,1]) {
      print("Covariance matrix is not symmetric... Returning .")
      return(NULL)
    }
    rho = Sig[1,2]/(sd1*sd2)
  }
  else if(missing(rho) || missing(sd1) || missing(sd2)) {
    sd1 = sd2 = 1
    rho = 0
  }
  
  Q = (x-mu[1])^2/sd1^2 + (y-mu[2])^2/sd2^2 -
    2*rho*(x-mu[1])*(y-mu[2])/(sd1*sd2)
  
  1/(2*pi*sd1*sd2*sqrt(1-rho^2))*exp(-Q/(2*(1-rho^2)))
}


## Calls persp() with preferred arguments
persp.plot = function(x, y, z, main="Bivariate Normal Density",
                      theta=30, phi=25, r=50, d=.1, expand=0.5, ltheta=90, lphi=180,
                      shade=0.5, ticktype="simple", nticks=5, col="lightgreen", zlab="", ...) {
  
  persp(x, y, z, main=main,
        theta=theta, phi=phi, r=r, d=d, expand=expand, ltheta=ltheta,
        lphi=lphi, shade=shade, ticktype=ticktype, nticks=nticks,
        col=col, zlab=zlab, ...)
}


## Creates covariance matrix from sd.x, sd.y, and rho
calc.Sig = function(sd.x, sd.y, rho) {
  
  sig.xy = rho*sd.x*sd.y
  matrix(c(sd.x^2, sig.xy, sig.xy, sd.y^2), nrow=2)
}


## Returns bivariate normal density for specified x-y grid
dmvnorm = function(x, y, mu, Sig) {
  
  if(missing(mu)) mu = c(0,0)
  if(missing(Sig)) Sig = diag(2)
  
  outer(x, y, fxy, mu, Sig)
}


## This is only the kernel of the bivariate Normal density
## x is a 2x1 vector
f = function(x, y, mu=c(0,0), sd.x=1, sd.y=1, rho=0) {
  
  #t(X-mu)%*%solve(Sig)%*%(X-mu)
  mu.x = mu[1]
  mu.y = mu[2]
  A = (x-mu.x)^2/sd.x^2 + (y-mu.y)^2/sd.y^2
  B = 2*rho/(sd.x*sd.y)*(x-mu.x)*(y-mu.y)
  return((A-B)/(1-rho^2))
}


### End: Function definitions ###




library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("The bivariate normal density"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("sd.x",
                  "X standard deviation:",
                  min = 0,
                  max = 1,
                  value = 0.5,
                  step = 0.01),
      sliderInput("sd.y",
                  "Y standard deviation:",
                  min = 0,
                  max = 1,
                  value = 0.5,
                  step = 0.01),
      sliderInput("rho",
                  "Correlation:",
                  min = -1,
                  max = 1,
                  value = 0,
                  step = 0.01)
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
    
    ## Define sequence of parameter
    
    p.seq = c(.8, .9, .95, .99)
    cont.lev = qchisq(p.seq, 2)
    cont.lab = c("80%", "90%", "95%", "99%")
    
    
    ## Plotting starts here
    
    par(cex.lab=2)
    par(cex.axis=1.75)
    par(las=1)
    par(mfrow=c(1,2))  # 1.5:1 aspect ratio
    
    sd.x = input$sd.x
    sd.y = input$sd.y
    rho = input$rho
    N = 100
    x = y = seq(-3.2,3.2,le=N)  # create x-y grid of size NxN
    mu = c(0,0)
    
    
    z = dmvnorm(x, y, mu, calc.Sig(sd.x, sd.y, rho))
    persp.plot(x, y, z, main="", col="lightblue", border=NA, cex.lab=1.5,
               axes=F)
    
    ## Contour Plot
    z = outer(x, y, f, mu, sd.x, sd.y, rho)
    contour(x, y, z, levels=cont.lev, cex.axis=1.4, labels=cont.lab,
            xlab="x", ylab="y", cex.lab=1.5)
    abline(v=0, h=0, lty=3, col="darkgrey")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
