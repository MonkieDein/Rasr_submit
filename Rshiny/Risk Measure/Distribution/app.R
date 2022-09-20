setwd("C:/GITHUB/Rasr_submit")
rm(list=ls())
source("Code/RASR_code.R")

library(shiny)
library(shinydashboard)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Distribution and Risk Measure"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
            # Risk Level Input (tail quantile)
            numericInput(inputId = "RiskInput",label = "Risk :",
                         min = 0,max = 1,value = 0.00,step=0.01),
            # Distribution Input 
            selectInput(inputId = "DistInput", label ="Distribution :", 
                        choices = list("Uniform" = "runif", "Normal" = "rnorm", "Exp" = "rexp"), 
                        selected = 1),
            # Number of samples Input
            numericInput("SampleInput", label = "Samples", min = 1, 
                        max = 10000, value = 100,step = 1),
            # Seed index Input
            numericInput(inputId = "SeedInput",
                         label = "Seed :",
                         min = 0,
                         max = 100,
                         value = 1,step=1)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           verbatimTextOutput("measure")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ERM_levels = (0.99^(1:3000))*10000
    
    compMeasures = reactive({
      set.seed(input$SeedInput)
      X = eval(parse(text=
                       paste0(input$DistInput,"(",input$SampleInput,")") 
      ))
      
      risk = input$RiskInput
      
      VaR = quantile(X,risk,type=1)
      CVaR = ifelse(abs(risk-0)<(1e-10),min(X),(mean(X*(X<=VaR)) + VaR*(risk - mean(X<=VaR)))/risk)
      E = mean(X)
      EVaR = EVAR(X,levels = ERM_levels,risk=1-risk)
      
      return(list(X = X, VaR = VaR, CVaR = CVaR,E = E,EVaR = EVaR ))
    })
    
    # Plot PDF and risk measures
    output$distPlot <- renderPlot({
      list[X,VaR,CVaR,E,EVaR] = compMeasures()
      # Draw GGplot
      line.data <- data.frame(xintercept = c(VaR, CVaR,EVaR,E), Measure = c("VaR", "CVaR","EVaR","Mean"),
                              stringsAsFactors = FALSE)
      ggplot(data.frame(X=X),aes(x=X,fill = after_stat(x > VaR))) + 
        theme(legend.position="top")+
        theme(legend.position="right") +
        geom_histogram(aes(y = ..density..), alpha = 0.4,position = position_dodge(),bins=100)+
        geom_vline(aes(xintercept = xintercept, color = Measure), line.data, size = 1) +
        scale_color_manual(values=c("red", "darkred", "black","blue"))
    })
    
    # Output Risk Measure Value
    output$measure <- renderText({
      list[X,VaR,CVaR,E,EVaR] = compMeasures()
      
      paste("\nMean :",E,"\nVaR :", VaR,"\nCVaR :",CVaR,"\nEVaR :",EVaR)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
