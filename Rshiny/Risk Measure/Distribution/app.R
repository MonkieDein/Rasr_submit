setwd("~/Desktop/GITHUB/Rasr_submit")
# setwd("C:/GITHUB/Rasr_submit")
rm(list=ls())
source("Code/RASR_code.R")
library(ggplot2)
# library(plotly)
library(shiny)
library(shinydashboard)
# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage(
      # Application title
      "Distribution and Risk Measure",
      tabPanel("Dist vs Measure",
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
            numericInput("SampleInput", label = "Samples :", min = 1, 
                         max = 10000, value = 100,step = 1),
            # Seed index Input
            numericInput(inputId = "SeedInput",label = "Seed :",
                         min = 0,max = 100,value = 1,step=1)
          ),
          
          # Show a plot of the generated distribution
          mainPanel(
            plotOutput("distPlot"),
            verbatimTextOutput("measure")
          )
        )
      ),
      tabPanel("Measure Change over risk",
               # Sidebar with a slider input for number of bins 
               sidebarLayout(
                 sidebarPanel(
                   # Risk Measure checkboxes
                   checkboxGroupInput("checkMeasure", label = "Risk Measure", 
                                      choices = list("VaR" = "VaR", "CVaR" = "CVaR", "EVaR" = "EVaR", "Mean" = "Mean"),
                                      selected = c("VaR","CVaR","EVaR","Mean")),
                   # Quantile Discretization 
                   selectInput(inputId = "QDisInput", label ="Discretization :", 
                               choices = list("0.0001" = 0.0001,"0.001" = 0.001, "0.01" = 0.01, "0.1" = 0.1), 
                               selected = 3),
                   # Distribution Input 
                   selectInput(inputId = "DistInput2", label ="Distribution :", 
                               choices = list("Uniform" = "runif", "Normal" = "rnorm", "Exp" = "rexp"), 
                               selected = 1),
                   # Number of samples Input
                   numericInput("SampleInput2", label = "Samples :", min = 1, 
                                max = 10000, value = 100,step = 1),
                   # Seed index Input
                   numericInput(inputId = "SeedInput2",label = "Seed :",
                                min = 0,max = 100,value = 1,step=1)
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   plotOutput("measurePlot"),
                 )
               )
      )
      
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    ERM_levels = (0.99^(1:3000))*10000
    # Compute value for each risk measure 
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
    
#----------------------- PAGE 1 [ Dist vs Measure ] ------------------------#
    # Plot PDF and risk measures
    output$distPlot <- renderPlot({
      list[X,VaR,CVaR,E,EVaR] = compMeasures()
      # Draw GGplot
      line.data <- data.frame(xintercept = c(VaR, CVaR,EVaR,E), Measure = c("VaR", "CVaR","EVaR","Mean"),
                              stringsAsFactors = FALSE)
      # ggplotly(
      ggplot(data.frame(X=X),aes(x=X,fill = after_stat(x > VaR))) + 
        theme(legend.position="right",text = element_text(size = 20)) +
        geom_histogram(aes(y = ..density..), alpha = 0.4,position = position_dodge(),bins=100)+
        geom_vline(aes(xintercept = xintercept, color = Measure), line.data, size = 1) +
        scale_color_manual(values=c("red", "darkred", "black","blue"))
      # )
    })
    
    # Output Risk Measure Value
    output$measure <- renderText({
      list[X,VaR,CVaR,E,EVaR] = compMeasures()
      
      paste("\nMean :",E,"\nVaR :", VaR,"\nCVaR :",CVaR,"\nEVaR :",EVaR)
    })
    
#----------------------- PAGE 2 [ Measure Change over risk ] ------------------------#
    # Plot Measure CHange over risk
    output$measurePlot <- renderPlot({
      set.seed(input$SeedInput2)
      X = eval(parse(text=
                       paste0(input$DistInput2,"(",input$SampleInput2,")")
      ))

      Risks = seq(0,1,as.numeric(input$QDisInput))

      for (measure in input$checkMeasure){
        # choices = list("VaR" = "VaR", "CVaR" = "CVaR", "EVaR" = "EVaR", "Mean" = "Mean"),
        if (measure == "VaR"){
          VaRs = quantile(X,Risks,type=1)
          dfVaR = data.frame(Risk = Risks, Value=VaRs, Measure = "VaR")
        } else if (measure == "CVaR"){
          VaRs = quantile(X,Risks,type=1)
          CVaRs = sapply(1:length(VaRs), function(i)
            ifelse(abs(Risks[i]-0)<(1e-10),min(X),(mean(X*(X<=VaRs[i])) + VaRs[i]*(Risks[i] - mean(X<=VaRs[i])))/Risks[i])
          )
          dfCVaR = data.frame(Risk = Risks, Value=CVaRs, Measure = "CVaR")
        } else if (measure == "EVaR"){
          ERM_levels = (0.99^(1:3000))*10000
          ERMs = sapply(ERM_levels,function(a) ERM(X=X,alpha = a))
          EVaRs = sapply(1-Risks, function(risk) ifelse(abs(risk-1)<1e-10, min(X),max(ERMs + log(1-risk)/ERM_levels)) )
          dfEVaR = data.frame(Risk = Risks, Value=EVaRs, Measure = "EVaR")
        } else if (measure == "Mean"){
          E = mean(X)
          dfMean = data.frame(Risk = Risks, Value=E, Measure = "Mean")
        }
      }

      df = do.call(rbind,lapply(input$checkMeasure,function(measure) eval(parse(text=paste0("df",measure))) ))


      ggplot(df,aes(x=Risk,y=Value, group = Measure)) + geom_line(aes(color=Measure),size=2)+
        theme(legend.position="right") + theme(text = element_text(size = 20))


    })

    
    
    session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)
