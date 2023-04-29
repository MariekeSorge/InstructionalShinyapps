#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Type I and Type II Error"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("sm",
                        "Alternative Mean",
                        value=0)
        ,numericInput("ss",
                      "Alternative Standard Deviation",
                      value=1, min=0, max=  100),
        numericInput("tm",
                       "Null Mean",
                       value=5, min=-100, max=  100), 
        numericInput("ts",
                     "Null Standard Deviation",
                     value=1), 
        sliderInput("a",
                     "Alpha",
                     min=0, max=1, value=.05, step=.01)
        
    ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           dataTableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      
      set.seed(8723264)         # Create example data
      x <- rnorm(100000, input$tm, input$ts)
      y <- rnorm(100000, input$sm, input$ss)
      plot(density(x), main ="Type I and Type II error",  xlab = "Mean", ylim = c(0,max(max(dnorm(y)), max(dnorm(x)))+.1))          # Plot density of x
           #xlim = c(- 6, 6),
           #ylim = c(0, 0.4))
      lines(density(y))
      #shadex <- c(x[which(x <= min(y))], x[which(x > min(x))])
      #shadey <- c(density(x)[which(x <= min(y))], density(x)[which(x > min(x))])
      right <- qnorm(input$a, sd=input$ts)
      left <- qnorm((1-input$a),sd=input$ts)
      xP <- c(seq(min(y),max(y),by=0.01))
      yP <- dnorm(xP,mean= input$sm, sd=input$ss)
      yP[xP > right +input$tm   | xP < left +input$tm] <- 0
      polygon(density(y), col="purple")
      #text(input$tm, max(dnorm(x)+.07), "H0")
      
      #text(input$sm, max(dnorm(y)+.03), "H1")
      ## x and y for the whole area
      xReject <- c(seq(min(x),max(x),by=0.01))
      yReject <- dnorm(xReject,mean= input$tm, sd=input$ts)
      
      ## set y = 0 for the area that is not rejected  
      yReject[xReject < left +input$tm & xReject > right +input$tm] <- 0
      rightb <- qnorm((1-input$a),sd=input$ss)
      xBeta <- c(seq(min(y),max(y),by=0.01))
      yBeta <- dnorm(xBeta,mean= input$sm, sd=input$ss)
      yBeta[xBeta < right +input$tm   | xBeta > left +input$tm] <- 0
      ## Plot the red areas
      polygon(c(xReject,xReject[length(xReject)],xReject[1]),
              c(yReject,0, 0), col='red')
      polygon(c(xBeta,xBeta[length(xBeta)],xBeta[1]),
              c(yBeta,0, 0), col='blue')
      legend("topleft", legend=c("Alpha", "Beta", "Power"),
             col=c("red", "blue", "purple"), lty=1, cex=0.8)
    })
    
    output$table <- renderDataTable({
      beta <- 1-pnorm(input$tm - qnorm(input$a)*input$ts, input$sm, input$ss)
      tab <- data.frame(matrix(nrow=2, ncol=2))
      tab$'Null Hypothesis is' <- c("Rejected", "Not rejected")
      tab$'True' <- c(paste("Type I Error False Positive Probability", as.character(input$a)), paste("Correct decision True Negative Probability", as.character(1-input$a)))
      tab$'False' <-  c(paste("Correct decision True Positive Probability", as.character(1-beta)), paste("Type II Error False Negative Probability", as.character(beta)))
      tab <- tab[,-c(1:2)]
      print(tab)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
