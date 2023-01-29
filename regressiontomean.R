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
    titlePanel("Regression Toward the Mean"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("sm",
                         "Mean",
                         value=0)
            ,numericInput("ss",
                          "Standard Deviation",
                          value=1, min=0, max=  100),
            actionButton("samp", "Sample a Point"),
            actionButton("samp2", "Sample a Second Point")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            verbatimTextOutput("outcome")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    r <- reactiveValues(p=NULL,p2 = NULL, x=NULL, xseq = NULL, yseq =NULL )
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        set.seed(8723264)         # Create example data
        r$x <- rnorm(1000000, input$sm, input$ss)
        #y <- rnorm(100000, input$sm, input$ss)
        plot(density(r$x), main ="Distribution")          # Plot density of x
        # observeEvent(input$samp, {r$p <- rnorm(1, input$sm, input$ss)
        # 
        # output$distPlot <- renderPlot({
        #     #set.seed(8723264)         # Create example data
        #     x <- rnorm(1000000,input$sm, input$ss)
        #     plot(density(x), main ="Distribution")
        #     abline(v=r$p, col="red")})  })
        # 
        observeEvent(input$samp, {r$p <- rnorm(1, input$sm, input$ss)
        
        output$distPlot <- renderPlot({
            #set.seed(8723264)         # Create example data
            #x <- rnorm(1000000,input$sm, input$ss)
            plot(density(r$x), main ="Distribution")
            abline(v=r$p, col="red")
            if (r$p> input$sm){ 
            right <- r$p
            left <- input$sm
            }else {
                right <- input$sm
                left <- r$p
            }
            
            r$xseq <- seq(left, right, length.out = 1000)
            r$yseq <- dnorm(r$xseq,mean= input$sm, sd=input$ss)
            polygon(c(r$xseq,r$xseq[length(r$xseq)],r$xseq[1]),
                             c(r$yseq,0, 0), col='red')
           
            })  })
        observeEvent(input$samp2, {
                     r$p2 <- rnorm(1,input$sm, input$ss)
                     output$distPlot <- renderPlot({
                         #set.seed(8723264)         # Create example data
                         #x <- rnorm(1000000,input$sm, input$ss)
                         plot(density(r$x), main ="Distribution")
                         abline(v=r$p, col="red")
                         # if (r$p> input$sm){ 
                         #     right <- r$p
                         #     left <- input$sm
                         # }else {
                         #     right <- input$sm
                         #     left <- r$p
                         # }
                         # 
                         # xseq <- seq(left, right, by=.01)
                         
                         #yseq <- dnorm(xseq,mean= input$sm, sd=input$ss)
                         polygon(c(r$xseq,r$xseq[length(r$xseq)],r$xseq[1]),
                                 c(r$yseq,0, 0), col='red')
                         abline(v=r$p2, col="blue")
                         if((r$p2-input$sm)^2 < (r$p-input$sm)^2){
                             output$outcome <- renderText("Regression toward the mean!")
                         } else{output$outcome <- renderText("Nope")}
                                  }) })
        
        
        # right <- qnorm(p, sd=input$ts)
        # left <- qnorm((1-p),sd=input$ts)
        # xP <- c(seq(min(y),max(y),by=0.01))
        # yP <- dnorm(xP,mean= input$sm, sd=input$ss)
        # yP[xP > right +input$sm   | xP < left +input$sm] <- 0
        # polygon(density(yP), col="purple")
        # #text(input$tm, max(dnorm(x)+.07), "H0")
        
        #text(input$sm, max(dnorm(y)+.03), "H1")
        ## x and y for the whole area
        # xReject <- c(seq(min(x),max(x),by=0.01))
        # yReject <- dnorm(xReject,mean= input$tm, sd=input$ts)
        # 
        # ## set y = 0 for the area that is not rejected  
        # yReject[xReject < left +input$tm & xReject > right +input$tm] <- 0
        # rightb <- qnorm((1-input$a),sd=input$ss)
        # xBeta <- c(seq(min(y),max(y),by=0.01))
        # yBeta <- dnorm(xBeta,mean= input$sm, sd=input$ss)
        # yBeta[xBeta < right +input$tm   | xBeta > left +input$tm] <- 0
        # ## Plot the red areas
        # polygon(c(xReject,xReject[length(xReject)],xReject[1]),
        #         c(yReject,0, 0), col='red')
        # polygon(c(xBeta,xBeta[length(xBeta)],xBeta[1]),
        #         c(yBeta,0, 0), col='blue')
        #legend("topleft", legend=c("Sampled Point 1", "Distance to Mean", "Sampled point 2"),
        #       col=c("red", "blue", "purple"), lty=1, cex=0.8)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

