#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinyjs)

source("probtreefuncs.R")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Probability Tree"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Number of splits",
                        min = 2,
                        max = 10,
                        value = 3),
            sliderInput("pA", "Proability of Event A",min=0,max=1,value=.5 ),
            verbatimTextOutput("pB"),
            actionButton("go", "Draw a tree!")
            #sliderInput("pB", "Proability of Event B",min=0,max=1,value=.5 ),
            #sliderInput("pC", "Proability of Event C",min=0,max=1,value=.5 ),
            #sliderInput("pD", "Proability of Event D",min=0,max=1,value=.5 )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
                plotOutput("distPlot", width="100%", height = "50%")
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    source("probtreefuncs.R")
    output$pB <- renderText(paste("Probability of Event B:", 1-input$pA))
    #observeEvent(input$probA, {shinyjs::reset("go")})
    observeEvent({input$n 
        input$go}, {
        showModal(modalDialog("Hang tight while your tree grows.", footer=NULL))
        output$distPlot <- renderPlot({
            pB <- 1-input$pA
            mat <- matmake(input$n)
            plt <- ggplot()
            mat[,6] <- condprob(input$pA, pB, mat,input$n)
            #mat[,5] <- 1:nrow(mat)
            #df <- data.frame()
            
             for (i in 1:nrow(mat)){
                plt <- seg_draw(plt, mat[i,1], mat[i,2], mat[i,3],mat[i,4],mat[i,5], mat[i,6])
                print(plt)
            }
            
        removeModal()
        
    }, height = 800, width = 600 )
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
