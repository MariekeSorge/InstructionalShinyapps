  library(shiny)
  library(shinyjs)
  ui <- fluidPage(
    useShinyjs(),

    # Application title
    titlePanel("Central Limit Theorem Simulator"),

    sidebarLayout(

      # Sidebar with a slider input
      sidebarPanel(
          helpText("Select the sample size, and the distribution of the population. Then use the button to take samples. See how the distribution of the means changes as you take more samples, or if you take samples of a different size."),
      #numericInput(
        #  "n", label = "Number of Samples",
        # value = 30
        #),
    
        numericInput(inputId = "ss",
                "Sample size", value = 15),
          selectInput("dist", h3("Select Distribution to Sample from"), 
                        choices = list("Normal" = 1, "Uniform" = 2
                                        ,"Log Normal" = 3, "Beta" = 4
                                        ), selected = 1),
          #sliderInput("go", "Sample #", value=1, min=1, max=1000)
          textOutput("value")
          #actionButton("sampd", "Reset the experiment"),

      ),

      # Show a plot of the generated distribution
      mainPanel(
        helpText("A plot of the population distribution is provided to compare to the distribution of means from the sample."),
        plotOutput("popPlot"),
        actionButton("sampa", "Take a Sample"),
        actionButton("rsamp", "Reset sample counter"),
        
        helpText("The sampled points appear in red on the plot of the populataion."),
          plotOutput("sampPlot"),
          actionButton("means", "Plot the Means of the samples"), 
          actionButton("reset", "Clear the Means Plot"), 
          helpText("For each sample a mean is recorded an plotted on this histogram"),
        plotOutput("distPlot"),
        
        
      )
    )
  )

  # Server logic
  server <- function(input, output, session) {
    nc <- 0
    #observeEvent(input$sampd, {nc <- 0})
    r <- reactiveValues(nc = 0, pnc =0)
    observe({
      observeEvent(input$sampd, {
        updateNumericInput(session, "ss", value = 15)
        updateActionButton(session, "sampa")
        updateActionButton(session, "means")
      })
          means <- data.frame(matrix(ncol=1, nrow=1000))
          samps <- data.frame(matrix(ncol=input$ss, nrow=1000))
          pop <- if(input$dist == 1){
                              rnorm(1000*5)
                              } else if (input$dist == 2) {
                                  runif(1000*5)
                              } else if (input$dist == 3){
                                  rlnorm(1000*5)
                              } else if (input$dist == 4){
                                rbeta(1000*5,2,5)
                              }
          for (i in 1:1000){
              samp <- sample(pop, input$ss)
                              
                                  
                                    
              samps[i,] <- samp
              means[i,] <- mean(samp)
          }
          
      #nc <- input$go
          

     observeEvent(input$sampa, {
        
       r$nc <- input$sampa - r$pnc
       #observeEvent(input$reset, {nc <- 0})
       output$value <- renderText({paste("Number of samples taken:", r$nc)})
      output$sampPlot <- renderPlot({
        #plot(as.numeric(1:(input$ss*input$n*10)), pop)
        
      plot(as.numeric(1:(1000*5)), pop, xlab = "Index", ylab = "Value", main = "Most recent sample ")
      points(which(pop %in% samps[r$nc,]), samp, col="red", pch=19, cex=2) 
      
      }) 
      
      #hist(samp)
      })
     observeEvent(input$rsamp, {
       r$nc <- 0
       r$pnc <- input$sampa
       output$sampPlot <- NULL
       output$distPlot <- NULL 
       })
     observeEvent(input$dist, {
      output$popPlot <- renderPlot({
    hist(pop, xlab = "Population", main = "Distribution of Population")
    r$nc <- 0
    output$sampPlot <- NULL
    output$distPlot <- NULL
    })
    })
     observeEvent(input$ss, {
       r$nc <- 0
       output$sampPlot <- NULL
       output$distPlot <- NULL 
     })
    observeEvent(input$means , {
    output$distPlot <-
    renderPlot({
      hist(means[1:r$nc,1], xlab = "Sample Means", main = "Distribution of Sample Means")})
  })
    observeEvent(input$reset,{
      output$distPlot <- NULL }) 
    
    })
  }
  # Complete app with UI and server components
  shinyApp(ui, server)