# Server file for the Shiny app
# Load libraries
library(shiny)
library(tidyverse)

####################################################################### EXAMPLE
# Server code
server <- function(input, output) {
  
  # Define histogram output
  output$histPlot <- renderPlot({
    
    # Generate 200 samples from normal dist.
    x <- rnorm(n = 200, 
               mean = input$mean,
               sd = sqrt(input$var))
    
    # Plot histogram
    hist(x, breaks = input$bin)
  })
  
  # Define summary statistics output
  output$summary <- renderPrint({
    
    # Generate 200 samples from normal dist.
    x <- rnorm(n = 200, 
               mean = input$mean,
               sd = sqrt(input$var))
    
    # Print summary
    summary(x)
  })
}
####################################################################### EXAMPLE

