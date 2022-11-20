# ui file for the Shiny app
# Load libraries
library(shiny)
library(shinythemes)

####################################################################### EXAMPLE
# UI code 
ui <- fluidPage(
  
  # Pre-set the theme
  theme = shinytheme("slate"),
  
  # Let the user select the theme
  themeSelector(),
  sidebarPanel(
    textInput("txt", "Text input:", "text here"),
    sliderInput("slider", "Slider input:", 1, 100, 30),
    actionButton("action", "Button",  icon = icon("cloud")),
    actionButton("action2", "Button2", class = "btn-primary")
  )
  
  # App name
  titlePanel("Simple histogram"),
  
  # Sidebar with a numeric input for mean and var
  # and a slider input for bin
  sidebarLayout(
    
    # Side panel
    sidebarPanel(
      # Mean
      numericInput(inputId = "mean",
                   label = "Mean of normal distributon.",
                   value = 0),
      # Var
      numericInput(inputId = "var",
                   label = "Variance of normal distributon.",
                   value = 1,
                   min = 0.01),
      # Bin
      sliderInput(inputId = "bin",
                  label = "Number of bins.",
                  value = 20,
                  min = 1,
                  max = 50)
    ),
    
    # Main panel
    mainPanel(
      # Histogram
      plotOutput(outputId = "histPlot"),
      # Summary statistics
      verbatimTextOutput(outputId = "summary")
    )
  )
)
####################################################################### EXAMPLE
