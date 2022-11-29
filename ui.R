# ui file for the Shiny app
# Load libraries
library(shiny)
library(shinythemes)
# To build a map
library(ggiraph)


#### UI CODE
ui <- fluidPage(
  
  # App name
  titlePanel("Covid-19 Map Data"),
  
  # Sidebar with a numeric input for mean and var
  # and a slider input for bin
  sidebarLayout(
    
    # Side panel
    sidebarPanel(
      
      # First input: Type of data
      selectInput(inputId = "choice",
                  label = "Data to display:",
                  choices = list(
                    "Number of Cases" = "total_cases", 
                    "Number of Cases per 1 mln of Population" = "cases_per1m",
                    "Number of Deaths" = "total_deaths", # *
                    "Number of Deaths per 1 mln of Population" = "deaths_per1m",
                    "Number of Recoveries" = "total_recovered", 
                    "Number of Active Cases" = "active_cases", 
                    "Number of Active Case per 1 mln of Population" = "active_per1m", 
                    "Number of Serious Cases" = "serious_cases",
                    "Number of Tests" = "total_tests", 
                    "Number of Tests per 1 mln of Population" = "tests_per1m", 
                    "Population" = "population")),
      
      # Button to download current map instant and data
      actionButton(inputId = "getCurrentData",
                   label = "Download Map & Data",
                   )
      ),
    
    # Main panel
    mainPanel(
      
      # Plot the map
      girafeOutput("distPlot")
    )
  )
)
#### END OF UI CODE
