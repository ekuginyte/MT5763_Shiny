# ui file for the Shiny app
# Load libraries
#library(shiny)
#library(shinythemes)
#library(shinyjs)
# To build a map
#library(ggiraph)
# To get map data
#library(maps)
#library(ggmap)


#### UI CODE
ui <- fluidPage(
  
# Navbar structure for ui
  navbarPage("Covid-19 Tracker", theme = shinytheme("yeti"),
  
  ### Global plot page
  
  tabPanel("Global Plot", icon = icon("globe"),
  
    # Sidebar with a numeric input for mean and var
    # and a slider input for bin
    sidebarLayout(
      # Side panel
      sidebarPanel(
        # First input: Type of data
        selectInput(inputId = "globeDataChoice",
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
                     label = "Download Map")
        ),
      # Main panel
      mainPanel(
        
        # Plot the map
        girafeOutput("distPlot")
      )
    )
  ),
  
  ### Data page
  
  tabPanel("Data", icon = icon("database"),
           
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput(inputId = "dataChoice",
                             label = "Choice of data to display",
                             choiceNames = list(
                               "Number of Cases", 
                               "Number of Cases per 1 mln of Population",
                               "Number of Deaths",
                               "Number of Deaths per 1 mln of Population",
                               "Number of Recoveries", 
                               "Number of Active Cases", 
                               "Number of Active Case per 1 mln of Population", 
                               "Number of Serious Cases",
                               "Number of Tests", 
                               "Number of Tests per 1 mln of Population", 
                               "Population"
                             ),
                             choiceValues = list(
                               "total_cases", 
                               "cases_per1m",
                               "total_deaths",
                               "deaths_per1m",
                               "total_recovered", 
                               "active_cases", 
                               "active_per1m", 
                               "serious_cases",
                               "total_tests", 
                               "tests_per1m", 
                               "population"
                             ),
                             selected = list("Number of Cases")
                             ),
               
               # Used to disable country selection if all selected
               shinyjs::useShinyjs(), 
               
               selectInput(inputId = "dataCountries",
                           label = "Countries",
                           choices = unique(map_data("world")$region),
                           multiple = TRUE,
                           selected = "UK"),
               
               selectInput(inputId = "dataFormat",
                            label = "Format of data",
                            choices = list("Long" = "l",
                                            "Wide" = "w")),
               
               downloadLink('downloadData', 'Download Data')
             ),
           
           mainPanel(
             dataTableOutput("dataToDownload")
           )
  )),
  
  ### Plots page
  
  tabPanel("Plots", icon = icon("chart-column"),
           
           sidebarLayout(
             # Side panel
             sidebarPanel(
               # First input: Type of data
               selectInput(inputId = "plotDataChoice",
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
               
               selectInput(inputId = "plotTypeChoice",
                           label = "Graph Type:",
                           choices = list(
                             "Vertical Bar Chart" = "vbar",
                             "Histogram" = "hist")),
               
               selectInput(inputId = "plotCountries",
                           label = "Countries",
                           choices = unique(map_data("world")$region),
                           multiple = TRUE,
                           selected = "UK"),
               
               # Button to download current map instant and data
               actionButton(inputId = "getCurrentPlot",
                            label = "Download Map"
               )
               
             ),
             # Main panel
             mainPanel(
               
               # Plot the map
               girafeOutput("plot")
             )
           )
          
)))
#### END OF UI CODE
