# ui file for the Shiny app
# Current date
lastSessionStart <- as.Date(strftime(Sys.time(), "%Y/%m/%d"))

### UI
ui <- fluidPage(
# Enable additional features
shinyjs::useShinyjs(), 
  
  # Navbar structure for ui
  navbarPage("Covid-19 Tracker", theme = shinytheme("paper"),
             
  ### World map data 1
  tabPanel("Historical Map", icon = icon("map"),
           sidebarLayout(
             sidebarPanel(
               
               # Input date to display
               dateInput("plot_date",
                        label = "Date: ",
                        min = as.Date("01/22/20", format = "%m/%d/%y"),
                        max = as.Date(strftime(Sys.time(), "%Y/%m/%d")),
                        value = as.Date("01/01/21")),
                          
              # Input the type of covid data to display
               selectInput(inputId = "map_data_choice",
                           label = "Data to display:",
                           choices = list(
                             "Number of Cases" = "confirmed",
                             "Numver of Deaths" = "deaths",
                             "Number of Recovered" = "recovered"),
                           selected = "confirmed")
      ),
      
      mainPanel(
        # Output the map generated
        girafeOutput("worldMap1")
      )
  )),
           
  ### World map data 2
  tabPanel("Current Map", icon = icon("globe"),
  
    # Sidebar with a numeric input for mean and var
    # and a slider input for bin
    sidebarLayout(
      # Side panel
      sidebarPanel(
        # INPUT: Type of data
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
        
        # OUTPUT: Button to download current map instant and data
        actionButton(inputId = "getCurrentData",
                     label = "Download Map"),
        
        # OUTPUT: Download button for the world plot shown
        downloadLink('downloadGlobePlot', 'Download Plot Data')
        ),
      
      # Main panel
      mainPanel(
        
        # OUTPUT: Plot the globe map
        girafeOutput("distPlot", height = "120%")
      )
    )
  ),
  
  ### Data page
  tabPanel("Data", icon = icon("database"),
           
           sidebarLayout(
             sidebarPanel(
               # INPUT: Select choice of data
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
               
               # INPUT: Select countries
               selectInput(inputId = "dataCountries",
                           label = "Countries",
                           choices = unique(map_data("world")$region),
                           multiple = TRUE,
                           selected = "UK"),
               
               # INPUT: Toggle between long and wide format
               selectInput(inputId = "dataFormat",
                            label = "Format of data",
                            choices = list("Long" = "l",
                                            "Wide" = "w")),
               
               # OUTPUT: Download the current data frame from query
               downloadLink('downloadData', 'Download Data')
             ),
             
           mainPanel(
             
             # OUTPUT: Display data frame from query
             dataTableOutput("dataToDownload")
           )
  )),
  
  ### Plots page
  
  tabPanel("Plots", icon = icon("chart-column"),
           
           sidebarLayout(
             # Side panel
             sidebarPanel(
               
               # INPUT: data to display
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
               
               # INPUT: choice of plot
               selectInput(inputId = "plotTypeChoice",
                           label = "Graph Type:",
                           choices = list(
                             "Vertical Bar Chart" = "vbar",
                             "Pie Chart" = "pie"),
                           selected = "vbar"),
               
               # INPUT: coutries to plot data from
               selectizeInput(inputId = "plotCountries",
                           label = "Countries (max 10)",
                           choices = unique(map_data("world")$region),
                           multiple = TRUE,
                           selected = "UK",
                           options = list(maxItems = 10)
                           ),
               
               # OUTPUT: Link to download plot shown
               downloadLink('downloadPlotData', 'Download Plot Data'),
               
               # OUTPUT: Link to download plot dataset
               downloadLink('downloadPlot', 'Download Plot Data')
             ),
             # Main panel
             mainPanel(
               
               # OUTPUT: Plot the map from above query
               girafeOutput("plot")
             )
           )
          
  ),
  
  actionButton(inputId = "refresh", label = "Refresh Data"),
  
  navbarMenu("More",
             tabPanel("About", 
                      fluidRow(column(6,
                                      includeMarkdown("Readme.md")))),
             
             tabPanel("Current Data Set"
                      ))

)
)
