# ui file for the Shiny app
# Current date
lastSessionStart <- as.Date(strftime(Sys.time(), "%Y/%m/%d"))

### UI
ui <- fluidPage(
# Enable additional features
shinyjs::useShinyjs(), 
  
  # Navbar structure for ui
  navbarPage("Covid-19 Tracker", theme = shinytheme("paper"),
             
  ### Map data page
  tabPanel("Data Map", icon = icon("map"),
           sidebarLayout(
             sidebarPanel(
               # INPUT: User selects the date
               sliderTextInput("plot_date",
                               label = "Date:",
                               choices = dateOptions,
                               selected = lastRefresh,
                               grid = FALSE,
                               animate = animationOptions(interval = 5, loop = FALSE)),
                          
              # INPUT: Type of covid data to display
               selectInput(inputId = "map_data_choice",
                           label = "Data to display:",
                           choices = list(
                             "Confirmed cases per million of population" = "confirmed",
                             "Deaths per million of population" = "deaths",
                             "Recoveries per million of population" = "recovered"),
                           selected = "confirmed"),
              
              # OUTPUT: Download the map
              downloadLink('download_map_plot', 'Download Map'),
              
              # OUTPUT: Download the map data
              downloadLink('download_map_data', 'Download Map Data')
      ),
      
      mainPanel(
        # Output the map generated
        girafeOutput("world_map", height = "120%")
      )
  )),
           
 
  
  ### Data page
  tabPanel("Data", icon = icon("database"),
           
           sidebarLayout(
             sidebarPanel(
               # INPUT: Select choice of data
               checkboxGroupInput(inputId = "map_data_choice",
                             label = "Data to display",
                             choiceNames = list(
                               "Confirmed cases per million of population",
                               "Deaths per million of population",
                               "Recoveries per million of population"
                             ),
                             choiceValues = list(
                               "confirmed",
                               "deaths",
                               "recovered"
                             ),
                             selected = list("Number of Cases")
                             ),
               
               # INPUT: Select countries
               selectInput(inputId = "dataCountries",
                           label = "Countries",
                           choices = unique(df_stats$Region),
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
                             "Confirmed cases per million of population" = "confirmed",
                             "Deaths per million of population" = "deaths",
                             "Recoveries per million of population" = "recovered")),
               
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
                           choices = unique(df_stats$Region),
                           multiple = TRUE,
                           selected = "UK",
                           options = list(maxItems = 10)
                           ),
               
               # OUTPUT: Link to download plot shown as jpg
               downloadLink('downloadPlotData', 'Download Plot'),
               
               # OUTPUT: Link to download plot dataset as csv
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
