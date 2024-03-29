#### UI
ui <- fluidPage(
  
# Enable additional features
shinyjs::useShinyjs(), 

  # Navbar structure for ui
  navbarPage("Covid-19 Tracker", theme = shinytheme("yeti"),     
  ################################################################################
  ### Map data page
  tabPanel("Data Map", icon = icon("globe"),
           sidebarLayout(
             sidebarPanel(
               # INPUT: User selects the date
               sliderTextInput("plot_date",
                               label = paste("Date (most recent: ", maxDate,"): "),
                               choices = dateOptions,
                               selected = format(as.Date(
                               strftime(Sys.time(), "%Y/%m/%d"), ) - 3, "%m/%d/%y"),# -3 due to local timezone issues
                               #grid = FALSE,
                               animate = animationOptions(interval = 300, loop = FALSE)
               ),
               
              # INPUT: Type of covid data to display
               selectInput(inputId = "map_data_choice",
                           label = "Data to display:",
                           choices = list(
                             "Confirmed cases per million of population" = "confirmed",
                             "Deaths per million of population" = "deaths",
                             "Recoveries per million of population" = "recovered"),
                           selected = "confirmed"),
              
              fluidRow( 
              # OUTPUT: Download the map
              downloadLink('download_map_plot', 'Download Globe Map')
                       ),

              fluidRow(
              # OUTPUT: Download the map data
              downloadLink('download_map_data', 'Download Globe Map Data')
              )
      ),
      
      mainPanel(
        # Output the map generated
        girafeOutput("world_map", height = "115%")
      )
  )),
  ################################################################################
  ### Data page
  tabPanel("Data", icon = icon("database"),
           
           sidebarLayout(
             sidebarPanel(
               # INPUT: Select choice of data
               checkboxGroupInput(inputId = "data_page_choice",
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
                             selected = "confirmed"
                             ),
               
               # INPUT: Select countries
               selectInput(inputId = "data_countries",
                           label = "Countries",
                           choices = all_regions,
                           multiple = TRUE,
                           selected = "Great Britain"),
               
               # INPUT: Toggle between long and wide format
               selectInput(inputId = "data_format",
                            label = "Format of data",
                            choices = list("Long" = "l",
                                            "Wide" = "w")),
               fluidRow(
               # OUTPUT: Download the current data frame from query
               downloadLink('download_data', 'Download Data')
               )
             ),
             
           mainPanel(
             
             # OUTPUT: Display data frame from query
             dataTableOutput("data_to_download")
           )
  )),
  ################################################################################
  ### Plots page
  tabPanel("Plots", icon = icon("chart-column"),
           
           sidebarLayout(
             # Side panel
             sidebarPanel(
               
               # INPUT: data to display
               selectInput(inputId = "plot_data_choice",
                           label = "Data to display:",
               
                           choices = list(
                             "Confirmed cases per million of population" = "confirmed",
                             "Deaths per million of population" = "deaths",
                             "Recoveries per million of population" = "recovered")),
               
               # INPUT: choice of plot
               selectInput(inputId = "plot_type_choice",
                           label = "Graph Type:",
                           choices = list(
                            "Vertical Bar Chart" = "vbar",
                            "Pie Chart" = "pie"),
                           selected = "vbar"),
               # INPUT: countries to plot data from
               selectizeInput(inputId = "plot_countries",
                           label = "Countries (max 10)",
                           choices = all_regions,
                           multiple = TRUE,
                           selected = "UK",
                           options = list(maxItems = 10)
                           ),
               
               # OUTPUT: Link to download plot dataset as csv
               fluidRow(
               downloadLink('download_plot', 'Download Plot')
               ),
               
               # OUTPUT: Link to download plot shown as png
               fluidRow(
               downloadLink('download_plot_data', 'Download Plot Data')
               )
               
             ),
             # Main panel
             mainPanel(
               
               # OUTPUT: Plot the map from above query
               girafeOutput("stat_plot")
             )
           )
          
  ),
  ################################################################################
  ### Additional info
  navbarMenu("More",
             tabPanel("About", 
                      fluidRow(column(6,
                                      includeMarkdown("Readme.md")))),
             
             tabPanel("Current Data Set",
                      fluidPage(
                        textOutput("last_refresh"),
                        actionButton(inputId = "refresh", label = "Refresh Data")
                      )

             )

)
))
################################################################################