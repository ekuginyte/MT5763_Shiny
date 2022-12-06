# Server file for the Shiny app
packages <- c("shiny", "tidyverse", "robotstxt", "rvest", "maps", "ggmap", "ggplot2",
              "ggiraph", "RColorBrewer", "reshape2", "shinyjs", "shinythemes", 
              "shinydashboard", "sf","rgdal", "leaflet", "shinyWidgets", "mgsub",
              "data.table")

installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# Current date
lastSessionStart <- as.Date(strftime(Sys.time(), "%Y/%m/%d"))

# Data frame containing covid data 2
dat <- get.data(website = "https://www.worldometers.info/coronavirus/", 
         table_name = "#main_table_countries_today")

# Initial dataset
TSData <- get.time.series.data()

### Server
server <- function(input, output){
  
 observe({
   
   # Refresh when clicked
    input$refresh
    # Refresh every hour
    invalidateLater(3600000)
    # Extract time series data
    TSData <- get.time.series.data()
    # Save today's date
    currentDate <- as.Date(strftime(Sys.time(), "%Y/%m/%d"))
    # Extract all possible dates
    dateOptions <- names(TSData[-1])
    dateOptions <- as.Date(dateOptions, "%m/%d/%y")
    
    # Map of time series data
    plot_map_1 <- confirmed_total_map(df = TSData, date = input$plot_date)
    
    updateSliderTextInput(session, "plot_date", choices = dateOptions)
    showNotification("Data Refreshed")
  })
  
  ### Globe plot page
  # Plot the JHU data map
  output$confirmed_map <- 
    renderGirafe({ggiraph(code = print(confirmed_total_map(
      df = TSData, user_input = input$map_data_choice)))#, date = plot_date))
    ggiraph(code = print(confirmed_total_map(df = TSData, user_input = input$map_data_choice)))
    # LEAFLET MAP NEEDS WORK
    #renderLeaflet({
    #confirmed_total_map(user_input = input$map_data_choice, date = plot_date)
  })
    
    # Save plot
  output$downloadMapPlot <- downloadHandler(
      filename <- function() {
        paste("Covid-19_map_", input$map_data_choice, "_", Sys.Date(), ".jpg", sep = "")
      },
      content <- function(con) {
        ggsave(con)
      }
    )
  
  # Plot the world map 
  output$distPlot <- renderGirafe({
    ggiraph(code = print(map(input_choice = input$globeDataChoice)))
  })
  
  # Save plot
  output$downloadGlobePlot <- downloadHandler(
    filename = function(){
      paste("Covid-19_map_",input$globeDataChoice,"_",Sys.Date(),".jpg", sep = "")
    },
    content = function(con) {
      ggsave(con)
    }
  )
  
  ### Data plot page
  
  dfInput <- reactive({
    get.df(input$dataFormat, dat, input$dataChoice, input$dataCountries)})
  
  output$dataToDownload <- renderDataTable(dfInput())
  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("Covid-19_data_", Sys.Date(),'.csv', sep='')
    },
    content = function(con) {
      write.csv(dfInput(), con)
    }
  )
  
  ### Plot page
  
  plotdfInput <- reactive({
    get.df("l", dat, input$plotDataChoice, input$plotCountries)})
  
  # Plot specified map
  output$plot <- renderGirafe({
    ggiraph(code = print(get.plot(plotName = input$plotTypeChoice, df = plotdfInput())))
  })
  
  # Save plot 
  output$downloadPlot <- downloadHandler(
    filename = function(){
      paste("Covid-19_map_",input$plotDataChoice,"_",input$plotTypeChoice,Sys.Date(),".jpg", sep = "")
    },
    content = function(con) {
      ggsave(con)
    }
  )
  
  # Save plot dataset
  output$downloadPlotData <- downloadHandler(
    filename = function(){
      paste("Covid-19_map_",input$plotDataChoice,"_",input$plotTypeChoice,Sys.Date(),".csv", sep = "")
    },
    content = function(con) {
      write.csv(dfInput(), con)
    }
  )
  
}
