# Server file for the Shiny app

### Load packages
if(!require(shiny)) install.packages("shiny")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(robotstxt)) install.packages("robotstxt")
if(!require(rvest)) install.packages("rvest")
if(!require(maps)) install.packages("maps")
if(!require(ggmap)) install.packages("ggmap")
if(!require(ggiraph)) install.packages("ggiraph")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(reshape2)) install.packages("reshape2")
if(!require(shinyjs)) install.packages("shinyjs")
if(!require(shinythemes)) install.packages("shinythemes")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.pakcages("data.table")
if(!require(rgdal)) install.pakcages("rgdal")
if(!require(leaflet)) install.pakcages("leaflet")
if(!require(shinyWidgets)) install.pakcages("shinyWidgets")
if(!require(data.table)) install.pakcages("data.table")

### Load libraries
library(shiny)
library(tidyverse)
library(robotstxt)
library(rvest)
library(maps)
library(ggmap)
library(ggiraph)
library(RColorBrewer)
library(rgdal)
library(reshape2)
library(shinyjs)
library(shinythemes)
library(maps)
library(data.table)
library(mgsub)
library(leaflet)
library(shinyWidgets)
library(data.table)

# Initialize variables

# Current date
lastSessionStart <- as.Date(strftime(Sys.time(), "%Y/%m/%d"))

# Data frame containing covid data 2
dat <- get.data(website = "https://www.worldometers.info/coronavirus/", 
         table_name = "#main_table_countries_today")

# Map of time series data
plot_map_1 <- confirmed_total_map()

### Server
server <- function(input, output){
  
  observe({
    input$refresh
    invalidateLater(3600000) # ms
    TSData <- get.time.series.data()
    currentDate <- as.Date(strftime(Sys.time(), "%Y/%m/%d"))
    dateOptions <- names(TSData[-1])
    
    # Map of time series data
    plot_map_1 <- confirmed_total_map()
    
    updateSliderTextInput(session, "plot_date", choices = dateOptions)
    
    output$TSData <- renderTable(TSData)
    
    showNotification("Data Refreshed")
    
  })
  
  ### Globe plot page
  # Plot the JHU data map
  output$confirmed_map <- renderLeaflet({
    confirmed_total_map(user_input = input$map_data_choice, date = plot_date)
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
