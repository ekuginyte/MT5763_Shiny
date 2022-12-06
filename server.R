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
if(!require(data.table)) install.pakcages("data.table")
if(!require(rgdal)) install.pakcages("rgdal")

### Load libraries
library(shiny)
library(tidyverse)
# Check if scraping is allowed
library(robotstxt)
# Scrape data
library(rvest)
# Extract world map data
library(maps)
library(ggmap)
# To build a map
library(ggiraph)
# Colours
library(RColorBrewer)
library(rgdal)
# Data frame re-format
library(reshape2)
# Dynamic shiny bits
library(shinyjs)
library(shinythemes)
# Formating data
library(data.table)


lastRefresh <- Sys.time()

dat <- get.data(website = "https://www.worldometers.info/coronavirus/", 
         table_name = "#main_table_countries_today")

### Server
server <- function(input, output){
  
  ### Globe plot page
  # Plot the JHU data map
  output$confirmed_map <- renderLeaflet({
    confirmed_total_map(user_input = input$map_data_choice, maxDate = plot_date)
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
