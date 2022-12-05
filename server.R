# Server file for the Shiny app
# Load libraries
library(shiny)
library(tidyverse)
# Check if scraping is allowed
library(robotstxt)
# Scrape data
library(rvest)
# Extract world map data
library(maps)
# To build a map
library(ggiraph)
# Colours
library(RColorBrewer)
# Data frame re-format
library(reshape2)
# Dynamic shiny bits
library(shinyjs)
# ggplot
library(ggplot2)
library(ggmap)
library(ggiraph)
library(shinythemes)
library(maps)

#### EXTRACT DATA
# Function to create a covid data frame
#   INPUT:
#     website - site where data will be scraped from,
#     table_name - name of table from the site.
#   OUTPUT:
#     dat - data frame containing covid data.
get_data <- function(website, table_name) {
  # Check if the inputs are correct
  if (is.na(website) || is.numeric(website)) {
    stop("Invalid website url!")
  }
  if (is.na(table_name) || is.numeric(table_name)) {
    stop("Invalid table link!")
  }
  # Check if scraping is allowed
  if (paths_allowed(website) == FALSE) {
    stop("Not allowed to scrape data!")
  }
  
  # Create empty data frame
  data <- data.frame(NA)
  
  # Download the whole web page
  html_page <- read_html(website) 
  
  # Access the whole table
  covid_data <- html_page %>%
    html_nodes(table_name) %>%
    html_table %>% 
    data.frame()
  
  # Separate data of countries from other data
  countries_covid <- covid_data[9:238, ]
  world_covid <- covid_data[c(1:8, 239:246), ]
  
  # Load geographical coordinates
  world_map <- map_data("world")
  world_map <- fortify(world_map)
  
  # Check which region names do not match
  a <- unique(world_map$region) %>% 
    data.frame()
  
  b <- unique(countries_covid$Country.Other) %>% 
    data.frame()
  
  # Find country names that don't match
  no_match1 <- setdiff(a, b) %>% 
    as.list
  no_match2 <- setdiff(a, b) %>% 
    as.list
  
  # Find covid data country name equivalents in world data
  wrong_titles <- c("Antigua and Barbuda", "Cabo Verde", "Curaçao", "Czechia",
                    "Congo", "Faeroe Islands", "St. Vincent Grenadines", 
                    "Saint Kitts and Nevis", "St. Barth",
                    "Saint Pierre Miquelon", "S. Korea", "Turks and Caicos",
                    "UAE", "Vatican City", "British Virgin Islands")
  
  new_titles <- c("Antigua", "Cape Verde", "Curacao", "Czech Republic",
                  "Republic of Congo", "Faroe Islands", 
                  "Saint Vincent", "Saint Kitts", "Saint Barthelemy", 
                  "Saint Pierre and Miquelon", "South Korea", 
                  "Turks and Caicos Islands", "United Arab Emirates", "Vatican",
                  "Virgin Islands")
  
  # Rename countries in the covid data to match the world data
  for (i in 1:length(wrong_titles)) {
    countries_covid$Country.Other[
      countries_covid$Country.Other == wrong_titles[i]] <- new_titles[i]
  }
  
  # Delete rows that will not have any covid data
  countries_covid <- subset(countries_covid, 
                     Country.Other != "DPRK" & Country.Other != "Hong Kong" &
                     Country.Other != "Réunion" & 
                     Country.Other != "Channel Islands" &
                     Country.Other != "DRC" & Country.Other != "CAR" &
                     Country.Other != "Eswatini" & Country.Other != "Nevis" &
                     Country.Other != "Gibraltar" & Country.Other != "Tuvalu" &
                     Country.Other != "Caribbean Netherlands" & 
                     Country.Other != "Macao" &
                     Country.Other != "MS Zaandam" & 
                     Country.Other != "Diamond Princess" &
                     Country.Other != "Barbuda" & 
                     Country.Other != "Trinidad and Tobago")
  
  # Delete rows that will not have any covid data
  world_map <- subset(world_map, 
                       region != "American Samoa" & region != "Antarctica" &
                       region != "Ascension Island" & region != "Azores" &
                       region != "Bonaire" & region != "Canary Islands" &
                       region != "Central African Republic" & 
                       region != "Chagos Archipelago" &
                       region != "Christmas Island" & region != "Cocos Islands" &
                       region != "French Southern and Antarctic Lands" & 
                       region != "Guam" & region != "North Korea" &
                       region != "Guernsey" & region != "Heard Island" &
                       region != "Jersey" & region != "Kosovo" &
                       region != "Madeira Islands" & region != "Pitcairn Islands" &
                       region != "Puerto Rico" & region != "Democratic Republic of the Congo" &
                       region != "Reunion" & region != "Saba" &
                       region != "Grenadines" & region != "Siachen Glacier" &
                       region != "Sint Eustatius" & region != "South Georgia" &
                       region != "South Sandwich Islands" & region != "Swaziland" &
                       region != "Trinidad" & region != "Tobago" &
                       region != "Turkmenistan" & region != "Barbuda" &
                       region != "Nevis" & region != "Northern Mariana Islands" &
                       region != "Norfolk Island" & region != "Saint Kitts")
  
  # Select only the data that is necessary
  dat <- data.frame(region = countries_covid$Country.Other,
                              total_cases = countries_covid$TotalCases,
                              cases_per1m = countries_covid$Tot.Cases.1M.pop,
                              total_deaths = countries_covid$TotalDeaths,
                              deaths_per1m = countries_covid$Deaths.1M.pop,
                              total_recovered = countries_covid$TotalRecovered,
                              active_cases = countries_covid$ActiveCases,
                              active_per1m = countries_covid$Active.Cases.1M.pop,
                              serious_cases = countries_covid$Serious.Critical,
                              total_tests = countries_covid$TotalTests,
                              tests_per1m = countries_covid$Tests.1M.pop,
                              population = countries_covid$Population)
  
  # Convert from characters to numeric values
  dat$total_cases <- as.numeric(gsub(",", "", dat$total_cases))
  dat$cases_per1m <- as.numeric(gsub(",", "", dat$cases_per1m))
  dat$total_deaths <- as.numeric(gsub(",", "", dat$total_deaths))
  dat$deaths_per1m <- as.numeric(gsub(",", "", dat$deaths_per1m))
  dat$total_recovered <- as.numeric(gsub(",", "", dat$total_recovered))
  dat$active_cases <- as.numeric(gsub(",", "", dat$active_cases))
  dat$active_per1m <- as.numeric(gsub(",", "", dat$active_per1m))
  dat$serious_cases <- as.numeric(gsub(",", "", dat$serious_cases))
  dat$total_tests <- as.numeric(gsub(",", "", dat$total_tests))
  dat$tests_per1m <- as.numeric(gsub(",", "", dat$tests_per1m))
  dat$population <- as.numeric(gsub(",", "", dat$population))
  
  # Reformat data frame, delete any rows with NAs
  dat <- drop_na(melt(dat, id = "region", value.name = "numeric", 
                      variable.name = "choice"))

  # Create final data frame containing only necessary data
  return(dat)
}

# Function to create a world data frame
#   OUTPUT:
#     world_map - data frame containing covid data.
get_world_map <- function() {
  
  # Create empty data frame to later return
  world_map <- data.frame(NA)
  
  # Load geographical coordinates
  world_map <- map_data("world")
  world_map <- fortify(world_map)
  
  # Delete rows that will not have any covid data
  world_map <- subset(world_map, 
                       region != "American Samoa" & region != "Antarctica" &
                       region != "Ascension Island" & region != "Azores" &
                       region != "Bonaire" & region != "Canary Islands" &
                       region != "Central African Republic" & 
                       region != "Chagos Archipelago" &
                       region != "Christmas Island" & region != "Cocos Islands" &
                       region != "French Southern and Antarctic Lands" & 
                       region != "Guam" & region != "North Korea" &
                       region != "Guernsey" & region != "Heard Island" &
                       region != "Jersey" & region != "Kosovo" &
                       region != "Madeira Islands" & region != "Pitcairn Islands" &
                       region != "Puerto Rico" & region != "Democratic Republic of the Congo" &
                       region != "Reunion" & region != "Saba" &
                       region != "Grenadines" & region != "Siachen Glacier" &
                       region != "Sint Eustatius" & region != "South Georgia" &
                       region != "South Sandwich Islands" & region != "Swaziland" &
                       region != "Trinidad" & region != "Tobago" &
                       region != "Turkmenistan" & region != "Barbuda" &
                       region != "Nevis" & region != "Northern Mariana Islands" &
                       region != "Norfolk Island" & region != "Saint Kitts")

  # Return the data frame
  return(world_map)
}

lastRefresh <- Sys.time()

dat <- get_data(website = "https://www.worldometers.info/coronavirus/", 
         table_name = "#main_table_countries_today")

#### END OF EXTRACTING DATA

#### BUILD THE BASE MAP
# Function to create the base map
#   INPUTS:
#     data - data frame containing covid data,
#     choice - choice of data to display, given by the user,
#     world_map - data frame containing world map data,
map <- function(data = dat, world_map = get_world_map(), input_choice) {
  
  # Selected plot look, function
  created_theme <- function () { 
    theme_minimal() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(), 
          panel.border = element_blank(), 
          strip.background = element_rect(fill = "red", colour = "red"),
          legend.position = "top")
  }
  
  # User selects the column displayed
  # NA rows removed
  df_map <- data %>%
    filter(choice == input_choice) %>%
    drop_na()
  
  # Duplicate data to the map data frame
  world_map["choice"] <- rep(input_choice, nrow(world_map))
  world_map["numeric"] <- df_map$numeric[match(world_map$region, 
                                               df_map$region)]
  
  # Use ggplot to plot the map
  plot_map <- ggplot() + 
    geom_polygon_interactive(data = subset(world_map), color = 'darkgray', size = 0.2,
                             aes(x = long, y = lat, fill = numeric, group = group, 
                                 tooltip = sprintf("%s<br/>%s", region, numeric))) + 
    scale_fill_gradientn(colours = brewer.pal(4, "BuPu"), na.value = 'white') +
    created_theme()
  
  # Return the plot
  return(plot_map)
}
#### END OF BUILDING THE MAP


#### SERVER CODE
server <- function(input, output){
  
  ### Globe plot page
  
  # Plot the world map 
  output$distPlot <- renderGirafe({
    ggiraph(code = print(map(input_choice =  input$globeDataChoice)))
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
#### END OF THE SERVER CODE

