#### Functions for Server and UI files

################################################################################
############################ PACKAGE HANDLING ##################################
################################################################################
#packages <- c("rvest", "robotstxt", "xml2", "maps", "shiny", "tidyverse",  "ggmap", 
#              "ggiraph", "RColorBrewer", "reshape2", "shinyjs", "shinythemes", 
#              "shinydashboard", "sf", "rgdal", "shinyWidgets", "mgsub", "data.table", 
#              "lubridate", "ggpattern", "markdown", "evaluate")

# Install packages required
#installed_packages <- packages %in% rownames(installed.packages())

#if (any(installed_packages == FALSE)) {
#  install.packages(packages[!installed_packages])
#}

# Packages loading
#invisible(lapply(packages, library, character.only = TRUE))

# "ggplot2", "leaflet"
library(rvest)
library(robotstxt)
library(xml2)
library(maps)
library(shiny)
library(tidyverse)
library(ggmap)
library(ggiraph)
library(RColorBrewer)
library(reshape2)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
library(sf)
library(rgdal)
library(shinyWidgets)
library(mgsub)
library(data.table)
library(lubridate)
library(ggpattern)
library(markdown)
library(evaluate)

################################################################################
############################### FUNCTIONS ######################################
################################################################################

### get.time.series.data
# Function to scrape data from GitHub and wrangle
#   INPUT:
#     type - ["confirmed", "deaths", "recovered"],
#     minDate - eg. as.Date("m/d/y", format="%m/%d/%Y", 
#     maxDate - eg. as.Date("m/d/y", format="%m/%d/%Y".
#       * dates must be Date objects with format specified
#   OUTPUT: 
#     df - data.frame of covid time series data.
get.time.series.data <- function(type, 
                                 minDate = as.Date("1/22/20", format = "%m/%d/%y"), 
                                 maxDate = as.Date(strftime(Sys.time(), "%Y/%m/%d")) - 2, 
                                 countries = NA){
  
  # Dictionary to act as register for URLs to access data
  JHU_data_dict <-
    c(
      "confirmed" = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
      "deaths" = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
      "recovered" = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
    )
  
  # Get list of dates desired
  dates <- format(seq(minDate, maxDate, by = "day"), format = "%m/%d/%y")

  # Returns a parsed df of required timeframe and by country, dropping redundant information
  # Fetch the data
  df <- fread(JHU_data_dict[type], drop = c("Province/State", "Lat", "Long")) %>%
    # Rename the column for ease later
    rename("Region" = "Country/Region") %>%
    # Group by region (normally country)
    group_by(Region)
 
   df <- df %>% 
    # Rename all dates to be in a nicer format
    rename_at(vars(names(df[-1])), ~as.character(format(
      as.Date(names(df[-1]), format = "%m/%d/%y"), format = "%m/%d/%y"))) %>%
    # Select all the dates found above
    select(all_of(dates)) %>%
    # Combine any rows which have the same region
    summarise_all(funs(sum(na.omit(.))))
  
  # Find covid data country name equivalents in world data
  wrong_titles <- c("Antigua and Barbuda", "Czechia", "Korea, North", "Korea, South", 
                    "Saint Kitts and Nevis", "Saint Vincent and the Grenadines", 
                    "Taiwan*", "Trinidad and Tobago", "United Kingdom", "US")
  
  new_titles <- c("Antigua", "Czech Republic", "North Korea", 
                  "South Korea", "Saint Kitts", "Grenadines", "Taiwan", 
                  "Trinidad", "UK", "USA")
  
  # Rename countries in the covid data to match the world data
  for (i in 1:length(wrong_titles)) {
    df$Region[df$Region == wrong_titles[i]] <- new_titles[i]
  }

  # Return parsed data
  return(df)
}

################################################################################

### get.population
# Function to extract world population data from worldometers.info
#  OUTPUT:
#     pd - data frame with world population column.
get.population <- function() {
  
  # Download the whole web page of population
  html_page <- read_html("https://www.worldometers.info/world-population/population-by-country/") 
  
  # Access the whole table
  pd <- html_page %>%
    html_nodes("#example2") %>%
    html_table %>% 
    data.frame()
  
  # Find covid data country name equivalents in world data
  wrong_titles <- c("Antigua and Barbuda", "Czech Republic (Czechia)", 
                    "United Kingdom", "Saint Kitts & Nevis", "Sao Tome & Principe",
                    "Trinidad and Tobago", "United States", "St. Vincent & Grenadines")
  
  new_titles <- c("Antigua", "Czech Republic", "UK", "Saint Kitts",
                  "Sao Tome and Principe", "Trinidad", "USA",
                  "Grenadines")
  
  # Rename countries in the covid data to match the world data
  for (i in 1:length(wrong_titles)) {
    pd$Country..or.dependency.[pd$Country..or.dependency. == 
                                 wrong_titles[i]] <- new_titles[i]
  }
  # Return data frame with populations
  return(pd)
}

################################################################################

### get.world.map
# GitHub data map plotting function
#  INPUT:
#    user_input - selection of type of plot;
#    date - selection of date.
#    df - overall df which will be queried
#  OUTPUT:
#    plot_map - map plot of selected data.
get.world.map <-  function(type, date, Curr_TSDATA, pd = today_pop) {
  
  # Filter the timeseries set to be the type selected
  df <- Curr_TSDATA[[type]]
  # Get total cases from selected data type
  df <- df %>%
    select(c("Region", format(date, format = "%m/%d/%y")))
  
  # Get world map geometry
  world_map <- map_data("world") %>%
    fortify()
  
  # Rename column name to match with covid data frame
  names(world_map)[names(world_map) == "region"] <- "Region"
  
  # Add the cases to the world map data frame
  world_map["cases"] <- df[match(world_map$Region, df$Region), 
                           format(date, format = "%m/%d/%y")]
  
  # Add population to the data frame
  #pd <- get.population()
  pd$Population..2020. <- as.numeric(gsub(",", "", pd$Population..2020.))
  # Combine
  world_map["population"] <- pd$Population..2020.[
    match(world_map$Region, pd$Country..or.dependency.)] 
  world_map$population <- as.numeric(gsub(",", "", world_map$population))
  
  # Cases relative to population
  world_map$cases <- round(world_map$cases / (world_map$population / 1000000), 2)
  
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
            # Adjust legend size
            legend.title = element_text(size = 6),
            legend.text = element_text(size = 6),
            #legend.key.height = unit(1, 'cm'),
            #legend.key.width = unit(5, 'cm'),
            legend.position = "top",
            #legend.title.align = 0,
            legend.direction = "horizontal")
  }
  
  # Use ggplot to plot the map
  plot_map <- ggplot() + 
    geom_polygon_interactive(data = subset(world_map), color = 'white', size = 0.1,
                             aes(x = long, y = lat, fill = cases, group = group, 
                                 tooltip = sprintf("%s<br/>%s", Region, cases))) + 
    labs(fill = "Cases") +
    scale_fill_gradientn(colours = brewer.pal(7, "Oranges"), na.value = "grey95") +
    created_theme()
  
  # Return the plot
  return(plot_map)
}

################################################################################

### get.world.stats
# GitHub data stats function
#  INPUT:
#    user_input - selection of type of plot;
#    date - selection of date.
#  OUTPUT:
#    df_stats - map plot of selected data.
get.world.stats <-  function(date, df_stats = data.table(), pd = today_pop) {

    # Get total cases from selected data type
  df_stats <- df_stats %>%
   select(c("Region", format(as.Date(date), format = "%m/%d/%y")))

  # Add population to the data frame
  # pd <- get.population()
  pd$Population..2020. <- as.numeric(gsub(",", "", pd$Population..2020.))
  
  # Combine the population data with the covid cases data
  df_stats$population <- pd$Population..2020.[match(df_stats$Region, pd$Country..or.dependency.)] 
  df_stats$population <- as.numeric(gsub(",", "", df_stats$population))
  
  # Cases relative to population
  df_stats[[2]] <- round(df_stats[[2]] / (df_stats$population / 1000000), 2)
  
  # Return the plot
  return(df_stats)
}

################################################################################

### get.plot - Plots function for plot page
#  INPUTS:
#   plot_name - type of plot to produce,
#   regions - user's selected regions,
#   type - user's selected bar or pie chart,
#   date - user's selected max date.
#  OUTPUTS:
#   p - either bar or pie chart plot.
get.plot <- function(plot_name, regions, type, date, Curr_TSDATA) {
  
  # Make sure the input is in date format
  date <- as.Date(date, "%m/%d/%y")
  
  # Extract the selected data
  df <- get.world.stats(date = date, df_stats = Curr_TSDATA[[type]])
  df <- df[df$Region %in% regions, ]
  
  # Extract variables for ggplot
  x_var <- as.list(df$Region)
  x_var <- as.character(x_var)
  y_var <- df[, 2]
  y_var <- as.vector(unlist(y_var))
  
  # Bar plot
  if (plot_name == "vbar") {
    p <- ggplot() +
      geom_col_pattern(aes(x = x_var, y = y_var, fill = x_var, pattern = x_var), 
                       alpha = 0.8,
                       pattern_fill = "white",
                       pattern_colour = "white",
                       pattern_alpha = 1,
                       pattern_angle = 45,
                       pattern_density = 0.02,
                       pattern_spacing = 0.03,
                       pattern_key_scale_factor = 1) + 
      labs(y = "Cases relative to population per million", 
           x = "Regions") +
      theme_minimal() +
      scale_fill_brewer(palette = "YlOrRd") +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.75), 
            legend.position = "none")
    
  # Pie chart  
  } else if (plot_name == "pie") {
    # Plot pie 
    p <- ggplot() +
      geom_bar(aes(x = "", y = y_var, fill = x_var), alpha = 0.8, 
                stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(fill = "Countries") +
      #geom_text(aes(x = 1.7, y = y_var, label = df$Percentages), 
      #          colour = "grey25",
      #          fontface = "bold") +
      scale_fill_brewer(palette = "YlOrRd")
    
  # Else plot empty  
  } else{
    p <- ggplot()
  }
  
  # Return the selected plot
  return(p)
}

################################################################################

### get.df2 - Function to return the selected data frame, Data Page
#   INPUTS:
#     date - user selects date on page 1,
#     type - user selects type of data,
#     df - original data frame that has been scraped,
#     countries - user selects countries to look at,
#     format - user selects format of data (long or wide).
#   OUTPUTS:
#     df_0 - returns data frame of selected countries in long format,
#     df_1 - returns data frame of selected countries in wide format.
get.df2 <- function(date, data_type, countries, format, Curr_TSDATA) {
  
  # Preset an empty data frame
  df_0 <- data.frame()
  
  # Make sure it's a date input
  date <- as.Date(date, "%m/%d/%y")
  
  # For however many types selected
  if (length(data_type) != 0 && length(countries) != 0) {
    data_type <- strsplit(data_type,"[[:space:]]")
    dfs <- vector("list", length = length(data_type))
    for (i in data_type) {
      # Extract data with user's choices
      df_00 <- get.world.stats(date = date, df_stats = Curr_TSDATA[[i]])
      
      # Extract user's selected regions
      df_00 <- df_00[df_00$Region %in% countries, ]
      # Add what type of data this is
      
      df_00 <- df_00 %>%
        mutate(Type = i) %>%
        select(-population) %>%
        drop_na()
      
      # Add df to list to bind later
      dfs[[i]] <- df_00
      
    }
  }  else if (length(data_type) != 0 && length(countries) == 0) {
    data_type <- strsplit(data_type,"[[:space:]]")
    dfs <- vector("list", length = length(data_type))
    for (i in data_type) {
      # Extract data with user's choices
      df_00 <- get.world.stats(date = date, df_stats = Curr_TSDATA[[i]])
      
      df_00 <- df_00 %>%
        mutate(Type = i) %>%
        select(-population) %>%
        drop_na()
      # Add df to list to bind later
      dfs[[i]] <- df_00
    }
    } else{
      # No data types chosen, display nothing, even if countries chosen
      return(df_0)
    }
    # Bind dataframes
    df_0 <- do.call(rbind, dfs)
  
    # Wide or long data (default: long)
    # Pivot wide
    if (format == "w"){
      df_0 <- df_0 %>%
        pivot_wider(names_from = Type, values_from = format(as.Date(date, format = "%m/%d/%y"), "%m/%d/%y")) %>%
        mutate(Date = date)
    }
    return(df_0)
  }
################################################################################

### scrape.all.data - scrapes all the timeseries data available and returns the
# multiple dataframes in a list
# OUTPUTS:
#   TSDATA - a list of dataframes containing timeseries data

scrape.all.data <- function(){
  # Possible types of data to chose
  types = c("confirmed", "deaths", "recovered")
  
  # List to store dataframes in
  TSData <- vector('list', 3)
  
  # Loop through all types
  for(type in types){
    TSData[[type]] = get.time.series.data(type = type)
  }
  # Return list of dataframes
  return(TSData)
}

################################################################################
############################### Initials #######################################
################################################################################

# Initial dataset
TSData <- scrape.all.data()

# Extract dates from the time series data
dateOptions <- names(TSData[["deaths"]][-1])
maxDate <- tail(dateOptions, n = 1)

# On boot record time as lastRefresh
lastRefresh <- paste("Last updated: ", Sys.time())


################################################################################
############################### Constants ######################################
################################################################################

# Regions available
all_regions <- TSData[["deaths"]]$Region

# Todays population - assumed to be constant once program started
today_pop <- get.population()

################################################################################