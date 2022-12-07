### Functions for Server and UI files

### Dictionaries

plot_labels <-
  c(
    "total_cases" = "Total Cases", 
    "cases_per1m" = "Cases per mln",
    "total_deaths" = "Total Deaths",
    "deaths_per1m" = "Deaths per mln",
    "total_recovered" = "Total Recovered", 
    "active_cases" = "Active Cases", 
    "active_per1m" = "Active per mln", 
    "serious_cases" = "Number of Serious Cases",
    "total_tests" = "Total Tests", 
    "tests_per1m" = "Tests per mln", 
    "population" = "Total Poplation"
  )

### Functions

# Function to create a covid data frame
#   INPUT:
#     website - site where data will be scraped from,
#     table_name - name of table from the site.
#   OUTPUT:
#     dat - data frame containing covid data.
get.data <- function(website, table_name) {
  # Check if the inputs are correct
  if (is.na(website) || is.numeric(website)) {
    stop("Invalid website url!")
  }
  if (is.na(table_name) || is.numeric(table_name)) {
    stop("Invalid table link!")
  }
  # Check if scraping is allowed
  if (robotstxt::paths_allowed(website) == FALSE) {
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
  dat <- drop_na(reshape2::melt(dat, id = "region", value.name = "numeric", 
                                variable.name = "choice"))
  
  # Add the continent component
  dat["continent"] <- countries_covid$Continent[match(dat$region, 
                                               countries_covid$Country.Other)]
  
  # Create final data frame containing only necessary data
  return(dat)
}


# Function to create a world data frame
#   OUTPUT:
#     world_map - data frame containing covid data.
get.world.map <- function() {
  
  # Create empty data frame to later return
  world_map <- data.frame(NA)
  
  # Load geographical coordinates
  world_map <- map_data("world")
  world_map <- fortify(world_map)
  
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

######### REDUNDANT
# Function to create the base map
#   INPUTS:
#     data - data frame containing covid data,
#     choice - choice of data to display, given by the user,
#     world_map - data frame containing world map data,

#!! data = dat assumes dat is initialised always

map <- function(data = NA, world_map = get.world.map(), input_choice) {
  
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
    geom_polygon_interactive(data = subset(world_map), color = 'red', size = 0.2,
                             aes(x = long, y = lat, fill = numeric, group = group, 
                                 tooltip = sprintf("%s<br/>%s", region, numeric))) + 
    scale_fill_gradientn(colours = brewer.pal(7, "Oranges"), na.value = 'white') +
    created_theme()
  
  # Return the plot
  return(plot_map)
}
######### REDUNDANT

# Dynamic dataset formatter for data page
get.df <- function(format, df_0, choices, countries){
  if (!is.null(countries)) {
    if (!is.null(choices)) {
      df <- filter(df_0, choice %in% choices) %>%
        filter(region %in% countries)
    } else{
      df <- filter(df_0, region %in% countries)
    }
  } else{
    if (!is.null(choices)) {
      df <- filter(df_0, choice %in% choices)
    } else{
      df <- df_0
    }
  }
  
  if (is_null(df) == FALSE) {
    if (format == "w"){
      df <- pivot_wider(df, names_from = "choice", values_from = "numeric")
    }
    return(df)
  } else {
    return(df_0)
  }
}

# Plots function for plot page
get.plot <- function(plotName = "vbar", df) {
  if (plotName == "vbar"){
    p <- ggplot(data = df, aes(x = region, y = numeric)) +
      geom_col() + ylab(plot_labels[unique(df$choice)]) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  } else if (plotName == "pie"){
    p <- ggplot(data = df, aes(x = "", y = numeric, fill = region)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start=0) +
      theme_void()
  } else{
    p <- ggplot()
  }
  return(p)
}

get.time.series.data <- function(type = "confirmed", 
                                 minDate = as.Date("1/22/20", format = "%m/%d/%y"), 
                                 maxDate = as.Date(strftime(Sys.time(), "%Y/%m/%d")) - 2, 
                                 countries = NA){
  # Function - Scrape data from GitHub and wrangle
  # Input - type : ["confirmed", "deaths", "recovered"],
  #         minDate : eg. as.Date("m/d/y", format="%m/%d/%Y", 
  #         maxDate : eg. as.Date("m/d/y", format="%m/%d/%Y"
  # * dates must be Date objects with format specified
  # Output - data.frame time series
  
  # EXAMPLE LINE:
  # get.time.series.data("deaths", as.Date("21/02/2022", format = "%d/%m/%Y"), as.Date("25/02/2022", format = "%d/%m/%Y"))
  
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
    rename_at(vars(names(df[-1])), ~as.character(format(as.Date(names(df[-1]), format = "%m/%d/%y"), format = "%m/%d/%y"))) %>%
    # Select all the dates found above
    select(all_of(dates)) %>%
    # Combine any rows which have the same region
    summarise_all(funs(sum(na.omit(.))))
  
  # ONCE COUNTRIES HAVE BEEN ASSIGNED can use lines of code similar to that seen in 
  # get.df() with countries to find 
  
  # Find unique regions, used to help find the association
  unique_regions <- unique(df$Region)
  
  # Delete rows with no mapping data
  df <- subset(df, 
               Region != "Congo (Brazzaville)" & Region != "Congo (Kinshasa)" & 
               Region != "Cote d'Ivoire" & Region != "Diamond Princess" &
               Region != "Cabo Verde" & Region != "Burma" &
               Region != "Eswatini" & Region != "Holy See" & 
               Region != "MS Zaandam" & Region != "Summer Olympics 2020" &
               Region != "Tuvalu" & Region != "West Bank and Gaza" & 
               Region != "Winter Olympics 2022")
  
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

# Function to create a world data frame for plot 2
#   INPUT:
#     df - data frame containing time series data
#   OUTPUT:
#     world_map_2 - data frame containing covid data.
get.world.map.2 <- function(df = NA) {
  
  # Create empty data frame to later return
  world_map_2 <- data.frame(NA)
  
  # Load geographical coordinates
  world_map_2 <- map_data("world")
  world_map_2 <- fortify(world_map_2)
  
  # Check which regions don't have any covid data
  dif <-  setdiff(unique(world_map_2$region), df$Region)
  # Delete
  world_map_2 <- world_map_2[!world_map_2$region %in% dif, ]
  
  # Delete regions with no covid data
  world_map_2 <- subset(world_map_2, 
                        region != "Burma" & region != "Cabo Verde" &
                          region != "Nevis" & region != "Tobago")
  
  # Rename column name to match with covid data frame
  names(world_map_2)[names(world_map_2) == "region"] <- "Region"
  
  # Return the data frame
  return(world_map_2)
}

# GitHub data map plotting functions

# Total confirmed cases
#  INPUT:
#    user_input - selection of type of plot;
#    maxDate - selection of date.
#  OUTPUT:
#    plot_map_1 - map plot of selected data.
confirmed_total_map <-  function(user_input = "confirmed", 
                                 date =  as.Date(strftime(Sys.time(), "%Y/%m/%d")) - 2,
                                 df) {
  
  #df <- get.time.series.data(type = user_input, maxDate = maxDate)
  #df$Cases <- rowSums(df[2:ncol(df)])# DOES NOT WORK WITH DATASET, data is active cases for confirmed
  df <- df %>%
    select(c("Region", format(date, format = "%m/%d/%y")))
  
  world_map_2 <- get.world.map.2(df)
  
  # Add the cases to the world map data frame
  world_map_2["cases"] <- df[match(world_map_2$Region, df$Region),format(date, format = "%m/%d/%y")]
  
  # Population data
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
  
  # Add population to the data frame
  pd$Population..2020. <- as.numeric(gsub(",", "", pd$Population..2020.))
  world_map_2["population"] <- pd$Population..2020.[
    match(world_map_2$Region, pd$Country..or.dependency.)] 
  world_map_2$population <- as.numeric(gsub(",", "", world_map_2$population))
  
  # Cases relative to population
  world_map_2$cases <- world_map_2$cases / world_map_2$population * 100
  
  ############################## LEAFLET ########################################

  # Plotting parameters for the world map
  #bins <- c(0, 10, 50, 100, 500, 1000, Inf)
  #map_palette <- colorBin("Blues", domain = world_map_2$cases, bins = bins)
  
  # Create base map 
  #plot_map_1 <- leaflet(world_map_2) %>% 
    #addTiles() %>% 
    #addLayersControl(
      #position = "topright",
      #overlayGroups = c("Covid-19 Confirmed", "Covid-19 Deaths", "Covid-19 Recovered"),
      #options = layersControlOptions(collapsed = FALSE)) %>% 
    #hideGroup(c("Covid-19 Deaths", "Covid-19 Recovered")) %>%
    #addProviderTiles(providers$CartoDB.Positron) %>%
    #fitBounds(~-100, -60, ~60, 70) %>%
    #addLegend("bottomright", pal = map_palette, values = ~world_map_2$cases,
    #          title = "<small>Cases</small>") 
  # Return the plot
  #return(plot_map_1)
#}
  
  ############################# GGPLOT #########################################
  
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
  
  # Use ggplot to plot the map
  plot_map <- ggplot() + 
    geom_polygon_interactive(data = subset(world_map_2), color = 'red', size = 0,
                             aes(x = long, y = lat, fill = cases, group = group, 
                                 tooltip = sprintf("%s<br/>%s", Region, cases))) + 
    scale_fill_gradientn(colours = brewer.pal(7, "Oranges"), na.value = "white") +
    created_theme()
  
  # Return the plot
  return(plot_map)
}

# Regions that need matching can be found by using 
#df$Region[is.na(match(unique(df$Region), unique(world_map$region)))]

# NEW MAP FUNCTION FOR JHU DATA
# GitHub data map plotting functions
# Total confirmed cases
#  INPUT:
#    user_input - selection of type of plot;
#    date - selection of date.
#    df - overall df which will be queried
#  OUTPUT:
#    plot_map_1 - map plot of selected data.
get.world.map.2b <-  function(user_input = "confirmed", 
                             date =  as.Date(strftime(Sys.time(), "%Y/%m/%d")) - 2,
                             df) {
  df <- df %>%
    select(c("Region", format(date, format = "%m/%d/%y")))
  
  world_map_2 <- map_data("world") %>%
    fortify()
  
  # Check which regions don't have any covid data
  dif <-  setdiff(unique(world_map_2$region), df$Region)
  # Delete
  world_map_2 <- world_map_2[!world_map_2$region %in% dif, ]
  
  # Delete regions with no covid data
  world_map_2 <- subset(world_map_2, 
                        region != "Burma" & region != "Cabo Verde" &
                          region != "Nevis" & region != "Tobago")
  
  # Rename column name to match with covid data frame
  names(world_map_2)[names(world_map_2) == "region"] <- "Region"
  
  # Add the cases to the world map data frame
  world_map_2["cases"] <- df[match(world_map_2$Region, df$Region),format(date, format = "%m/%d/%y")]
  
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
  
  # Use ggplot to plot the map
  plot_map <- ggplot() + 
    geom_polygon_interactive(data = subset(world_map_2), color = 'red', size = 0,
                             aes(x = long, y = lat, fill = cases, group = group, 
                                 tooltip = sprintf("%s<br/>%s", Region, cases))) + 
    scale_fill_gradientn(colours = brewer.pal(7, "Oranges"), na.value = "white") +
    created_theme()
  
  # Return the plot
  return(plot_map)
}



