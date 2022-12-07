### Functions for Server and UI files

# Function to scrape data from GitHub and wrangle
#   INPUT:
#     type - ["confirmed", "deaths", "recovered"],
#     minDate - eg. as.Date("m/d/y", format="%m/%d/%Y", 
#     maxDate - eg. as.Date("m/d/y", format="%m/%d/%Y".
#       * dates must be Date objects with format specified
#   OUTPUT: 
#     df - data.frame of covid time series data.
get.time.series.data <- function(type = map_data_choice, 
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
    rename_at(vars(names(df[-1])), ~as.character(format(as.Date(names(df[-1]), format = "%m/%d/%y"), format = "%m/%d/%y"))) %>%
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



# Function to extract world population data
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
  return(pd)
}



# GitHub data map plotting function
#  INPUT:
#    user_input - selection of type of plot;
#    date - selection of date.
#    df - overall df which will be queried
#  OUTPUT:
#    plot_map - map plot of selected data.
get.world.map <-  function(type = map_data_choice, 
                             date =  as.Date(strftime(Sys.time(), "%Y/%m/%d")) - 2,
                             df) {
  
  # Get total cases from selected data type
  df <- df %>%
    select(c("Region", format(date, format = "%m/%d/%y")))
  world_map <- map_data("world") %>%
    fortify()
  
  # Rename column name to match with covid data frame
  names(world_map)[names(world_map) == "region"] <- "Region"
  
  # Add the cases to the world map data frame
  world_map["cases"] <- df[match(world_map$Region, df$Region), format(date, format = "%m/%d/%y")]
  
  # Add population to the data frame
  pd <- get.population()
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
            legend.title = element_text(size = 7),
            legend.text = element_text(size = 7),
            legend.key.height = unit(1, 'cm'),
            legend.key.width = unit(0.25, 'cm'),
            #legend.title.align = 0,
            legend.box = "vertical")
      )
  }
  
  # Use ggplot to plot the map
  plot_map <- ggplot() + 
    geom_polygon_interactive(data = subset(world_map), color = 'white', size = 0.1,
                             aes(x = long, y = lat, fill = cases, group = group, 
                                 tooltip = sprintf("%s<br/>%s", Region, cases))) + 
    labs(fill = "Cases relative to population, per million") +
    scale_fill_gradientn(colours = brewer.pal(7, "Oranges"), na.value = "grey95") +
    created_theme()
  
  # Return the plot
  return(plot_map)
}


# GitHub data stats function
#  INPUT:
#    user_input - selection of type of plot;
#    date - selection of date.
#    df - overall df which will be queried
#  OUTPUT:
#    df_stats - map plot of selected data.
get.world.stats <-  function(type = map_data_choice, 
                           date =  as.Date(strftime(Sys.time(), "%Y/%m/%d")) - 2,
                           df) {
  
  # Get total cases from selected data type
  df <- df %>%
    select(c("Region", format(date, format = "%m/%d/%y")))
  df_stats <- map_data("world") %>%
    fortify()
  
  # Rename column name to match with covid data frame
  names(df_stats)[names(df_stats) == "region"] <- "Region"
  
  # Add the cases to the world map data frame
  df_stats["cases"] <- df[match(df_stats$Region, df$Region), format(date, format = "%m/%d/%y")]
  
  # Add population to the data frame
  pd <- get.population()
  pd$Population..2020. <- as.numeric(gsub(",", "", pd$Population..2020.))
  # Combine
  df_stats["population"] <- pd$Population..2020.[
    match(df_stats$Region, pd$Country..or.dependency.)] 
  df_stats$population <- as.numeric(gsub(",", "", df_stats$population))
  
  # Cases relative to population
  df_stats$cases <- round(df_stats$cases / (df_stats$population / 1000000), 2)
  
  # Return the plot
  return(df_stats)
}



# Plots function for plot page
get.plot <- function(plotName = "vbar", df_stats) {
  # Plot a bar chart
  if (plotName == "vbar"){
    p <- ggplot(data = df_stats, aes(x = Region, y = cases)) +
      geom_col() + #ylab(plot_labels[unique(df$choice)]) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
  # Plot a pie chart  
  } else if (plotName == "pie"){
    p <- ggplot(data = df_stats, aes(x = "", y = cases, fill = Region)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void()
  } else{
    p <- ggplot()
  }
  return(p)
}



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





