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
library(ggplot2)

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