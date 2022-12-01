### Functions

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

get.plot <- function(plotName, data){
  if (plotName == "vbar"){
    p <- ggplot(data, aes(x = choice, y = numerical)) +
      geom_bar()
  } else{
    return (NULL)
  }
  return(p)
}