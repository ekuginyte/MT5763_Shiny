### Updates for plots

plot_type_choice <- "vbar"
plot_type_choice <- "pie"

plot_countries <- c("UK", "Algeria", "Canada", "South Africa", "Estona", "Sweden", "France")
plot_data_choice <- "confirmed"
plot_date <- format(as.Date(strftime(Sys.time(), "%Y/%m/%d"), ) - 3, "%m/%d/%y")
TSData <- scrape.all.data()

get.plot <- function(plot_name, regions, type, date, Curr_TSDATA) {
  
  # Make sure the input is in date format
  #date <- format(mdy(date), "%m/%d/%y")
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
      #ylab(plot_labels[unique(df$choice)]) +
      labs(y = "Cases relative to population per million", 
           x = "Regions") +
      theme_minimal() +
      scale_fill_brewer(palette = "YlOrRd") +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.75), 
            legend.position = "none")
    
    # Pie chart  
  } else if (plot_name == "pie") {
    
    # Preset variables
    a <- rep(NA, length(y_var))
    b <- NA
    
    # Find percentages
    for (i in 1:nrow(df)) {
      # Sum of all the respective cases
      b[[i]] <- as.vector(unlist(df[i, 2] / df[i, 3] / 1000000))
    }
    # Create percentages for the pie plot
    for (i in 1:nrow(df)) {
      # Percentages
      a[[i]]<- (df[i, 2] / df[i, 3] / 1000000) / sum(b)
      a <- as.vector(unlist(a)) %>% round(1)
    }
    
    # Add the percentage to the data frame
    p <- ggplot() +
      geom_bar(aes(x = "", y = y_var, fill = x_var), 
               alpha = 0.85, stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      theme_void() +
      labs(fill = "Countries") +
      geom_text(aes(x = 1.7, y = y_var, label = scales::percent(a, accuracy = .1)), 
                position = position_stack(vjust = .5), 
                colour = "grey25",
                fontface = "bold") +
      scale_fill_brewer(palette = "YlOrRd")
    
    # Else plot empty  
  } else{
    p <- ggplot()
  }
  
  # Return the selected plot
  return(p)
}



get.plot(plot_name = plot_type_choice,
         regions = plot_countries, 
         type = plot_data_choice,
         date = as.Date(plot_date, "%m/%d/%y"),
         Curr_TSDATA = TSData)
