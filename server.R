# Server file for the Shiny app
# Load libraries
library(shiny)
library(tidyverse)
library(robotstxt)
library(rvest)
library(maps)

# Check if scraping is allowed
paths_allowed("https://www.worldometers.info/coronavirus/")

# Web page containing covid data from 28/11/2022
URL <- "https://www.worldometers.info/coronavirus/"

# Download the whole web page
html_page <- read_html(URL) 

# Access the whole table
covid_data <- html_page %>%
  html_nodes("#main_table_countries_today") %>%
  html_table %>% 
  data.frame()

# Separate data of countries from other data
countries_covid <- covid_data[9:238, ]
world_covid <- covid_data[c(1:8, 239:246), ]

# Load geographical coordinates
world_data <- map_data("world")
world_data <- fortify(world_data)

# Check which region names do not match
a <- unique(world_data$region) %>% 
  data.frame()


b <- unique(countries_covid$Country.Other)

intersect(a, b)
setdiff(a, b)

####################################################################### EXAMPLE
# Server code
server <- function(input, output) {
  
  # Define histogram output
  output$histPlot <- renderPlot({
    
    # Generate 200 samples from normal dist.
    x <- rnorm(n = 200, 
               mean = input$mean,
               sd = sqrt(input$var))
    
    # Plot histogram
    hist(x, breaks = input$bin)
  })
  
  # Define summary statistics output
  output$summary <- renderPrint({
    
    # Generate 200 samples from normal dist.
    x <- rnorm(n = 200, 
               mean = input$mean,
               sd = sqrt(input$var))
    
    # Print summary
    summary(x)
  })
}
####################################################################### EXAMPLE

