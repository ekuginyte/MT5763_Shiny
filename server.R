### Server
server <- function(input, output) {

 observeEvent(
   
   # Refresh when clicked
    input$refresh, {
      
      # Initial dataset
      #map_data_choice <- "confirmed"
      
      # Extract all possible values for dates
      #TSData <- get.time.series.data(type = input$map_data_choice)
      # Extract dates from the time series data
      #dateOptions <- names(TSData[-1]) 
      #dateOptions <- as.Date(strftime(dateOptions, "%m/%d/%y"))
    
      # Refresh every hour
      #invalidateLater(3600000)
      # Save today's date
      lastRefresh <- as.Date(strftime(Sys.time(), "%Y/%m/%d")) - 2
      #lastRefresh <- format(lastRefresh, "%m/%d/%y") 
      # Extract all possible dates
      #dateOptions <- as.Date(dateOptions, "%m/%d/%y")
      showNotification("Data Refreshed")
      })
  
  ### Map plot page
  # Plot the world map 
  output$world_map <- renderGirafe({
    ts_df <- get.time.series.data(type = input$map_data_choice)
    ggiraph(code = print(get.world.map(type = input$map_data_choice, 
                                       date = input$plot_date, df = ts_df)))
    })
  
  # Save Globe plot
  output$download_map_plot <- downloadHandler(
      filename <- function() {
        paste("Covid-19_map_", input$map_data_choice, "_", Sys.Date(), ".png", sep = "")
        },
      content <- function(file) {
        ggsave(world_map, file)
        }
      )
  
  # Save mapping data
  output$download_map_data <- downloadHandler(
    filename = function() {
      paste("Covid-19_data_", Sys.Date(), ".csv", sep = "")
      },
    content = function(file) {
      write.csv(ts_df, file)
      }
    )
  
  
  ### Data page
  dfInput <- reactive({
    get.df2(date = input$plot_date, type = input$data_page_choice, 
            countries = input$data_countries, format = input$data_format)})
  
  output$data_to_download <- renderDataTable(dfInput())
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Covid-19_data_", Sys.Date(),'.csv', sep = '')
      },
    content = function(file) {
      write.csv(dfInput(), file)
      }
    )
  
  ### Plot page
  #plotdfInput <- reactive({
  #  get.df1(regions = input$plot_countries, type = input$plot_data_choice,
  #          date = input$plot_date, df = df)
  #})
    #get.df("l", dat, input$map_data_choice, input$plot_countries)})
  
  # Plot specified map
  output$stat_plot <- renderGirafe({
    ggiraph(code = print(get.plot(plot_name = input$plot_type_choice, 
                                  regions = input$plot_countries, 
                                  type = input$plot_data_choice,
                                  date = input$plot_date)))
  })
  
  # Save plot 
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("Covid-19_map_", input$plot_data_choice, "_", input$plot_type_choice, 
            Sys.Date(),".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plotInput())
    }
  )
  
  # Save plot dataset
  output$download_plot_data <- downloadHandler(
    filename = function() {
      paste("Covid-19_map_", input$plot_data_choice,"_", input$plot_type_choice, 
            Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(dfInput(), file)
    }
  )
  
}

