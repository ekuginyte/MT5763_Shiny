#### Server
server <- function(input, output) {
  
  ## GET DATAFRAMES ######################
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  # Keeps track of when to automatically refresh
  auto_refresh <- reactiveTimer(3600000) # 1hr
  
  RefreshDetect <- reactive({
    list(input$refresh)
  })
  
  observeEvent(
     RefreshDetect(), {
      auto_refresh()
      #TSData <- get.time.series.data(type = input$map_data_choice)
      # Extract dates from the time series data
       dateOptions <- names(TSData[["confirmed"]][-1])
       maxDate <- tail(dateOptions, n = 1)
      # Save today's date
      lastRefresh <- Sys.time()
      # Extract all possible dates
      showNotification("Data Refreshed")
      })
  
  ### Map plot page
  # Plot the world map 
  output$world_map <- renderGirafe({
    ggiraph(code = print(get.world.map(type = input$map_data_choice, 
                                       date = as.Date(input$plot_date, "%m/%d/%y"), Curr_TSDATA = TSData)))
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
    get.df2(date = as.Date(input$plot_date, "%m/%d/%y"), type = input$data_page_choice, 
            countries = input$data_countries, format = input$data_format,
            Curr_TSDATA = TSData)})
  
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
  
  # Plot specified map
  output$stat_plot <- renderGirafe({
    ggiraph(code = print(get.plot(plot_name = input$plot_type_choice, 
                                  regions = input$plot_countries, 
                                  type = input$plot_data_choice,
                                  date = as.Date(input$plot_date, "%m/%d/%y"),
                                  Curr_TSDATA = TSData)))
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

