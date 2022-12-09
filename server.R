#### Server
server <- function(input, output, session) {
  
  ##############################################################################
  ############################ Refresh Items ###################################
  ##############################################################################
  
  # tracks when to refresh. Either time limit of 1hr or button
  RefreshDetect <- reactive({
    list(input$refresh, invalidateLater(3600000))
  })
  
  # Update data and settings when RefreshDetect
  observeEvent(
     RefreshDetect(), {
      # Scrape a new set of data
      TSData <- scrape.all.data()
      # Extract dates from the time series data
      dateOptions <- names(TSData[["confirmed"]][-1])
      # Find the max possible date to chose
      maxDate <- tail(dateOptions, n = 1)
      # See if any additional regions to include in the queries/plots
      all_regions <- TSData[["deaths"]]$Region
      # Save current time as last refresh time
      output$last_refresh <- renderText({paste("Last updated: ", Sys.time())})
      # Update the slider with new available dates
      updateSliderTextInput(session,
                          inputId = "plot_date", 
                            choices = dateOptions,
                          label = paste("Date (most recent: ", maxDate,"): "))
      # Display a refresh update
      showNotification("Data Refreshed")
      }, ignoreInit = T)
  
  # Initial refresh date taken to be boot time
  output$last_refresh <- isolate(renderText({lastRefresh}))
  
  ##############################################################################
  ############################# Map plot page ##################################
  ##############################################################################
  
  # Plot the world map 
  output$world_map <- renderGirafe({
    ggiraph(code = print(get.world.map(type = input$map_data_choice, 
                                       date = as.Date(input$plot_date, "%m/%d/%y"), Curr_TSDATA = TSData,
                                       pd = today_pop)))
    })
  
  # Save Globe plot
  output$download_map_plot <- downloadHandler(
      filename <- function() {
        paste("Covid-19_map_", input$map_data_choice, "_", Sys.Date(), "_", input$map_data_choice,".png", sep = "")
        },
      content <- function(file) {
        ggsave(file)
        }
      )
  
  # Save mapping data
  output$download_map_data <- downloadHandler(
    filename = function() {
      paste("Covid-19_data_", Sys.Date(), "_", input$map_data_choice,".csv", sep = "")
      },
    content = function(file) {
      write.csv(TSData[[input$map_data_choice]][], file)
      }
    )
  
  ##############################################################################
  ############################## Data page #####################################
  ##############################################################################
  
  # Dataframe to display as a reactive element
  dfInput <- reactive({
    get.df2(date = as.Date(input$plot_date, "%m/%d/%y"), 
            data_type = input$data_page_choice, 
            countries = input$data_countries, 
            format = input$data_format,
            Curr_TSDATA = TSData)})
  
  # Render queried data
  output$data_to_download <- renderDataTable(dfInput())
  
  # Download the queried data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Covid-19_data_", Sys.Date(),'.csv', sep = '')
      },
    content = function(file) {
      write.csv(dfInput(), file)
      }
    )
  
  ##############################################################################
  ################################# Plot page ##################################
  ##############################################################################
  
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
      ggsave(file)
    }
  )
  
  # Save plot dataset
  output$download_plot_data <- downloadHandler(
    filename = function() {
      paste("Covid-19_map_", input$plot_data_choice,"_", input$plot_type_choice, 
            Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(get.df2(date = as.Date(input$plot_date, "%m/%d/%y"),
                        data_type = input$plot_data_choice,
                        countries = input$plot_countries,
                        format = "l",
                        Curr_TSDATA = TSData), file)
    }
  )
  
}
##############################################################################
