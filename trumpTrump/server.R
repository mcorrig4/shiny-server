#######################
#####  SERVER.R  ######
#######################

library(shiny)
library(shinydashboard)
source("loader.R")


data.source <- "https://data.cityofchicago.org/api/views/ijzp-q8t2/rows.csv"
data.file <- "./data/rows.csv"
fake.connection = TRUE


shinyServer(function(input, output, session) {
  # ------------
  # LOAD EVERYTHING AND RETURN CONNECTION TO DATABASE TABLE
  #-------------
  if(!fake.connection) {  
    crimes_tbl <- load_data(data.file, fake.connection = fake.connection)
  } else {
    hide("loading_page", anim = FALSE)
    shinyjs::show("main_content", anim = TRUE, animType = "slide", time = 1.0)
  }
  
  # ------------
  # CLOSE SPARK CONNECTION ON APP EXIT
  #-------------
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    if (!fake.connection && !is.null(sc)) {
      spark_disconnect(sc)
    }
  })
  
  # ------------
  # HEADER BAR
  #-------------
  
 
  # ------------------------------
  # TAB 1 INTRO
  # ------------------------------
  

  # ------------------------------
  # TAB 2 INPUTS
  # ------------------------------
  output$tab2_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
  })
  
  tab2_map_center <- reactive({
    input$tab2_map_center
  })

  
  # ------------------------------
  # TAB 3 DESCRIPTIVE STATISTICS
  # ------------------------------
  # ------------
  # INPUT VALUES
  # ------------
  output$tab3_timeRange_start <- renderValueBox({
    valueBox(
      value = as.yearmon(as.Date(input$tab2_datesRange[1])),
      subtitle = "Begin Date Range",
      icon = icon("calendar")
    )
  })
  output$tab3_timeRange_end <- renderValueBox({
    valueBox(
      value = as.yearmon(as.Date(input$tab2_datesRange[2])),
      subtitle = "End Date Range",
      icon = icon("calendar")
    )
  })
  output$tab3_location_lat <- renderValueBox({
    b <- tab2_map_center()
    valueBox(
      value = round(b$lat, 6),
      subtitle = "Latitude",
      icon = icon("globe")
    )
  })
  output$tab3_location_lng <- renderValueBox({
    b <- tab2_map_center()
    valueBox(
      value = round(b$lng, 6),
      subtitle = "Longitude",
      icon = icon("globe")
    )
  })
  output$tab3_crimeTypes <- renderUI({
    s <- as.vector(input$tab2_crimeTypes)
    p(paste0(as.character(s), collapse = ", "))
  })
  # ------------
  # MAP
  # ------------
  output$tab3_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
  })
  # ------------
  # HISTOGRAM
  # ------------
  output$tab3_timeOfDay_hist <- renderPlotly({
    plot_ly(iris, x = ~Petal.Length, y = ~Petal.Width,color = ~Species, mode = "markers", type="scatter")
  })
  output$tab3_timeOfDay_mean <- renderInfoBox({
    infoBox(
      value = "mean##",
      title = "Mean",
      icon = icon("bell")#icon("bars")
    )
  })
  output$tab3_timeOfDay_stdev <- renderInfoBox({
    infoBox(
      value = "stdev##",
      title = "Standard Deviation",
      icon = icon("random")
    )
  })
  # ------------
  # TIME SERIES
  # ------------
  output$tab3_timeSeries <- renderDygraph({
    lungDeaths <- cbind(mdeaths, fdeaths)
    dygraph(lungDeaths)
  })
  output$tab3_timeSeries_mean <- renderInfoBox({
    infoBox(
      title = "Mean",
      value = "mean##",
      icon = icon("bell")
    )
  })
  output$tab3_timeSeries_stdev <- renderInfoBox({
    infoBox(
      title = "Standard Deviation",
      value = "std##",
      icon = icon("random")
    )
  })
  output$tab3_timeSeries_growthRate <- renderInfoBox({
    infoBox(
      title = "Growth Rate",
      value = "gr##",
      icon = icon("line-chart")
    )
  })
  
  
  # ------------------------------
  # TAB 4 INFERENTIAL STATISTICS
  # ------------------------------
  
  
  # ------------------------------
  # TAB 5 CONCLUSION
  # ------------------------------
  
})

