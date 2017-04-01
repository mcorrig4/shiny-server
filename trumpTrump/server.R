#######################
#####  SERVER.R  ######
#######################

library(shiny)
library(shinydashboard)
source("loader.R")


data.source <- "https://data.cityofchicago.org/api/views/ijzp-q8t2/rows.csv"
data.file <- "./data/rows.csv"
fake.connection = FALSE


shinyServer(function(input, output, session) {
  # ------------
  # LOAD EVERYTHING AND RETURN CONNECTION TO DATABASE TABLE
  #-------------
  crimes.df <- read.csv(data.file)
  
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
  
  
  df.raw <- read_csv(data.file)
  
  # Manually adjust column types
  #-----------------------
  df.raw$`Primary Type` <- as.factor(df.raw$`Primary Type`)
  df.raw$`Description` <- as.factor(df.raw$`Description`)
  df.raw$`Location Description` <- as.factor(df.raw$`Location Description`)
  df.raw$Date <- as.POSIXct(df.raw$Date, format="%m/%d/%Y %I:%M:%S %p", tz="CST6CDT")
  df.raw$Month <- month(df.raw$Date)
  df.raw$Day <- day(df.raw$Date)
  df.raw$Hour <- hour(df.raw$Date)

  # SETUP
  #===========
  # POPULATION DENSITY CALCULATION
  # ===============================
  population_2000 <- 2893666
  population_2010 <- 2695598
  
  # CAGR - Compound Annual Growth Rate of the Population
  cagr <- (population_2010/population_2000)^(1/(10*12)) - 1; cagr
  
  num_years <- 18
  num_months <- 18*12
  
  #Starting Point
  year <- rep(2000:2017,each=12); year; length(year)
  month <- rep(1:12, 18); month; length(month)
  
  pop <- population_2000
  
  for (i in 1:(num_months-1)) {
    pop[i+1] <- pop[i] * (1+cagr)	
  }	
  
  df.population <- data.frame(Year=year, Month=month, Population=pop/100000)
  
  
  # AREA OF CHICAGO
  # ===============================
  bl <- c(min(df.raw$Latitude, na.rm=T), min(df.raw$Longitude, na.rm=T))
  tp <- c(max(df.raw$Latitude, na.rm=T),max(df.raw$Longitude, na.rm=T))
  area_chicago <- (tp[1]-bl[1])*(tp[2]-bl[2]); area_chicago
  
  
  # DATA PREPARATION
  # ======================
  # Inputs:
  #	loc - a list containing 2 elements: vector(latitude/longitude) for bottom-left and top-right points
  #	tim - a list containing 2 elements: vector(start and end time), interval (Y, M)
  #	typ - a list containing 1 element:  vector(primary type)
  tim <- list(year=c(2001,2017), month=c(1,2))
  typ <- list(prm=c("ASSAULT", "BATTERY"))
  loc <- list(bl=c(min(df.raw$Latitude, na.rm=T), min(df.raw$Longitude, na.rm=T)), 
              tr=c(max(df.raw$Latitude, na.rm=T), max(df.raw$Longitude, na.rm=T)))
  
  
  sel_area <- (loc$tr[1]-loc$bl[1])*(loc$tr[2]-loc$bl[2]); sel_area
  sel_ratio <- sel_area/area_chicago
  print("sel_ratio");print(sel_ratio)
  print("df");print(df)
  df <- df.raw %>%
    filter(!is.na(Latitude), !is.na(Longitude), !is.na(Date), !is.na(`Primary Type`)) %>%
    filter(`Primary Type` %in% typ$prm) %>%
    filter(Latitude >= loc$bl[1], Latitude <= loc$tr[1], Longitude >= loc$bl[2], Longitude <= loc$tr[2]) %>%
    filter(((Year>tim$year[1] & Year<tim$year[2]) | (Year==tim$year[1] & Month>=tim$month[1]) | (Year==tim$year[2] & Month<=tim$month[2])))
  
  print("df");print(df)
  df <- df %>%
    inner_join(df.population, by = c("Year", "Month"))
  print(df.population);print(dim(df.population))
  print("length(df)");print(length(df));print(names(df));print(dim(df))
  
  df.totals <- df %>%
    group_by(Year, Month, Population) %>%
    summarise(Total=n()) %>%
    mutate(CrimeRate=Total/(Population*sel_ratio))
  
  print("df.totals");print(df.totals)
  
  # DESCRIPTIVE STATISTICS
  # ===============================
  
  # 1. Mean and Standard Dev for CrimeRate (Monthly)
  # -------------------------------
  monthly_mean <- mean(df.totals$CrimeRate); monthly_mean 
  monthly_std <- sd(df.totals$CrimeRate); monthly_std
  
  #    Mean and Standard Dev for Crime Rate (Annually)
  annual_total <- df.totals %>%
    summarise(Total=sum(CrimeRate))
  annual_mean <- mean(annual_total$Total); annual_mean
  annual_std <- sd(annual_total$Total); annual_std
  
  #2. Average Annual Growth Rate 
  
  monthly_growthrate <- diff(df.totals$CrimeRate)/df.totals$CrimeRate[1:(length(df.totals$Total)-1)]
  avg_monthly_growthrate <- mean(monthly_growthrate); avg_monthly_growthrate
  
  
  # 3. Mean Time of Day for the Occurence of Crime
  # -------------------------------
  hour_stats <- df %>%
    summarise(mean_hour=mean(Hour), std_hour=sd(Hour)); hour_stats
  
  
  # DESCRIPTIVE GRAPHS
  # ===============================
  
  # 1. Histogram Displaying Frequency of Crime based on Time of Day
  # -------------------------------

  

  # 2. Time Series Graph Displaying the Number of Incidents of Crime in Every Month
  # ------------------------------
  
  # Convert to Time Series
  # -------------------------------
  
  df.month <- as.data.frame(df.totals)
  ts.month <- df.month %>%
    select(CrimeRate) %>%
    ts( frequency=12, start=c(tim$year[1],tim$month[1]), end=c(tim$year[2],tim$month[2]))
  
 
  
  
 
  
  
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
  df <- crimes.df
  df$Date <- as.POSIXct(df$Date, format="%m/%d/%Y %I:%M:%S %p", tz="CST6CDT")
  df$Month <- month(df$Date)
  df$Day <- day(df$Date)
  df$Hour <- hour(df$Date)
  output$tab3_timeOfDay_hist <- renderPlotly({
    
    plot_ly(alpha = 0.6) %>%
      add_histogram(x = df$Hour) %>%
      layout(xaxis = list(title="Time (Hours)"), yaxis= list(title="Frequency"))
    
    
  })

  hour_stats <- df %>% summarise(mean_hour=mean(Hour), std_hour=sd(Hour))
  
  output$tab3_timeOfDay_mean <- renderInfoBox({
    infoBox(
      value = round(hour_stats$mean_hour, 4),
      title = "Mean",
      icon = icon("bell")#icon("bars")
    )
  })
  output$tab3_timeOfDay_stdev <- renderInfoBox({
    infoBox(
      value = round(hour_stats$std_hour, 4),
      title = "Standard Deviation",
      icon = icon("random")
    )
  })
  # ------------
  # TIME SERIES
  # ------------
  
  output$tab3_timeSeries <- renderDygraph({
    
    dygraph(ts.month, main = "Monthly Incidents of Crime in Chicago") %>%
      dyOptions(fillGraph=TRUE, fillAlpha=0.2) %>%
      dySeries(label=paste(typ$prm, sep="", collapse="+")) %>%
      dyAxis("x", label = "Time (Months)", valueRange=c(tim$year[1], tim$year[2])) %>%
      dyAxis("y", label = "Number of Incidents") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>%
      dyRangeSelector()
  })
  output$tab3_timeSeries_mean_y <- renderInfoBox({
    infoBox(
      title = h5("Annual Mean"),
      value = round(annual_mean,4),
      icon = icon("bell")
    )
  })
  output$tab3_timeSeries_stdev_y <- renderInfoBox({
    infoBox(
      title = h5("Annual Standard Deviation"),
      value = round(annual_std,4),
      icon = icon("random")
    )
  })
  output$tab3_timeSeries_growthRate_y <- renderInfoBox({
    infoBox(
      title = h5("Annual Growth Rate"),
      value = round(annual_total,4),
      icon = icon("line-chart")
    )
  })
  
  
  output$tab3_timeSeries_mean_m <- renderInfoBox({
    infoBox(
      title = h5("Monthly Mean"),
      value = round(monthly_mean,4),
      icon = icon("bell")
    )
  })
  output$tab3_timeSeries_stdev_m <- renderInfoBox({
    infoBox(
      title = h5("Monthly Standard Deviation"),
      value = round(monthly_std,4),
      icon = icon("random")
    )
  })
  output$tab3_timeSeries_growthRate_m <- renderInfoBox({
    infoBox(
      title = h5("Monthly Growth Rate"),
      value = round(monthly_growthrate,4),
      icon = icon("line-chart")
    )
  })
  
  # ------------------------------
  # TAB 4 INFERENTIAL STATISTICS
  # ------------------------------
  
  
  # ------------------------------
  # TAB 5 CONCLUSION
  # ------------------------------
  
  
  # ------------------------------
  # REACTIVE FUNCTIONS
  # ------------------------------
  
  tab2_map_center <- reactive({
    input$tab2_map_center
  })
  
  
  crimeTypeValues <- reactive({
    vals <- input$tab2_crimeTypes
    return(vals)
  })
  
  dateRangeValues <- reactive({
    vals <- input$tab2_datesRange
    return(vals)
  })
  
  mapValue <- reactive({
    vals <- input$tab2_map_bounds
    return(vals)
  })
  
  bl <- data.frame(lat = 36.6, lng = -91.7)
  tr <- data.frame(lat = 42.0, lng = -87.5)
  locc <- data.frame(bl = c(bl$lat, bl$lng), tr = c(tr$lat, tr$lng))
  
  
  observe({
    event <- input$tab2_map_click
    if (is.null(event)) {
      return()
    }
    bounds <- mapValue()
    bl$lat <- bounds$south
    bl$lng <- bounds$west
    tr$lat <- bounds$north
    tr$lng <- bounds$east
    locc$bl <- c(bl$lat, bl$lng)
    locc$tr <- c(tr$lat, tr$lng)
  })
  
  
  # tim <- reactive ({
  #   drange <- input$tab2_datesRange
  #   ret <- list(year = c(year(drange[1]), year(drange[2])), month = c(month(drange[1], drange[2])))
  #   return(ret)
  # })
  
  #typ <- reactive({
  #  ret <- list(prm = as.vector(input$tab2_crimeTypes))
  #  return(ret)
  #})
  
  # loc <- reactive({
  #   bounds2 <- input$tab2_map_bounds
  #   bounds <- data.frame(north = 42, south = 36.6, east = -87.5, west = -91.7)
  #   if (is.null(bounds)) {
  #     bounds <- data.frame(north = locc$tr[1], south = locc$bl[1], east = locc$tr[2], west = locc$bl)
  #   }
  #   ret <- list(bl = c(bounds$south, bounds$west), tr = c(bounds$north, bounds$east))
  #   return(ret)
  # })
  
  
  
  
  
  
  
  
  
  
  
  
})

