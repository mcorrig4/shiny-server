# UI
#---------------

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("stockVis"),
  
  sidebarLayout(#position = "right",
    
    ##################
    #### SIDE BAR ####
    ##################
    sidebarPanel( 
      helpText("Select a stock to examine. Information will be collected from yahoo finance."),
      textInput("stockName", label = "Symbol", value = "AAPL"),
      br(),
      #dateInput("startDate", label = "Date Range", value = "2013-01-01"),
      #dateInput("endDate", label = NULL, value = NULL),
      dateRangeInput("date", label = "Date Range", start = "2013-01-01"),
      br(), 
      br(),
      checkboxInput("logScale", label = "Plot y axis on log scale"),
      checkboxInput("inflation", label = "Adjust prices for inflation")
      ),
    
    ##################
    ### MAIN PANEL ###
    ##################
    mainPanel(
      plotOutput("plot")
    )
  )
))