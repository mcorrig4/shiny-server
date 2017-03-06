# SERVER
#-----------------

library(shiny)
source("./helpers.R")
counties <- readRDS("./data/counties.rds")
library(maps)
library(mapproj)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$myMap <- renderPlot({
    
    args <- switch(input$ethnicitySelector,
                   "Percent White" = list(counties$white, "darkgreen"), 
                   "Percent Black" = list(counties$black, "black"),
                   "Percent Hispanic" = list(counties$hispanic, "orange"), 
                   "Percent Asian" = list(counties$asian, "purple")
                   )
    args$legend.title = input$ethnicitySelector
    args$min = input$rangeSelector[1]
    args$max = input$rangeSelector[2]


    do.call(percent_map, args)
  })
})