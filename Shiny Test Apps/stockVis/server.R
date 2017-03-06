# server.R

library(quantmod)
source("helpers.R")

shinyServer(function(input, output) {
  
    
  dataInput <- reactive({
    getSymbols(input$stockName, src = "yahoo", 
               from = input$date[1],
               to = input$date[2],
               auto.assign = FALSE)
    })
  adjustedData <- reactive(({
    data <- dataInput()
    if (input$inflation) {
      data <- adjust(data)
      #data <- adjust(dataInput())
    }
    return(data)
  }))
    
  output$plot <- renderPlot({
    
    chartSeries(adjustedData(), theme = chartTheme("white"), 
                type = "line", log.scale = input$logScale, TA = NULL)
  })
  
})