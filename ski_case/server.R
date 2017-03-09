#######################
#####  SERVER.R  ######
#######################

setwd("C:/Users/liamc/Google Drive/Ivey/BUSS - Business Statistics 9002b-2/shiny-server/ski_case")

library(shiny)
library(dplyr)
source("helper.R")


data.set <- read.csv("./Whistler Blackcomb Exhibit1.csv", skip = 2)
names(data.set)

colnames(data.set)[5] <- "Prebookings"
colnames(data.set)[6] <- "Actual.Demand"
colnames(data.set)[7] <- "Holiday"
colnames(data.set)[8] <- "Matched.DOW"

names(data.set)
d <- data.set



pairs(Actual.Demand~Matched.DOW, d)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # ------------
  # TAB 1
  #-------------
  
  dd <- reactiveValues(Actual = d$Actual.Demand, Match = d$Matched.DOW)
  observeEvent(input$daysOfWeek, {
    print(input$daysOfWeek)
    f <- d[d$Day.of.Week == input$daysOfWeek, ]#filter(d, Day.of.Week == input$daysOfWeek)
    print(f)
    dd$Actual <- f$Actual.Demand
    dd$Match <- f$Matched.DOW
    print(dd$Match)
    
  })
  

  showLine <- reactiveValues(show = FALSE)
  observeEvent(input$abButton1, {
    showLine$show <- !showLine$show
  })

  
  output$summaryTxt <- renderText({
    if (showLine$show) {
      print(paste("R^2 = ", summary(lm(dd$Actual ~ dd$Match))$r.squared))
    } else {
      print("R^2 = ")
    }
  })
  output$plot1 <- renderPlot({
    plot(dd$Actual ~ dd$Match, col = "blue")
    if (showLine$show) abline(lm(dd$Actual ~ dd$Match), col = "red")
  })
  
  # ------------
  # TAB 2
  #-------------
  
  
  # ------------
  # TAB 3
  #-------------
})