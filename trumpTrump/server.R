#######################
#####  SERVER.R  ######
#######################

#setwd("C:/Users/liamc/Google Drive/Ivey/BUSS - Business Statistics 9002b-2/shiny-server/ski_case")

library(shiny)
library(dplyr)
source("helper.R")


data.source <- "https://data.cityofchicago.org/api/views/ijzp-q8t2/rows.csv"



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # ------------
  # HEADER BAR
  #-------------
  
  output$messageMenu <- renderMenu({
    # Code to generate each of the messageItems here, in a list. This assumes
    # that messageData is a data frame with two columns, 'from' and 'message'.
    msgs <- apply(messageData, 1, function(row) {
      messageItem(from = row[["from"]], message = row[["message"]])
    })
    
    # This is equivalent to calling:
    #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
    dropdownMenu(
      type = "messages",
      badgeStatus = "warning", #makes the status indicator yellow
      .list = msgs
    )
  })
  
  
  # ------------
  # TAB 1
  #-------------
  

  
  # ------------
  # TAB 2
  #-------------
  
  
  # ------------
  # TAB 3
  #-------------
})