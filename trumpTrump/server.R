#######################
#####  SERVER.R  ######
#######################

library(shiny)
library(shinydashboard)
source("loader.R")


data.source <- "https://data.cityofchicago.org/api/views/ijzp-q8t2/rows.csv"
data.file <- "./data/rows.csv"



shinyServer(function(input, output) {
  crimes_tbl <- load_data(data.file, fake.connection = FALSE)
  
  # ------------
  # HEADER BAR
  #-------------
  
 
  # ------------
  # TAB 1
  #-------------
  

  # ------------
  # TAB 2
  #-------------
  
  
  # ------------
  # TAB 3
  #-------------
  
  
  # ------------
  # TAB 4
  #-------------
  
  
  # ------------
  # TAB 5
  #-------------
  
})

