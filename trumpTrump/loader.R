#######################
#####  LOADER.R  ######
#######################
source("helper.R")

library(zoo);library(leaflet);library(plotly);library(shinydashboard);library(shinyjs);library(shinythemes);library(rsparkling);library(sparklyr);library(dplyr);library(ggplot2);library(tidyr);library(reshape2);library(readr);library(lubridate);library(tidyr);library(dygraphs);library(plotly);library(TTR);library(forecast);

libraryList <- c(
  "shinyjs",
  "shinythemes",
  "rsparkling",
  "sparklyr",
  "dplyr",
  "ggplot2",
  "tidyr",
  "reshape2",
  "readr",
  "lubridate",
  "tidyr",
  "dygraphs",
  "plotly",
  "TTR",
  "forecast",
  "leaflet",
  "zoo"
)

#------------
# Global Spark Connection
#------------
sc <- NULL


load_data <- function(data.source, fake.connection = FALSE) {
  #--------------
  # LOAD APP
  #--------------
  withProgress(
    message = "Please be patient while app loads",
    style = "notification",
    value = 0,
    detail = "Initializing",
    {
      fakeLag(5)
      #--------------
      # LOAD LIBRARIES
      #--------------
      incProgress(0.05, detail = "Libraries")
      withProgress(
        message = "Loading library:",
        detail = "Initializing",
        value = 0,
        {
          fakeLag()
          nLibs <- length(libraryList)
          step <- 1/(nLibs+1)
          for (i in 1:nLibs) {
            incProgress(step, detail = libraryList[i])
            library(libraryList[i], character.only = TRUE)
          }
          incProgress(step, detail = "Done")
        }
      )  
      #--------------
      # LOAD shinyjs
      #--------------
      incProgress(0.1, detail = "Setup shinyjs")
      useShinyjs()
      fakeLag()
      #--------------
      # LOAD SPARK
      #--------------
      incProgress(0.05, detail = "Hadoop and Spark")
      withProgress(
        message = "Setting up database",
        detail = "Initializing",
        value = 0,
        {
          if(fake.connection) {
            crimes_tbl <- NULL
            fakeLag(5)
            incProgress(0.9, detail = "Almost done")
            fakeLag()
            incProgress(0.1, detail = "Done")
          } else {
            fakeLag()
            incProgress(0.1, detail = "Spark options")
            options(rsparkling.sparklingwater.version = "2.0.3")
            incProgress(0.2, detail = "Initializing Spark connection")
            sc <- spark_connect(master = "local", version = "2.1.0", hadoop_version="2.7")
            incProgress(0.65, detail = "Almost done")
            fakeLag(5.0)
            incProgress(0.05, detail = "Done")
          }
        }
      )
      #--------------
      # READ DATA
      #--------------
      incProgress(0.2, detail = "Reading data into database")
      withProgress(
        message = "Loading data into database",
        detail = "This can take a while, please be patient",
        value = 0,
        {
          fakeLag()
          incProgress(0.1, detail = "This can take a while, please be patient")            
          if(!fake.connection) {
            crimes_tbl <- spark_read_csv(sc, "crimes", data.source)
          }
          incProgress(0.8, detail = "Almost done")
          fakeLag(5.0)
          incProgress(0.09, detail = "Done")
        }
      )
    }
  )
  
  #--------------
  # SWAP LOADIGN FOR MAIN SCREEN
  #--------------
  hide("loading_page", anim = FALSE)
  shinyjs::show("main_content", anim = TRUE, animType = "fade", time = 2.0)
  
  #spark_disconnect(sc)
  return(crimes_tbl)
}

