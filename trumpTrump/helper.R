#######################
#####  HELPER.R  ######
#######################

library(lubridate)
library(dplyr)
library(ggplot2)

load_data <- function() {
  Sys.sleep(2)
  hide("loading_page")
  show("main_content")
}

myStat <- function(data, loc, tim, typ) {
  # Inputs:
  #	loc - a list containing 2 elements: vector(latitude/longitude) for bottom-left and top-right points
  #	tim - a list containing 2 elements: vector(start and end time), interval (Y, M, D)
  #	typ - a list containing 1 element:  vector(primary type)
  #--------------------------------------------------------------------------
  df <- data
  
  df <- df[!is.na(df$Latitude)
           & !is.na(df$Longitude)
           & !is.na(df$Date)
           & !is.na(df$Primary.Type),]
  
  # Subset by type
  # ------------------
  df <- df[df$Primary.Type %in% typ$prm,]
  
  
  # Subset by location
  # ------------------
  df <- df[df$Latitude >= loc$bl[1] 
           & df$Latitude <= loc$tr[1]
           & df$Longitude >= loc$bl[2]	
           & df$Longitude <= loc$tr[2],]
  
  # Subset by time
  # ------------------
  df$Date <- as.POSIXct(as.character(df$Date),format="%m/%d/%Y %I:%M:%S %p", tz="CST6CDT")
  
  df$Year <- year(df$Date)
  df$Month <- month(df$Date)
  df$Day <- day(df$Date)
  df$Hour <- hour(df$Date)
  
  df <- switch(tim$int,
               "Y" = df[df$Year>=tim$pd[1] & df$Year<=tim$pd[2],],
               "M" = df[df$Month>=tim$pd[1] & df$Month<=tim$pd[2],],
               "D" = df[df$Day>=tim$pd[1] & df$Day<=tim$pd[2],])
  
  # Descriptive Graphs
  # ------------------
  df.grp <- df %>%
    group_by(Hour)
  
  ggplot(df.grp, aes(Hour, fill=Primary.Type)) +
    geom_histogram(binwidth = 1)
  
}

