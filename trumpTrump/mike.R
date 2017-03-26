# Stats Project
# -----------------------

# Load Packages
# =======================
library(lubridate)
library(dplyr)
library(ggplot2)


#setwd("C://Users//mgencare//Documents//2. Academics//2. Academic 2017-2018//3. Courses//1. BUSS - Business Statistics//Project")
setwd("C:/Users/liamc/Google Drive/Ivey/BUSS - Business Statistics 9002b-2/shiny-server/trumpTrump")

# Load dataset
#---------------------------------------
df.crime <- read.csv("./data/crimes.csv")
df.crime2 <- df.crime[1:1000,]

switchvar <- function(typ="L") {
  x <- df.crime
  if (typ == "L") {
    x <- df.crime2
  }
  return(x)
}

df.raw <- switchvar("H")
names(df.raw)


# Descriptive Statistics
#---------------------------------------

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



loc <- list(bl=c(min(df.raw$Latitude, na.rm=T), min(df.raw$Longitude, na.rm=T)), 
            tr=c(max(df.raw$Latitude, na.rm=T), max(df.raw$Longitude, na.rm=T)))
tim <- list(pd=c(2003,2016), int="Y")
#typ <- list(prm=c("CRIM SEXUAL ASSAULT", "SEX OFFENSE", "ASSAULT"))
typ <- list(prm=c("ASSAULT", "BATTERY"))
myStat(df.raw,loc,tim,typ)




# Scrap
#---------------------------------------
# levels(df.raw$Primary.Type)
# df.typ <- group_by(df.raw, Primary.Type)
# summarise(df.typ, count=length(Primary.Type))
# 
# levels(df.raw$Description)
# df.dsc <- group_by(df.raw, Description)
# summarise(df.dsc, count=length(Description))


# ggplot(df.sex_assault, aes(Hour, fill=Primary.Type)) +
# 	geom_histogram(binwidth = 1)


# df2 <- df %>%
# 	group_by(Primary.Type) %>%
# 	#filter(Primary.Type %in% c("CRIM SEXUAL ASSAULT", "SEX OFFENSE")) %>%
# 	droplevels()
# 
# # Display graph
# df.sex_assault <- df.typ %>%
# 	filter(Primary.Type %in% c("CRIM SEXUAL ASSAULT", "SEX OFFENSE")) %>%
# 	droplevels()
# 
# df.sex_assault$Year <- year(df.sex_assault$Date)
# df.sex_assault$Month <- month(df.sex_assault$Date)
# df.sex_assault$Day <- day(df.sex_assault$Date)
# df.sex_assault$Hour <- hour(df.sex_assault$Date)