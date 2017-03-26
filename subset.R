# Stats Project
# ======================

# Include Packages
# ======================
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
setwd("C://Users//mgencare//Documents//2. Academics//2. Academic 2017-2018//3. Courses//1. BUSS - Business Statistics//Project")

# Load raw data
# ======================
df.raw <- read_csv("Crimes-2001-present.csv")

# Manually adjust column types
#-----------------------
df.raw$`Primary Type` <- as.factor(df.raw$`Primary Type`)
df.raw$`Description` <- as.factor(df.raw$`Description`)
df.raw$`Location Description` <- as.factor(df.raw$`Location Description`)
df.raw$Date <- as.POSIXct(df.raw$Date, format="%m/%d/%Y %I:%M:%S %p", tz="CST6CDT")
df.raw$Year <- year(df.raw$Date)
df.raw$Month <- month(df.raw$Date)
df.raw$Day <- day(df.raw$Date)
df.raw$Hour <- hour(df.raw$Date)


# Filter data based on: location, time, and crime type
# ======================
# Inputs:
#	loc - a list containing 2 elements: vector(latitude/longitude) for bottom-left and top-right points
#	tim - a list containing 2 elements: vector(start and end time), interval (Y, M, D)
#	typ - a list containing 1 element:  vector(primary type)
tim <- list(pd=c(2003,2016), int="Y")
typ <- list(prm=c("ASSAULT", "BATTERY"))
loc <- list(bl=c(min(df.raw$Latitude, na.rm=T), min(df.raw$Longitude, na.rm=T)), 
			tr=c(max(df.raw$Latitude, na.rm=T), max(df.raw$Longitude, na.rm=T)))


df <- df.raw %>%
	filter(!is.na(Latitude), !is.na(Longitude), !is.na(Date), !is.na(`Primary Type`)) %>%
	filter(`Primary Type` %in% typ$prm) %>%
	filter(Latitude >= loc$bl[1], Latitude <= loc$tr[1], Longitude >= loc$bl[2], Longitude <= loc$tr[2])%>%
	filter(switch(tim$int,
		   "Y" = (Year>=tim$pd[1] & Year<=tim$pd[2]),
		   "M" = (Month>=tim$pd[1] & Month<=tim$pd[2]),
		   "D" = (Day>=tim$pd[1] & Day<=tim$pd[2]) ))



# Descriptive Statistics and Graphs
# ======================
# (using HOMICIDE)
df.hom <- df.raw %>% filter(`Primary Type`=="HOMICIDE")


# 1. Calculate the mean time of murders
df.hr <- df.hom %>%
	group_by(`Primary Type`) %>%
	summarise(mean_hour=median(Hour,na.rm=T))
df.hr

ggplot(df.hom, aes(x=Hour, fill=`Primary Type`)) +
	geom_histogram(binwidth = 1, position="stack")


# 2. Calcualte the total amount of the crime type for each year
df.yr <- df.hom %>%
	group_by(`Primary Type`, Year) %>%
	summarise(total_year=n())
as.data.frame(df.yr)
