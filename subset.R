# Stats Project
# -----------------------

# Load dataset
#---------------------------------------
loadData <- function(filepath, typ="H") {
	df <- read.csv(filepath)
	if (typ == "L") {
		df <- df[1:1000,]
	}
	return(df)
}
# Descriptive Statistics
#---------------------------------------
susbsetData <- function(data, loc, tim, typ) {
	# Inputs:
	#	loc - a list containing 2 elements: vector(latitude/longitude) for bottom-left and top-right points
	#	tim - a list containing 2 elements: vector(start and end time), interval (Y, M, D)
	#	typ - a list containing 1 element:  vector(primary type)
	#--------------------------------------------------------------------------
	df <- data
	df <- df[  !is.na(df$Latitude)
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
	
	return(df)
}

# Descriptive Graphs
# ------------------
plotGraph <- function(df) {
	df.grp <- df %>%
		group_by(Hour)
	
	ggplot(df.grp, aes(Hour, fill=Primary.Type)) +
		geom_histogram(binwidth = 1)
}




# MAIN PROGRAM
# =======================
library(lubridate)
library(dplyr)
library(ggplot2)


setwd("C://Users//mgencare//Documents//2. Academics//2. Academic 2017-2018//3. Courses//1. BUSS - Business Statistics//Project")

crime.data <- loadData("Crimes-2001-present.csv")
tim <- list(pd=c(2003,2016), int="Y")
typ <- list(prm=c("ASSAULT", "BATTERY"))
loc <- list(bl=c(min(df.raw$Latitude, na.rm=T), min(df.raw$Longitude, na.rm=T)), 
			tr=c(max(df.raw$Latitude, na.rm=T), max(df.raw$Longitude, na.rm=T)))


graph.data <- subsetData(crime.data, loc, tim, typ)
plotGraph(graph.data)







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