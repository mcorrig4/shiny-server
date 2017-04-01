
#Clear environment
rm(list=ls()) 

#Chicago Crime Statistics 2001 to Present
library(lubridate)
library(dplyr)
library(ggplot2)
library(leaflet)
library(ggmap)
library(readr)
library(rgdal)
library(sp)
library(tigris)
library(acs)
library(stringr)


# *** SET WORKING DIRECTORY TO WHERE THE FILE IS ***

ChiCrimeDF <- read_csv("./data/Crimes-2001-present.csv")
df.raw <- ChiCrimeDF[sample(1:6280886,10000,replace=T),]

print("Setting up...Please Wait...")

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

# ----------------------------------
# GRAB THE SPATIAL DATA USING TARGIS
# ----------------------------------
tracts <- tracts(state = "IL", county = 031)

# ------------------------------
# GET THE TABULAR DATA USING ACS
# ------------------------------
api.key.install(key="81fa7113920ce054779c411ea0a9f6a6fed1bad2") #2010

# create a geographic set to grab tabular data (acs)
geo <- geo.make(state = "IL",
				county = 031, tract = "*")#, block.group = "*")

acs.lookup(endyear = 2012, span = 5, keyword = "race", case.sensitive = FALSE) #used to find tables

ethnicity <- acs.fetch(endyear = 2012, span = 5, geography = geo, 
					   table.number = "B03002", 
					   #table.name = "RACE",
					   col.names = "pretty")

# convert to a data.frame for merging
ethnicity.df <- data.frame(paste0(str_pad(ethnicity@geography$state, 2, "left", pad="0"), 
								  str_pad(ethnicity@geography$county, 3, "left", pad="0"), 
								  str_pad(ethnicity@geography$tract, 6, "left", pad="0")),
						   ethnicity@estimate[,c("Hispanic or Latino by Race: Total:",
						   					  "Hispanic or Latino by Race: Not Hispanic or Latino: Black or African American alone")], 
						   stringsAsFactors = FALSE)

ethnicity.df <- select(ethnicity.df, 1:3)
rownames(ethnicity.df) <- 1:nrow(ethnicity.df)
names(ethnicity.df) <- c("GEOID", "total", "black")
ethnicity.df$percent <- 100*(ethnicity.df$black/ethnicity.df$total)

# ----------------------------------
# MERGE THE SPATIAL AND TABULAR DATA
# ----------------------------------
eth.merged <- geo_join(tracts, ethnicity.df, "GEOID", "GEOID")
# there are some tracts with no land that we should exclude
eth.merged <- eth.merged[eth.merged$ALAND>0, ]

# ----------------------
# MAKE MAP USING LEAFLET
# ----------------------

popup   <- paste0 ("GEOID: ", eth.merged$GEOID, "<br>", 
				   "Percent of Black Households: ", round(eth.merged$percent, 2))
pal     <- colorNumeric( palette = "YlGnBu",
						 domain = eth.merged$percent)

agg.map    <- leaflet(options = leafletOptions(minZoom = 8, maxZoom = 18)) %>%
	addProviderTiles("CartoDB.Positron") %>%
	addPolygons(data = eth.merged, 
				fillColor = ~pal(percent), 
				color = "#b2aeae", # you need to use hex colors
				fillOpacity = 0.7, 
				weight = 1, 
				smoothFactor = 0.2,
				popup = popup) %>%
	addMarkers(data = df,
			   popup = ~sprintf('Violent Crime Description: ', Description), 
			   #layerId = rownames(quakes),
			   clusterOptions = markerClusterOptions(FreezeAtZoom = 12)) %>%  
	addLegend(pal = pal,
			  values = eth.merged$percent,
			  position = "bottomright",
			  title = "Percent of African <br>American Households",
			  labFormat = labelFormat(suffix = "%"))
agg.map

print("Done Loading.")

#colnames(ChiCrime.test) <- colnames(ChiCrimeDF)

#Setting input box boundaries
# set.llLon <- -87.73949
# set.llLat <- 41.79613
# set.urLon <- -87.51977
# set.urLat <- 41.95973
#
# set.timeRange <-
# set.violentCrime <-
# map.location <- c(set.llLon, set.llLat, set.urLon, set.urLat)
#
# #setting map with Boundaries
# map.Chicago <- get_map(location=map.location, source="google",  maptype="roadmap", crop=FALSE)
# ggmap(map.Chicago) + geom_point  (aes(x=Longitude, y=Latitude, colour=ChiCrimeDF$Primary.Type, colour=factor(ChiCrimeDF$Year)), data = ChiCrimeDF, alpha=0.1)