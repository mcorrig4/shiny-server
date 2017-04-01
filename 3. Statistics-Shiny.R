# Stats Project
# ======================

# Include Packages
# ======================
library(readr)
library(lubridate)
library(dplyr)
library(dygraphs)
library(plotly)
library(TTR)
library(forecast)
library(shiny)
library(ggplot2)

# *** SET WORKING DIRECTORY TO WHERE THE FILE IS ***

# DATA LOAD
# ======================
df.raw <- read_csv("./data/Crimes-2001-present.csv")

print("Setting up...Please wait...")
# Manually adjust column types
#-----------------------
df.raw$`Primary Type` <- as.factor(df.raw$`Primary Type`)
df.raw$`Description` <- as.factor(df.raw$`Description`)
df.raw$`Location Description` <- as.factor(df.raw$`Location Description`)
df.raw$Date <- as.POSIXct(df.raw$Date, format="%m/%d/%Y %I:%M:%S %p", tz="CST6CDT")
df.raw$Month <- month(df.raw$Date)
df.raw$Day <- day(df.raw$Date)
df.raw$Hour <- hour(df.raw$Date)


# SETUP
#===========
# POPULATION DENSITY CALCULATION
# ===============================
population_2000 <- 2893666 #Values obtained from census data
population_2010 <- 2695598

# CAGR - Compound Annual Growth Rate of the Population
cagr <- (population_2010/population_2000)^(1/(10*12)) - 1; cagr

num_years <- 18
num_months <- 18*12

#Starting Point
?rep
year <- rep(2000:2017,each=12); year; length(year)
month <- rep(1:12, 18); month; length(month)

pop <- population_2000

for (i in 1:(num_months-1)) {
	pop[i+1] <- pop[i] * (1+cagr)	
}	

df.population <- data.frame(Year=year, Month=month, Population=pop/100000)


# AREA OF CHICAGO
# ===============================
bl <- c(min(df.raw$Latitude, na.rm=T), min(df.raw$Longitude, na.rm=T))
tp <- c(max(df.raw$Latitude, na.rm=T),max(df.raw$Longitude, na.rm=T))
area_chicago <- (tp[1]-bl[1])*(tp[2]-bl[2]); area_chicago

year <- c("2000" = 2000,"2001"= 2001,"2002"= 2002,"2003"= 2003,"2004"= 2004,"2005"= 2005,"2006"= 2006,"2007"= 2007,"2008" = 2008,"2009"= 2009,"2010"= 2010,"2011"= 2011, "2012"= 2012,"2013"= 2013,"2014"= 2014,"2015"= 2015,"2016"= 2016,"2017"= 2017)
month <- c("Jan"=1, "Feb"=2, "Mar"=3, "Apr"=4, "May"=5, "Jun"=6, "Jul"=7, "Aug"=8, "Sep"=9, "Oct"=10, "Nov"=11, "Dec"=12)
forecast <- c("ETS"="ets", "Simple Exponential Smoothing"="smp", "Holt's Exponential Smoothing"="hlt", "Holt-Winter's Exponential Smoothing"="hwt")



print("Done Loading.")

ui <- fluidPage(
	titlePanel("Love Trumps Hate"),
	sidebarLayout(
		sidebarPanel(
			#Type of Crime to Analyze
			selectInput(inputId ="type",
						label="Type of Violent Crime",
						choices=c("Homicide"= "HOMICIDE",
								  "Assault" = "ASSAULT",
								  "Battery" = "BATTERY",
								  "Sexual Assault" = "CRIM SEXUAL ASSAULT"),
						selected="HOMICIDE",
						multiple=TRUE),
			
			

			# Start Year
			selectInput(inputId ="start_year",
						label="Start Year",
						choices=year,
						multiple=FALSE),
			# Start Month
			selectInput(inputId ="start_month",
						label="Start Month",
						choices=month,
						multiple=FALSE),
			# End Year
			selectInput(inputId ="end_year",
						label="End Year",
						choices=year,
						selected="2017",
						multiple=FALSE),
			# End Month
			selectInput(inputId ="end_month",
						label="End Month",
						choices=month,
						multiple=FALSE),
			# Forecast Type
			selectInput(inputId = "forecast_type",
						label="Forecast Method",
						choices=forecast,
						multiple=FALSE),
			# Submit Button
			actionButton(inputId = "submit_subset",
						 label="Submit")
			
		),
		
		mainPanel(
			dygraphOutput("timeseries"),
			dygraphOutput("forecast")
		)
	)
)


server <- function(input, output) {
	
	v <- reactiveValues()
	
	observeEvent(input$submit_subset, {
								
								print("Processing Selection...")
								typ <- list(prm=input$type)
							    tim <- list(year=c(as.numeric(input$start_year), as.numeric(input$end_year)),
									 		    month=c(as.numeric(input$start_month), as.numeric(input$end_month)))
								loc <- list(bl=c(min(df.raw$Latitude, na.rm=T), min(df.raw$Longitude, na.rm=T)),
												  tr=c(max(df.raw$Latitude, na.rm=T), max(df.raw$Longitude, na.rm=T)))
								
								
								# Filter data
								#-----------------------
								df <- df.raw %>%
									filter(!is.na(Latitude), !is.na(Longitude), !is.na(Date), !is.na(`Primary Type`)) %>%
									filter(`Primary Type` %in% typ$prm) %>%
									filter(Latitude >= loc$bl[1], Latitude <= loc$tr[1], Longitude >= loc$bl[2], Longitude <= loc$tr[2]) %>%
									filter(((Year>tim$year[1] & Year<tim$year[2]) | (Year==tim$year[1] & Month>=tim$month[1]) | (Year==tim$year[2] & Month<=tim$month[2])))

								# Determine the ratio of the area selected
								# -----------------------
								sel_area <- (loc$tr[1]-loc$bl[1])*(loc$tr[2]-loc$bl[2]); sel_area
								sel_ratio <- sel_area/area_chicago
								
								# Join with calculated population data
								# -----------------------
								df <- df %>%
									inner_join(df.population, by = c("Year", "Month"))

								# Calculate the Crime Rate based on Area and Population Density
								# -----------------------
								df.totals <- df %>%
									group_by(Year, Month, Population) %>%
									summarise(Total=n()) %>%
									mutate(CrimeRate=Total/(Population*sel_ratio))
								
								df.month <- as.data.frame(df.totals)

								ts.month <- ts(df.month$CrimeRate, frequency=12, start=c(tim$year[1],tim$month[1]), end=c(tim$year[2],tim$month[2]))
								
								
								# Forecasting Method
								# ===============================
								model_type <- input$forecast_type #Possible values can be ets, simple, holt, or holt-winters
								
								model <- switch(model_type,
												"ets" = ets(ts.month),
												"smp" = HoltWinters(ts.month, beta=FALSE, gamma=FALSE),
												"hlt" = HoltWinters(ts.month, gamma=FALSE),
												"hwt" = HoltWinters(ts.month))
								
								interval <- round(0.3*(12*(tim$year[2]-tim$year[1])+(tim$month[2]-tim$month[1])),0)
								f.pred <- forecast(model, h=interval)
								f.pred$residuals <- na.omit(f.pred$residuals)
								
								ts.pred <- as.data.frame(f.pred) %>%
									select(`Lo 95`, `Point Forecast`, `Hi 95`) %>%
									ts(frequency=12, start=c(tim$year[2], tim$month[2]+1), end=c(tim$year[2]+floor(interval/12), tim$month[2]+interval%%12))
								
								combined <- cbind(ts.month,ts.pred)
								colnames(combined) <- c("Actual", "Low", "Point", "High")
								
								
								# REACTIVE VALUES
								# =======================
								v$typ <- typ
								v$tim <- tim
								v$loc <- loc
								v$crime <- df.totals
								v$time <- ts.month
								v$combined <- combined
								v$residuals <- f.pred$residuals
								
								print("Finished Processing.")
								})
	
	
	output$timeseries <- renderDygraph({
		if (!is.null(v$time)) {
			dygraph(v$time, main = "Monthly Incidents of Crime in Chicago") %>%
				dyOptions(fillGraph=TRUE, fillAlpha=0.2) %>%
				dySeries(label=paste(v$typ$prm, sep="", collapse="+")) %>%
				dyAxis("x", label = "Time (Months)", valueRange=c(v$tim$year[1], v$tim$year[2])) %>%
				dyAxis("y", label = "Crime Rate per 100,000 population") %>%
				dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>%
				dyRangeSelector()
		}
	})
	
	output$forecast <- renderDygraph({
		if (!is.null(v$combined)) {
			dygraph(v$combined, main = "Forecasting Incidents of Crime in Chicago") %>%
				dyAxis("x", drawGrid = FALSE) %>%
				dySeries("Actual", label = paste(v$typ$prm, sep="", collapse="+")) %>%
				dySeries(c("Low", "Point", "High"), label = "Predicted") %>%
				dyAxis("x", label = "Time Period", valueRange=c(v$tim$year[1], v$tim$year[2])) %>%
				dyAxis("y", label = "Crime Rate per 100,000 population") %>%
				dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>%
				dyRangeSelector()
		}
	})
	
}

shinyApp(ui = ui, server = server)


