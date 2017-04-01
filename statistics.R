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
setwd("C://Users//mgencare//Documents//2. Academics//2. Academic 2017-2018//3. Courses//1. BUSS - Business Statistics//Project")

# DATA LOAD
# ======================
df.raw <- read_csv("Crimes-2001-present.csv")

# Manually adjust column types
#-----------------------
df.raw$`Primary Type` <- as.factor(df.raw$`Primary Type`)
df.raw$`Description` <- as.factor(df.raw$`Description`)
df.raw$`Location Description` <- as.factor(df.raw$`Location Description`)
df.raw$Date <- as.POSIXct(df.raw$Date, format="%m/%d/%Y %I:%M:%S %p", tz="CST6CDT")
df.raw$Month <- month(df.raw$Date)
df.raw$Day <- day(df.raw$Date)
df.raw$Hour <- hour(df.raw$Date)

# POPULATION DENSITY CALCULATION
# ===============================
population_2000 <- 2893666
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

# DATA PREPARATION
# ======================
# Inputs:
#	loc - a list containing 2 elements: vector(latitude/longitude) for bottom-left and top-right points
#	tim - a list containing 2 elements: vector(start and end time), interval (Y, M)
#	typ - a list containing 1 element:  vector(primary type)
tim <- list(year=c(2001,2017), month=c(1,2))
typ <- reactive({
  ret <- list(prm = input$tab2_)
})

loc <- reactive({
  bounds <- input$tab2_map_bounds
  ret <- list(bl = c(bounds$south, bounds$west), tr = c(bounds$north, bounds$east))
  return(ret)
})

  list(bl=c(min(df.raw$Latitude, na.rm=T), min(df.raw$Longitude, na.rm=T)), 
			tr=c(max(df.raw$Latitude, na.rm=T), max(df.raw$Longitude, na.rm=T)))

sel_area <- (loc$tr[1]-loc$bl[1])*(loc$tr[2]-loc$bl[2]); sel_area
sel_ratio <- sel_area/area_chicago

df <- df.raw %>%
	filter(!is.na(Latitude), !is.na(Longitude), !is.na(Date), !is.na(`Primary Type`)) %>%
	filter(`Primary Type` %in% typ$prm) %>%
	filter(Latitude >= loc$bl[1], Latitude <= loc$tr[1], Longitude >= loc$bl[2], Longitude <= loc$tr[2]) %>%
	filter(((Year>tim$year[1] & Year<tim$year[2]) | (Year==tim$year[1] & Month>=tim$month[1]) | (Year==tim$year[2] & Month<=tim$month[2])))

df <- df %>%
	inner_join(df.population, by = c("Year", "Month"))

df.totals <- df %>%
	group_by(Year, Month, Population) %>%
	summarise(Total=n()) %>%
	mutate(CrimeRate=Total/(Population*sel_ratio))
	

# DESCRIPTIVE STATISTICS
# ===============================

# 1. Mean and Standard Dev for CrimeRate (Monthly)
# -------------------------------
monthly_mean <- mean(df.totals$CrimeRate); monthly_mean 
monthly_std <- sd(df.totals$CrimeRate); monthly_std

# 2. Mean and Standard Dev for Crime Rate (Annually)
annual_total <- df.totals %>%
	summarise(Total=sum(CrimeRate))
annual_mean <- mean(annual_total$Total); annual_mean
annual_std <- sd(annual_total$Total); annual_std

#3. Average Annual Growth Rate 
monthly_growthrate <- diff(df.totals$CrimeRate)/df.totals$CrimeRate[1:(length(df.totals$Total)-1)]
avg_monthly_growthrate <- mean(monthly_growthrate); avg_monthly_growthrate


# 3. Mean Time of Day for the Occurence of Crime
# -------------------------------
hour_stats <- df %>%
	summarise(mean_hour=mean(Hour), std_hour=sd(Hour)); hour_stats


# DESCRIPTIVE GRAPHS
# ===============================

# 1. Histogram Displaying Frequency of Crime based on Time of Day
# -------------------------------
p <- plot_ly(alpha = 0.6) %>%
	add_histogram(x = df$Hour) %>%
	layout(xaxis = list(title="Time (Hours)"), yaxis= list(title="Frequency"))

p
# 2. Time Series Graph Displaying the Number of Incidents of Crime in Every Month
# ------------------------------

# Convert to Time Series
# -------------------------------

df.month <- as.data.frame(df.totals)
ts.month <- df.month %>%
	select(CrimeRate) %>%
	ts( frequency=12, start=c(tim$year[1],tim$month[1]), end=c(tim$year[2],tim$month[2]))

dygraph(ts.month, main = "Monthly Incidents of Crime in Chicago") %>%
	dyOptions(fillGraph=TRUE, fillAlpha=0.2) %>%
	dySeries(label=paste(typ$prm, sep="", collapse="+")) %>%
	dyAxis("x", label = "Time (Months)", valueRange=c(tim$year[1], tim$year[2])) %>%
	dyAxis("y", label = "Number of Incidents") %>%
	dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>%
	dyRangeSelector()


# INFERENTIAL STATISTICS - FORECASTING
# ===============================
# Decompose Time Series into Trend, Seasonal, and Random components
# -------------------------------
ts.decomp <- decompose(ts.month)
plot(ts.decomp)


# Forecasting Method
# ===============================
model_type <- "ets" #Possible values can be ets, simple, holt, or holt-winters

model <- switch(model_type,
	   "ets" = ets(ts.month),
	   "smp" = HoltWinters(ts.month, beta=FALSE, gamma=FALSE),
	   "hlt" = HoltWinters(ts.month, gamma=FALSE),
	   "hwt" = HoltWinters(ts.month))

interval <- round(0.3*(12*(tim$year[2]-tim$year[1])+(tim$month[2]-tim$month[1])),0)
f.pred <- forecast(model, h=interval); f.pred
f.pred$residuals <- na.omit(f.pred$residuals); f.pred$residuals


ts.pred <- as.data.frame(f.pred) %>%
	select(`Lo 95`, `Point Forecast`, `Hi 95`) %>%
	ts(frequency=12, start=c(tim$year[2], tim$month[2]+1), end=c(tim$year[2]+floor(interval/12), tim$month[2]+interval%%12))

combined <- cbind(ts.month,ts.pred)
colnames(combined) <- c("Actual", "Low", "Point", "High")

dygraph(combined, main = "Forecasting Incidents of Crime in Chicago") %>%
	dyAxis("x", drawGrid = FALSE) %>%
	dySeries("Actual", label = paste(typ$prm, sep="", collapse="+")) %>%
	dySeries(c("Low", "Point", "High"), label = "Predicted") %>%
	dyAxis("x", label = "Time Period", valueRange=c(tim$year[1], tim$year[2])) %>%
	dyAxis("y", label = "Number of Incidents") %>%
	dyHighlight(highlightSeriesOpts = list(strokeWidth = 1)) %>%
	dyRangeSelector()

# ACF
# ===============================

acf(f.pred$residuals, lag.max=20)
Box.test(f.pred$residuals, lag=20, type="Ljung-Box")

plot.ts(f.pred$residuals)

forecasterrors <- f.pred$residuals
	# make a histogram of the forecast errors:
	mybinsize <- IQR(forecasterrors)/4
	mysd   <- sd(forecasterrors)
	mymin  <- min(forecasterrors) - mysd*5
	mymax  <- max(forecasterrors) + mysd*3
	# generate normally distributed data with mean 0 and standard deviation mysd
	mynorm <- rnorm(1000, mean=0, sd=mysd)
	mymin2 <- min(mynorm)
	mymax2 <- max(mynorm)
	if (mymin2 < mymin) { mymin <- mymin2 }
	if (mymax2 > mymax) { mymax <- mymax2 }
	# make a red histogram of the forecast errors, with the normally distributed data overlaid:
	mybins <- seq(mymin, mymax, mybinsize)
	
	forecastdensity <- forecasterrors/length(forecasterrors)

	myhist <- hist(mynorm, plot=TRUE, breaks=mybins)
	points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
	plot_ly(alpha = 0.6, color="red") %>%
		add_histogram(x = forecasterrors, nbinsx=mybins) %>%
		layout(xaxis = list(title="Error"), yaxis= list(title="Frequency")) %>%
		add_lines(x = ~myhist, y = ~myhist$density, mode = "lines", line = list(color = "#5E88FC"))
	
	hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
	# freq=FALSE ensures the area under the histogram = 1
	# generate normally distributed data with mean 0 and standard deviation mysd
	# myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
	# plot the normal curve as a blue line on top of the histogram of forecast errors:
	#points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)

