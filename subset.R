# Stats Project
# ======================

# Include Packages
# ======================
library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
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


# DATA PREPARATION
# ======================
# Inputs:
#	loc - a list containing 2 elements: vector(latitude/longitude) for bottom-left and top-right points
#	tim - a list containing 2 elements: vector(start and end time), interval (Y, M)
#	typ - a list containing 1 element:  vector(primary type)
tim <- list(year=c(2003,2016), month=c(1,12))
typ <- list(prm=c("HOMICIDE", "ASSAULT", "BATTERY"))
loc <- list(bl=c(min(df.raw$Latitude, na.rm=T), min(df.raw$Longitude, na.rm=T)), 
			tr=c(max(df.raw$Latitude, na.rm=T), max(df.raw$Longitude, na.rm=T)))


df <- df.raw %>%
	filter(!is.na(Latitude), !is.na(Longitude), !is.na(Date), !is.na(`Primary Type`)) %>%
	filter(`Primary Type` %in% typ$prm) %>%
	filter(Latitude >= loc$bl[1], Latitude <= loc$tr[1], Longitude >= loc$bl[2], Longitude <= loc$tr[2])%>%
	filter(Year>=tim$year[1], Year<=tim$year[2], Month>=tim$month[1], Month<=tim$month[2])

df.totals <- df %>%
	group_by(Year, Month) %>%
	summarise(Total=n())



# DESCRIPTIVE STATISTICS
# ===============================

# 1. Mean and Standard Dev for Number of Incidents (Monthly)
# -------------------------------
days <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),14)
days[26 + (4*12)*(0:2)] <- 29
monthly_mean <- mean(df.totals$Total) 
monthly_std <- sd(df.totals$Total)

# 2. Mean and Standard Dev for Number of Incidents (Annually)
annual_total <- df.totals %>%
	summarise(Total=sum(Total))
annual_mean <- mean(annual_total$Total)
annual_std <- sd(annual_total$Total)

# 3. Mean Time of Day for the Occurence of Crime
# -------------------------------
hour_mean <- df %>%
	summarise(mean_hour=mean(Hour))


# DESCRIPTIVE GRAPHS
# ===============================

# 1. Histogram Displaying Frequency of Crime based on Time of Day
# -------------------------------
p <- plot_ly(alpha = 0.6) %>%
	add_histogram(x = df$Hour)


# 2. Time Series Graph Displaying the Number of Incidents of Crime in Every Month
# ------------------------------

# Convert to Time Series
# -------------------------------
df.month <- as.data.frame(df.totals)
ts.month <- df.month %>%
	select(-Year, -Month) %>%
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
model <- ets(ts.month, h=8)

hw <- HoltWinters(ldeaths)
predicted <- predict(hw, n.ahead = 72, prediction.interval = TRUE)

dygraph(predicted, main = "Predicted Lung Deaths (UK)") %>%
	dyAxis("x", drawGrid = FALSE) %>%
	dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
	dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))

dygraph(combined , main = "Forcested Crime Rate") %>%
	dyAxis("x", drawGrid = FALSE) %>%
	dySeries("actual", label = "Actual") %>%
	dySeries(paste0("predicted.", c("lwr", "fit", "upr")), label = "Predicted") %>%
	dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))




# Using ets function
# ----------------------
?ets
?forecast
f.ets <- ets(ts.month)
f.ets
f <- forecast(model)
plot(f)
f.ets$residuals

# ACF
# ------
acf(f.ets$residuals, lag.max=20)
Box.test(f.ets$residuals, lag=20, type="Ljung-Box")

plot.ts(f.ets$residuals)
plotForecastErrors(f.ets$residuals)


plotForecastErrors <- function(forecasterrors)
{
	# make a histogram of the forecast errors:
	mybinsize <- IQR(forecasterrors)/4
	mysd   <- sd(forecasterrors)
	mymin  <- min(forecasterrors) - mysd*5
	mymax  <- max(forecasterrors) + mysd*3
	# generate normally distributed data with mean 0 and standard deviation mysd
	mynorm <- rnorm(10000, mean=0, sd=mysd)
	mymin2 <- min(mynorm)
	mymax2 <- max(mynorm)
	if (mymin2 < mymin) { mymin <- mymin2 }
	if (mymax2 > mymax) { mymax <- mymax2 }
	# make a red histogram of the forecast errors, with the normally distributed data overlaid:
	mybins <- seq(mymin, mymax, mybinsize)
	hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
	# freq=FALSE ensures the area under the histogram = 1
	# generate normally distributed data with mean 0 and standard deviation mysd
	myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
	# plot the normal curve as a blue line on top of the histogram of forecast errors:
	points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


# Adjust for Seasonal Component
# -------------------------------
ts.seasonal.adjust <- ts.month - ts.decomp$seasonal
plot(ts.seasonal.adjust)

# Simple Moving Average to reduce noise
# -------------------------------
ts.sma <- SMA(ts.month,n=5)
plot(ts.smooth)
