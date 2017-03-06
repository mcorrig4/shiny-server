# SHINY APP TUTORIAL
#-------------------------

library(shiny)
setwd("C:/Users/liamc/Google Drive/Ivey/BUSS - Business Statistics 9002b-2/Shiny")
runApp("app-1")
runApp("census-app", display.mode = "showcase")
counties <- readRDS("census-app/data/counties.rds")
getwd()
library(maps)
library(mapproj)
source("census-app/helpers.R")
counties <- readRDS("census-app/data/counties.rds")
percent_map(counties$white, "darkgreen", "% White")
