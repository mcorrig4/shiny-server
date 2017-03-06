# some code taken from tutorial at:
# http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/

#install.packages(c("rgdal", "sp", "leaflet", "dplyr", "ggplot2"))
library(rgdal)    # for readOGR and others
library(sp)       # for spatial objects
library(leaflet)  # for interactive maps (NOT leafletR here)
library(dplyr)    # for working with data frames
library(ggplot2)  # for plotting

#install.packages(c("tigris", "acs", "stringr"))
library(tigris)
library(acs)
library(stringr) # to pad fips codes


# ----------------------------------
# GRAB THE SPATIAL DATA USING TARGIS
# ----------------------------------
# Chicago is in Cook county Illinois
# FIPS code information from http://www2.census.gov/geo/docs/reference/codes/files/st17_il_cou.txt
blocks <- blocks(state = "IL", county = 031) # 031 is the FIPS code for cook county
tracts <- tracts(state = "IL", county = 031)
# this is > 340MB so be patient

# ------------------------------
# GET THE TABULAR DATA USING ACS
# ------------------------------
# 2010 census data on race for cook county
# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=DEC_10_SF1_P9&prodType=table
# table P9 - HISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE
api.key.install(key="81fa7113920ce054779c411ea0a9f6a6fed1bad2") 

# create a geographic set to grab tabular data (acs)
geo <- geo.make(state = "IL",
                county = 031, tract = "*")#, block.group = "*")


# 
acs.lookup(endyear = 2012, span = 5, keyword = "race", case.sensitive = FALSE) #used to find tables

ethnicity <- acs.fetch(endyear = 2012, span = 5, geography = geo, 
                       table.number = "B03002", 
                       #table.name = "RACE",
                       col.names = "pretty")

# the resulting object is not a data.frame it's a list
# to see what's available

names(attributes(ethnicity))
# [1] "endyear"        "span"           "acs.units"      "currency.year"  "modified"       "geography"     
# [7] "acs.colnames"   "estimate"       "standard.error" "class"   


attr(ethnicity, "acs.colnames")
# [1] "Hispanic or Latino by Race: Total:"                                                                                                 
# [2] "Hispanic or Latino by Race: Not Hispanic or Latino:"                                                                                
# [3] "Hispanic or Latino by Race: Not Hispanic or Latino: White alone"                                                                    
# [4] "Hispanic or Latino by Race: Not Hispanic or Latino: Black or African American alone"                                                
# [5] "Hispanic or Latino by Race: Not Hispanic or Latino: American Indian and Alaska Native alone"                                        
# [6] "Hispanic or Latino by Race: Not Hispanic or Latino: Asian alone"                                                                    
# [7] "Hispanic or Latino by Race: Not Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone"                               
# [8] "Hispanic or Latino by Race: Not Hispanic or Latino: Some other race alone"                                                          
# [9] "Hispanic or Latino by Race: Not Hispanic or Latino: Two or more races:"                                                             
# [10] "Hispanic or Latino by Race: Not Hispanic or Latino: Two or more races: Two races including Some other race"                         
# [11] "Hispanic or Latino by Race: Not Hispanic or Latino: Two or more races: Two races excluding Some other race, and three or more races"
# [12] "Hispanic or Latino by Race: Hispanic or Latino:"                                                                                    
# [13] "Hispanic or Latino by Race: Hispanic or Latino: White alone"                                                                        
# [14] "Hispanic or Latino by Race: Hispanic or Latino: Black or African American alone"                                                    
# [15] "Hispanic or Latino by Race: Hispanic or Latino: American Indian and Alaska Native alone"                                            
# [16] "Hispanic or Latino by Race: Hispanic or Latino: Asian alone"                                                                        
# [17] "Hispanic or Latino by Race: Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone"                                   
# [18] "Hispanic or Latino by Race: Hispanic or Latino: Some other race alone"                                                              
# [19] "Hispanic or Latino by Race: Hispanic or Latino: Two or more races:"                                                                 
# [20] "Hispanic or Latino by Race: Hispanic or Latino: Two or more races: Two races including Some other race"                             
# [21] "Hispanic or Latino by Race: Hispanic or Latino: Two or more races: Two races excluding Some other race, and three or more races"

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

popup <- paste0("GEOID: ", eth.merged$GEOID, "<br>", 
                "Percent of Black Households: ", round(eth.merged$percent, 2))
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = eth.merged$percent
)

map3<-leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = eth.merged, 
              fillColor = ~pal(percent), 
              color = "#b2aeae", # you need to use hex colors
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              popup = popup) %>%
  addLegend(pal = pal, 
            values = eth.merged$percent, 
            position = "bottomright", 
            title = "Percent of Households<br>above $200k",
            labFormat = labelFormat(suffix = "%")) 
map3

