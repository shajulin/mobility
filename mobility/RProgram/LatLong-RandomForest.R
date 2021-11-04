##################
## R program 
## Author: Shajulin Benedict
## Email: shajulin@iiitkottayam.ac.in
## https://www.kaggle.com/lihyalan/2020-corona-virus-timeseries (Dataset)
##################

# Phases: i) Collect the data in the field
data <- read.csv(file="/home/iotcloud/shaju/workspace/Eclipseworkspace/EnergyAnalyzer/tests/mobility/latlongCovidData.csv",sep=",",head=TRUE, stringsAsFactors=T)
head(data)

# Change the lengthy column names
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(plotly)
library(lubridate)

library(RgoogleMaps)
library(ggmap)
library(geosphere)
library(maps)
library(sp)
library(graphics)
library(gmapsdistance)


# #Select one point in the Indian map for finding the distances
# IndiaMap=c(20.5937,78.9629);
# ModifyMap <- GetMap(center=IndiaMap, zoom=5, maptype = "satellite", destfile = "IndiaNew.png");
# PlotOnStaticMap(ModifyMap);


# #Select a point to notify the latitude and longitude from the map
# points=IdentifyPoints(ModifyMap, n=1, verbose);
# PlotOnStaticMap(ModifyMap, lat = c(points[1,1]), lon = c(points[1,2]), lwd=3.5,col=c('red'), add=TRUE);
# lonMapPoint <- points[2];
# latMapPoint <- points[,1];


# #Identify the name of the location where we want to find the quality of water
# Location <- revgeocode(c(lonMapPoint,latMapPoint), output="address");
# print(Location);

# Rename required columns
names(data)[names(data) == "confirmed_cases"] <- "confirmed-cases"

# [1] "country"         "latitude"        "longitude"       "confirmed-cases"
# [5] "deaths"          "recovered"       "update_time"     "country_code"   
# [9] "region" 

# Identify NUL and mark it as 0
data$recovered[is.na(data$recovered)] <- 0

# Convert date to numeric number and create unique id
filterOmitData <- na.omit(data)
filterOmitData$nRecovered <- as.numeric(filterOmitData$recovered)
filterOmitData$posixDate <- as.POSIXlt(filterOmitData$update_time, "%Y-%m-%d %H:%M:%OS", tz="")
filterOmitData$nDate <- as.numeric(filterOmitData$posixDate)
filterUniqueDate <- transform(filterOmitData,dateID=as.numeric(factor(nDate)))

# Calculate and plot how many entries are found for each countries. 
# Find the unique code for countries
# Find how many times the unique code appeared. 
# Plot the figure. 
filterUniqueDate$nCountry <- as.numeric(filterUniqueDate$country)
filterUniqueCountry <- transform(filterUniqueDate,countryID=as.numeric(factor(nCountry)))
# Order data based on the date of occurance: (Increasing the date, covid cases increases)
OrderData <- na.omit(filterUniqueCountry) #[order(filterUniqueDate[,13],decreasing=TRUE),]



# Plot values for different dates -- increasing date, increases the deaths. 
fig1 <- ggplot(OrderData, aes(x = dateID, y = deaths), color=dateID) +
    geom_col() 
ggplotly(fig1)
# Plot values for different dates -- increasing date, increases the confirmed cases. 
fig2 <- ggplot(OrderData, aes(x = dateID, y = confirmed.cases), color=dateID) +
    geom_col() 
ggplotly(fig2)
# Plot values for different dates -- increasing date, increases the recovered cases. 
fig3 <- ggplot(OrderData, aes(x = dateID, y = nRecovered), color=dateID) + 
scale_y_continuous(limits=c(0,50000)) +
    geom_col() 
ggplotly(fig3)



fig4 <- ggplot(OrderData, aes(x = dateID, y = nRecovered), color=dateID) + 
scale_y_continuous(limits=c(0,50000)) +
    geom_col() 
ggplotly(fig4)


# Store it in filterData
nrow(OrderData)
filterData <- OrderData



