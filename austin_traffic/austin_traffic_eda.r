install.packages("RJSONIO")

library(RCurl)
library(RJSONIO)
library(plyr)
library(tidyverse)
library(lubridate)
library("ggmap")

traffic <- read_csv("C:/Users/Valued Customer/Documents/R Scripts/austin-traffic/Traffic_Count_Study_area.csv")



# clean column names
traffic <- as.data.frame(traffic) %>% rename(location = '24 HOUR VOLUME COUNT LOCATIONS',
                              northbound = 'NB TOTAL',
                              southbound = 'SB TOTAL',
                              eastbound = 'EB TOTAL',
                              westbound = 'WB TOTOAL',
                              total_volume = 'TOTAL VOLUME',
                              date = DATE)

# Delete time since it is midnight for all entries, convert to date
traffic$date <- gsub(" .*", "", traffic$date)
traffic <- traffic %>% 
              mutate(date = mdy(date)) 

traffic$date <- as.Date(traffic$date)

# Data ranges from 2001 - 2015
traffic %>%   summarise(min = min(date),
                        max = max(date))



# Filter date range to only include dates since 1/1/2010

traffic <- traffic %>% filter(date >= as.Date("2010/01/01"))
nrow(traffic)


# Remove extra location info
traffic$location <- gsub(" - .*","", traffic$location)

# Locate GPS coordinates of location
api_key = 'AIzaSyD_lrb7PLeFP8l-q_p6r9RA4Pz1llCxyE0'

# Convert location into url with api
url <- function(location, return.call = "json") {
  root <- "https://maps.googleapis.com/maps/api/geocode/"
  location <- paste(location, 'Austin', 'TX', sep = ", ") 
  location <- gsub(" ", "+", location)
  u <- paste(root, return.call, "?address=", location, "&key=", api_key, sep = "")
  return(URLencode(u))
}

# Get location info 
geoCode <- function(location,verbose=FALSE) {
  if(verbose) cat(location,"\n")
  u <- url(location)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type  <- x$results[[1]]$geometry$location_type
    formatted_address  <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(.5)
  } else {
    print(x$status)
    return(c(NA,NA,NA, NA))
  }
}

# Get gps coords in chunks, otherwise API is overloaded and fails
for (x in seq(1, nrow(traffic), 50)) {
  if (x == 951) {
    traffic[x:nrow(traffic), 'latitude'] <- apply(traffic[x:(nrow(traffic)),]['location'], 1, geoCode)[1,]
    traffic[x:nrow(traffic), 'longitude'] <- apply(traffic[x:(nrow(traffic)),]['location'], 1, geoCode)[2,]
  } 
  else {
    traffic[x:(x+49), 'latitude'] <- apply(traffic[x:(x+49),]['location'], 1, geoCode)[1,]
    traffic[x:(x+49), 'longitude'] <- apply(traffic[x:(x+49),]['location'], 1, geoCode)[2,]
  }
  Sys.sleep(10)
}

traffic$latitude <- as.numeric(traffic$latitude)
traffic$longitude <- as.numeric(traffic$longitude)

# plot coordinates
# Set API Key
ggmap::register_google(key = "AIzaSyA94oLsseE35T4_081pfqaeePzAP0ePvi0")

p <- ggmap(get_googlemap(center = c(lon = -97.7431, lat = 30.2672),
                         zoom = 11, scale = 2,
                         maptype ='terrain',
                         color = 'color'))

p + geom_point(aes(x = longitude, y = latitude,  color = total_volume), data = traffic, size = 2) + ggtitle("Traffic hotspots in Austin, TX") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + scale_color_gradient(low = "green",high = "red",  trans='log') 

# first we separate traffic into eastbound, westbound, northbound, and southbound

p + geom_point(aes(x = longitude, y = latitude,  color = total_volume), data = traffic, size = 2) + ggtitle("Traffic hotspots in Austin, TX") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + scale_color_gradient(low = "green",high = "red",  trans='log') 

eastbound <- traffic %>% filter(is.na(eastbound) == FALSE) %>% mutate(latitude = latitude + .0015) %>% select(eastbound, latitude, longitude)

westbound <- traffic %>% filter(is.na(westbound) == FALSE) %>% mutate(latitude = latitude - .0015) %>% select(westbound, latitude, longitude)

northbound <- traffic %>% filter(is.na(northbound) == FALSE) %>% mutate(longitude = longitude + .0015) %>% select(northbound, latitude, longitude)

southbound <- traffic %>% filter(is.na(southbound) == FALSE) %>% mutate(longitude = longitude - .0015) %>% select(southbound, latitude, longitude)

p + geom_point(aes(x = longitude, y = latitude, color = eastbound), data = eastbound, size = 2, shape = 22) +
  geom_point(aes(x = longitude, y = latitude, color = westbound), data = westbound, size = 2, shape = 21) +
  geom_point(aes(x = longitude, y = latitude, color = northbound), data = northbound, size = 2, shape = 24) +
  geom_point(aes(x = longitude, y = latitude, color = southbound), data = southbound, size = 2, shape = 25) +
  ggtitle("Traffic hotspots in Austin, TX") + 
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  scale_color_gradient(low = "green",high = "red",  trans='log') 

# there is no visual difference between northbound/southbound rates and eastbound/westbound rates and the map is cluttered by shapes, so we will aggregate into two groups

eastwest <- traffic %>% filter(is.na(eastbound) == FALSE) %>% select(total_volume, latitude, longitude)
northsouth <- traffic %>% filter(is.na(northbound) == FALSE) %>% select(total_volume, latitude, longitude)

p + geom_point(aes(x = longitude, y = latitude, color = total_volume), data = eastwest, size = 2, shape = 15) +
  geom_point(aes(x = longitude, y = latitude, color = total_volume), data = northsouth, size = 2, shape = 17) +
  ggtitle("Traffic hotspots in Austin, TX") + 
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  scale_color_gradient(low = "green",high = "red",  trans='log') 

# This is a better and more clear representation of the data

