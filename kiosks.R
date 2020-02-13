library(lubridate)
library(ggplot2)
library(dplyr)
library(data.table)
library(ggrepel)
library(tidyverse)
library("ggmap")

# Get the latest Install
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)

setwd("C:/Users/Valued Customer/Documents/datasets/coa/")

kiosks <- read.csv(file = "Austin_B-Cycle_Kiosk_Locations.csv")
head(kiosks)

kiosks <- subset(kiosks, select=-Location)
head(kiosks)

dim(kiosks)



#Set your API Key
ggmap::register_google(key = "AIzaSyA94oLsseE35T4_081pfqaeePzAP0ePvi0")

p <- ggmap(get_googlemap(center = c(lon = -97.7431, lat = 30.2672),
                         zoom = 13, scale = 2,
                         maptype ='terrain',
                         color = 'color'))

p + geom_point(aes(x = Longitude, y = Latitude,  color = Kiosk.Status), data = kiosks, size = 4) + theme(legend.position="bottom") + ggtitle("BCycle Locations in Austin, TX") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())


