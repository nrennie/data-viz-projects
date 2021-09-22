library(readxl)
library(tidyverse)
library(ggdist)
library(extrafont)
library(patchwork)
library(lubridate)
library(sf)


setwd("C:/Users/rennien/OneDrive - Lancaster University/Programming/Tableau")
dat <- read_excel("ws_data.xlsx") 
#add in variables
#decimal time
dat$dec_time <- sapply(dat$time, function(x) as.numeric(difftime(x, ymd_hms("1899-12-31 00:00:00"), units = "hour")))


#load gps data

setwd("C:/Users/rennien/OneDrive - Lancaster University/Programming/R/western_states_analysis")
r <- st_read("ws_route.gpx", layer="tracks")
plot(r$geometry)

#points data
#altitude plot

r_wp <- st_read("ws.gpx", layer="waypoints")
plot(r_wp$geometry)

#plot these in ggplot, states


