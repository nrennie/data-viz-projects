library(readxl)
library(tidyverse)
library(ggdist)
library(extrafont)
library(patchwork)
library(lubridate)

setwd("C:/Users/rennien/OneDrive - Lancaster University/Programming/Tableau")
dat <- read_excel("ws_data.xlsx") 
#add in variables
#decimal time
dat$dec_time <- sapply(dat$time, function(x) as.numeric(difftime(x, ymd_hms("1899-12-31 00:00:00"), units = "hour")))
#gender placing
m_dat <- filter(dat, gender == "M")

#pyramid plot of winning times
gender_winning <- dat %>% group_by(year, gender) %>% summarise(min_time = min(dec_time))
p3 <- ggplot() +  
  geom_bar(data=gender_winning, aes(x = year, y = min_time, fill = gender), stat = "identity", width = .8) +
  coord_flip()
p3

#distribution of all times
gender_time <- filter(dat, !is.na(dec_time))
p4 <- ggplot(data=gender_time, mapping=aes(x = gender, y = dec_time, fill=gender, colour=gender)) +
  stat_halfeye(adjust = .5, width = .5, .width = c(.5, .95)) + 
  stat_dots(side = "left", dotsize = .4, justification = 1.05, binwidth = (1/12)) +
  coord_flip() 
p4

#For both male and female runners, peaks at 24 hours and 30 hours (the official cut off for most years except ...)

#decide on colour scheme
#make it pretty
#add text
#join together

p <- p1 + plot_spacer() + plot_spacer() + p2 + p3 + p4 + plot_spacer() + plot_spacer() + plot_layout(ncol = 2) + 
  plot_annotation(title="GENDER") & 
  theme(panel.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        plot.background = element_rect(fill = "#ffdbac", colour="#ffdbac"))
p

#brown #8a5f3c

##4e250c

#-2020 covid
#-2008 wildfires
#1975 1 starter no finishes



