library(readxl)
library(tidyverse)
library(extrafont)
library(patchwork)
library(lubridate)

dat <- read_excel("ws_data.xlsx") 

#distribution of ages
gender_age <- filter(dat, !is.na(age))
p <- ggplot() +
  geom_density(data=filter(gender_age, gender == "M"), mapping=aes(x=age, fill=gender, colour=gender)) +
  geom_density(data=filter(gender_age, gender == "F"), mapping=aes(x=age, fill=gender, colour=gender)) +
  scale_fill_manual("", values=alpha(c("#B04BC4", "#46a3a3"),0.2), labels=c("Female", "Male")) +
  scale_colour_manual("", values=c("#B04BC4", "#46a3a3"), labels=c("Female", "Male")) +
  labs(x="\nAge", y="") +
  theme(panel.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        plot.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        legend.background = element_rect(fill = "#ffdbac"),
        legend.position = "bottom",
        legend.key = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        axis.text = element_text(colour = "#4e250c", size=12, family="Segoe UI"),
        axis.title = element_text(colour = "#4e250c", size=12, family="Segoe UI", face="plain"),
        plot.title = element_text(colour = "#8a5f3c", size=24, family="Playbill"),
        plot.subtitle = element_text(colour = "#4e250c", size=12, hjust=0, family="Segoe UI"),
        legend.text = element_text(colour = "#4e250c", size=12, family="Segoe UI"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) #top, right, bottom, left
p
