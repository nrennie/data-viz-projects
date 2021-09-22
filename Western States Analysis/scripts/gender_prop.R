library(readxl)
library(tidyverse)
library(ggdist)
library(extrafont)
library(patchwork)
library(lubridate)

setwd("C:/Users/rennien/OneDrive - Lancaster University/Programming/Tableau")
dat <- read_excel("ws_data.xlsx") 

#geom_area plot of proportion
gender_count <- dat %>% group_by(gender) %>% count(year)
p <- ggplot() +
  geom_area(data = gender_count, aes(x = year, y = n, fill = gender), stat = "identity", position = "fill") + 
  scale_fill_manual("", values=c("#6f50a1", "#1e8f89"), labels=c("Female", "Male")) +
  coord_cartesian(expand = F) +
  labs(x="", y="Proportion of finishers\n", title="MALE V FEMALE PROPORTION", 
       caption = "N. Rennie | Data: www.wser.org",
       subtitle="The first female runner completed the race in 1978. Since then, the proportion\nof female finishers has been slowly increasing, but is still less than 25%.\n") +
  theme(panel.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        plot.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        legend.background = element_rect(fill = "#ffdbac"),
        legend.position = "bottom",
        legend.key = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        axis.text = element_text(colour = "#4e250c", size=12, family="Segoe UI"),
        axis.title = element_text(colour = "#4e250c", size=12, family="Segoe UI", face="plain"),
        plot.title = element_text(colour = "#8a5f3c", size=28, hjust=0, family="Playbill"),
        plot.subtitle = element_text(colour = "#4e250c", size=12, hjust=0, family="Segoe UI"),
        plot.caption = element_text(colour = "#4e250c", size=12, hjust = 0, family="Segoe UI"),
        legend.text = element_text(colour = "#4e250c", size=12, family="Segoe UI"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm"))
p

#save image
dev.new(width=7,height=7,unit="in", noRStudioGD = TRUE)


