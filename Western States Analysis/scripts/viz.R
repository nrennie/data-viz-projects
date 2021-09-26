library(readxl)
library(tidyverse)
library(extrafont)
library(patchwork)
library(lubridate)
library(cowplot)
library(sf)
library(gridExtra)


#### read data ####
dat <- read_excel("Western States Analysis/data/ws_data.xlsx") 
r <- st_read("Western States Analysis/data/ws_route.gpx", layer="tracks")
r_wp <- st_read("Western States Analysis/data/ws.gpx", layer="waypoints")


#### prep data ####
dat$dec_time <- sapply(dat$time, function(x) as.numeric(difftime(x, ymd_hms("1899-12-31 00:00:00"), units = "hour")))


#### intro ####
p_intro <- ggplot() +
  labs(x="\n", y="", title="", subtitle="") +
  theme(panel.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        plot.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        legend.background = element_rect(fill = "#ffdbac"),
        legend.position = "bottom",
        legend.key = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        axis.text = element_text(colour = "#4e250c", size=12, family="Segoe UI"),
        axis.title = element_text(colour = "#4e250c", size=12, family="Segoe UI", face="plain"),
        plot.title = element_text(colour = "#8a5f3c", size=36, family="Playbill"),
        plot.subtitle = element_text(colour = "#4e250c", size=12, hjust=0, family="Segoe UI"),
        legend.text = element_text(colour = "#4e250c", size=12, family="Segoe UI"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) #top, right, bottom, left
p_intro


#### hardest year ####
#median per year for men
m_dat <- filter(dat, gender == "M" & year >= "1980") %>% group_by(year) %>% summarise(med = median(dec_time))
p_hardest1 <- ggplot() + 
  geom_bar(data=m_dat, aes_string(x="year",y="med", fill='factor(ifelse(med==max(med), 3, ifelse(med==min(med), 2, 1)))'), stat="identity", colour="transparent") + 
  scale_fill_manual("", values=c("1"='#8a5f3c', "2"='#6f50a1', "3"='#1e8f89'), labels=c("Fastest", "Slowest"), breaks=c()) + 
  labs(x="", y="Median finish time\n(hours)\n") +
  coord_cartesian(expand=F) +
  ggtitle("", subtitle="Male") +
  ylim(0,35) +
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
        plot.margin = unit(c(0.5, 0.5, 1, 1), "cm")) #top, right, bottom, left
p_hardest1
#median per year for women
f_dat <- filter(dat, gender == "F" & year >= "1980") %>% group_by(year) %>% summarise(med = median(dec_time))
p_hardest2 <- ggplot() +
  geom_bar(data=f_dat, aes_string(x="year",y="med", fill='factor(ifelse(med==max(med), 3, ifelse(med==min(med), 2, 1)))'), stat="identity", colour="transparent") + 
  scale_fill_manual("", values=c("1"='#8a5f3c', "2"='#6f50a1', "3"='#1e8f89'), labels=c("Fastest", "Slowest"), breaks=c("2","3")) + 
  labs(x="", y="Median finish time\n(hours)\n") +
  coord_cartesian(expand=F) +
  ylim(0,35) +
  ggtitle("", subtitle="Female") +
  theme(panel.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        plot.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        legend.background = element_rect(fill = "#ffdbac"),
        legend.position = c(1,1),
        legend.justification= c(1,1),
        legend.key.size = unit(0.5, 'cm'),
        legend.key = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        axis.text = element_text(colour = "#4e250c", size=12, family="Segoe UI"),
        axis.title = element_text(colour = "#4e250c", size=12, family="Segoe UI", face="plain"),
        plot.title = element_text(colour = "#8a5f3c", size=24, family="Playbill"),
        plot.subtitle = element_text(colour = "#4e250c", size=12, hjust=0, family="Segoe UI"),
        legend.text = element_text(colour = "#4e250c", size=12, family="Segoe UI"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        plot.margin = unit(c(0.5, 1, 1, 0.5), "cm")) #top, right, bottom, left
p_hardest2


#### route ####
p0 <- ggplot() +
  geom_sf(data=r, colour="#4e250c") +
  geom_sf(data=r_wp, colour="#4e250c") +
  labs(x="", y="") +
  theme(panel.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        plot.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        legend.background = element_rect(fill = "#ffdbac"),
        legend.position = "bottom",
        legend.key = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(colour = "#8a5f3c", size=24, family="Playbill"),
        plot.subtitle = element_text(colour = "#4e250c", size=12, hjust=0, family="Segoe UI"),
        legend.text = element_text(colour = "#4e250c", size=12, family="Segoe UI"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))
p0  
p_route <- cowplot::ggdraw(p0) + theme(panel.background = element_rect(fill = "#ffdbac", colour = "#ffdbac"))
p_route

#### gender ####
gender_winning <- dat %>% group_by(year, gender) %>% summarise(min_time = min(dec_time))
gender_winning_m <- filter(gender_winning, gender=="M")
gender_winning_f <- filter(gender_winning, gender=="F") %>% mutate(min_time_neg = min_time*(-1))

p_gender1 <- ggplot() +  
  geom_bar(data=gender_winning_m, aes(x = year, y = min_time, fill = gender), stat = "identity", width = .8) +
  geom_bar(data=gender_winning_f, aes(x = year, y = min_time_neg, fill = gender), stat = "identity", width = .8) +
  coord_flip() +
  scale_fill_manual("", values=c("F"="#B04BC4","M"="#46a3a3"), labels=c("Female", "Male")) +
  labs(x="", y="\nFinish time (hours)") +
  scale_y_continuous("", limits=c(-30,30), breaks=c(-30,-20,-10,0,10,20,30), labels=c(30,20,10,0,10,20,30)) +
  theme(panel.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        plot.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        legend.background = element_rect(fill = "#ffdbac"),
        legend.position = "none",
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
        plot.margin = unit(c(5, 0.6, 0.8, 0.3), "cm"))
p_gender1 

gender_count <- dat %>% group_by(gender) %>% count(year)
p_gender2 <- ggplot() +
  geom_area(data = gender_count, aes(x = year, y = n, fill = gender), stat = "identity", position = "fill") + 
  scale_fill_manual("", values=c("#B04BC4", "#46a3a3"), labels=c("Female", "Male")) +
  coord_cartesian(expand = F) +
  labs(x="", y="Proportion of finishers\n") +
  theme(panel.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        plot.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        legend.background = element_rect(fill = "#ffdbac"),
        legend.position = c(0.05, 0.05),
        legend.justification = c(0,0),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        legend.key.size = unit(0.5, 'cm'),
        axis.text = element_text(colour = "#4e250c", size=12, family="Segoe UI"),
        axis.title = element_text(colour = "#4e250c", size=12, family="Segoe UI", face="plain"),
        plot.title = element_text(colour = "#8a5f3c", size=28, hjust=0, family="Playbill"),
        plot.subtitle = element_text(colour = "#4e250c", size=12, hjust=0, family="Segoe UI"),
        plot.caption = element_text(colour = "#4e250c", size=12, hjust = 0, family="Segoe UI"),
        legend.text = element_text(colour = "#4e250c", size=12, family="Segoe UI"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        plot.margin = unit(c(5, 1, 0.8, 0.3), "cm"))
p_gender2


#### age ####
#distribution plots
gender_age <- filter(dat, !is.na(age))
p_age1 <- ggplot() +
  geom_density(data=filter(gender_age, gender == "M"), mapping=aes(x=age, fill=gender, colour=gender)) +
  geom_density(data=filter(gender_age, gender == "F"), mapping=aes(x=age, fill=gender, colour=gender)) +
  scale_fill_manual("", values=alpha(c("#B04BC4", "#46a3a3"),0.2), labels=c("Female", "Male")) +
  scale_colour_manual("", values=c("#B04BC4", "#46a3a3"), labels=c("Female", "Male")) +
  labs(x="\nAge", y="") +
  theme(panel.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        plot.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        legend.background = element_rect(fill = "#ffdbac"),
        legend.position = "none",
        legend.key = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        axis.text = element_text(colour = "#4e250c", size=12, family="Segoe UI"),
        axis.title = element_text(colour = "#4e250c", size=12, family="Segoe UI", face="plain"),
        plot.title = element_text(colour = "#8a5f3c", size=24, family="Playbill"),
        plot.subtitle = element_text(colour = "#4e250c", size=12, hjust=0, family="Segoe UI"),
        legend.text = element_text(colour = "#4e250c", size=12, family="Segoe UI"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        plot.margin = unit(c(5, 0.5, 0.5, 0.5), "cm")) #top, right, bottom, left
p_age1
#scatter plot 
p_age2 <- ggplot() +
  geom_point(data=gender_age, mapping=aes(x=age, y=dec_time, fill=gender, colour=gender), size=0.5) +
  scale_fill_manual("", values=alpha(c("#B04BC4", "#46a3a3"),0.2), labels=c("Female", "Male")) +
  scale_colour_manual("", values=c("#B04BC4", "#46a3a3"), labels=c("Female", "Male")) +
  labs(x="\nAge", y="Finish time (hours)\n") +
  theme(panel.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        plot.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        legend.background = element_rect(fill = "#ffdbac"),
        legend.position = c(0.95,0.05),
        legend.justification  = c(1,0),
        legend.key = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        axis.text = element_text(colour = "#4e250c", size=12, family="Segoe UI"),
        axis.title = element_text(colour = "#4e250c", size=12, family="Segoe UI", face="plain"),
        plot.title = element_text(colour = "#8a5f3c", size=24, family="Playbill"),
        plot.subtitle = element_text(colour = "#4e250c", size=12, hjust=0, family="Segoe UI"),
        legend.text = element_text(colour = "#4e250c", size=12, family="Segoe UI"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour="#4e250c"),
        plot.margin = unit(c(5, 1, 0.5, 0.5), "cm")) #top, right, bottom, left
p_age2



#### join plots ####
left_panel <- grid.arrange(p_intro, p_hardest1, nrow = 2, heights=c(1,1))
left_panel

middle_panel <- grid.arrange(p_route, p_hardest2, nrow = 2, heights=c(1,1))
middle_panel

right_panel <- grid.arrange(p_gender1, p_age1, nrow = 2, ncol=1)
right_panel 

right_panel2 <- grid.arrange(p_gender2, p_age2, nrow = 2, ncol=1)
right_panel2 

viz <- grid.arrange(left_panel, middle_panel, right_panel, right_panel2, ncol=4, widths=c(1,1,1,1))


#add text
q <- ggdraw(viz) + 
  #title
  draw_label(label="Western States 100", x=0.05, y=0.92, hjust=0, fontfamily="Playbill", size=48, colour = "#8a5f3c") +
  draw_label(label="The Hardest Years", x=0.05, y=0.60, hjust=0, fontfamily="Playbill", size=24, colour = "#8a5f3c") +
  draw_label(label="Male vs Female Finishers", x=0.5, y=0.93, hjust=0, fontfamily="Playbill", size=24, colour = "#8a5f3c") +
  draw_label(label="Does age matter?", x=0.5, y=0.48, hjust=0, fontfamily="Playbill", size=24, colour = "#8a5f3c") +
  #text
  draw_label(label=  "The Western States 100-Mile Endurance Run\nis the world's oldest 100 mile trail race.\nThe run starts in Olympic Valley, California,\nnear the site of the 1960 Winter Olympics,\nand ends 100.2 miles later in Auburn, California.\nIn the decades since its inception in\n1974, Western States has come to represent\none of the ultimate endurance tests in the\nworld.",
             x=0.05, y=0.775, hjust=0, fontfamily="Segoe UI", size=12, colour = "#4e250c") +
  draw_label(label=  "By looking at the median finish times, we can determine which year* was the hardest\n(and the easiest). The slowest year was 1995 for men, and 1998 for women. The fastest\nyear was 1984 for men and 1985 for women.\n\n*Since 1980.",
             x=0.05, y=0.54, hjust=0, fontfamily="Segoe UI", size=12, colour = "#4e250c") +
  draw_label(label=  "Jim Walmsley set the male course record of 14:09:28 in 2019. The female course record has stood\nat 16:47:19 since 2012, run by Ellie Greenwood. The first female runner completed the race in 1978.\nSince then, the proportion of female finishers has been slowly increasing, but is still less than 25%.\n",
             x=0.5, y=0.87, hjust=0, fontfamily="Segoe UI", size=12, colour = "#4e250c") +
  draw_label(label=  "There is little difference in the ages of male and female finishers, though female finishers tend\nto be slightly younger. The average female finisher is 40.2 years old, and the average male is\n42.5 years old. There is some correlation between age and finish time, with faster finishing\ntimes more likely to come from younger athletes.",
             x=0.5, y=0.42, hjust=0, fontfamily="Segoe UI", size=12, colour = "#4e250c") +
  #caption
  draw_label(label="N. Rennie | Data: www.wser.org", x=0.05, y=0.03, hjust=0, fontfamily="Segoe UI", size=12, colour = "#4e250c") +
  #add lines
  draw_line(x = c(0.53, 0.95), y = c(0.51, 0.51), color = "#8a5f3c", size = 0.5) +
  draw_line(x = c(0.05, 0.47), y = c(0.63, 0.63), color = "#8a5f3c", size = 0.5)
  #add facts
q


#save viz  
ggsave(q, filename="Western States Analysis/viz.jpg", width=16.5, height=11.7, units="in")
ggsave(q, filename="Western States Analysis/viz.pdf", width=16.5, height=11.7, units="in", device=cairo_pdf)

