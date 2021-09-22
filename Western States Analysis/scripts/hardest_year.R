library(readxl)
library(tidyverse)
library(extrafont)
library(patchwork)
library(lubridate)

dat <- read_excel("ws_data.xlsx") 

#decimal time
dat$dec_time <- sapply(dat$time, function(x) as.numeric(difftime(x, ymd_hms("1899-12-31 00:00:00"), units = "hour")))

#median per year for men
m_dat <- filter(dat, gender == "M" & year >= "1980") %>% group_by(year) %>% summarise(med = median(dec_time))
p1 <- ggplot() + 
  geom_bar(data=m_dat, aes_string(x="year",y="med", fill='factor(ifelse(med==max(med), 3, ifelse(med==min(med), 2, 1)))'), stat="identity", colour="transparent") + 
  scale_fill_manual("", values=c("1"='#8a5f3c', "2"='#6f50a1', "3"='#1e8f89'), labels=c("Fastest", "Slowest"), breaks=c()) + 
  labs(x="", y="Median finish time\n(hours)\n") +
  coord_cartesian(expand=F) +
  ggtitle("", subtitle="Male") +
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
        plot.margin = unit(c(0, 0, 0, 0), "cm")) #top, right, bottom, left
p1

#median per year for women
f_dat <- filter(dat, gender == "F" & year >= "1980") %>% group_by(year) %>% summarise(med = median(dec_time))
p2 <- ggplot() +
  geom_bar(data=f_dat, aes_string(x="year",y="med", fill='factor(ifelse(med==max(med), 3, ifelse(med==min(med), 2, 1)))'), stat="identity", colour="transparent") + 
  scale_fill_manual("", values=c("1"='#8a5f3c', "2"='#6f50a1', "3"='#1e8f89'), labels=c("Fastest", "Slowest"), breaks=c("2","3")) + 
  labs(x="", y="Median finish time\n(hours)\n") +
  coord_cartesian(expand=F) +
  ggtitle("", subtitle="Female") +
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
        plot.margin = unit(c(0, 0, 0, 0), "cm")) #top, right, bottom, left
p2

#join plots
p <- p1 + p2 + plot_layout(nrow=2, ncol=1) + 
  plot_annotation(title = "The Hardest Years", 
                  caption = "N. Rennie | Data: www.wser.org", 
                  subtitle="By looking at the median finish times, we can determine which year* was the hardest\n(and the easiest). The slowest year was 1995 for men, and 1998 for women. The fastest\nyear was 1984 for men and 1985 for women.\n\n*Since 1980.") &
  theme(plot.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        panel.background = element_rect(fill = "#ffdbac", colour="#ffdbac"),
        plot.title = element_text(colour = "#8a5f3c", size=28, hjust=0, family="Playbill"),
        plot.subtitle = element_text(colour = "#4e250c", size=12, hjust=0, family="Segoe UI"),
        plot.caption = element_text(colour = "#4e250c", size=12, hjust = 0, family="Segoe UI"),
        plot.margin = unit(c(0.3, 0.5, 0.3, 0.5), "cm"))
p

#save image
dev.new(width=7,height=7,unit="in", noRStudioGD = TRUE)




