library(tidyverse)

# Data from HADCET
# See 2022 30DayChartChallenge data
df <- as_tibble(read.table("2022/data/hadcet.txt", header = F))
colnames(df) <- c("year",
                  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
                  "avg")
plot_data <- df |> 
  mutate(decade = 10*floor(year/10)) |> 
  pivot_longer(cols = `Jan`:`Dec`, 
               names_to = "month",
               values_to = "temp") |> 
  select(decade, month, temp) |> 
  filter(decade %in% seq(1920, 2010, by = 10)) |> 
  group_by(decade, month) |> 
  summarise(temp = mean(temp)) |> 
  mutate(month = factor(month,
                        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
         decade = factor(decade)) |> 
  ungroup() |> 
  mutate(temp_cut = cut(temp, breaks = 5))

# plot
ggplot() +
  geom_tile(data = plot_data,
            mapping = aes(month, decade, fill = temp_cut),
            colour = "#bbbbbd",
            linewidth = 1.5) +
  scale_y_discrete(limits=rev) +
  scale_fill_manual(values = c("#358681", "#7fa69f", "#c8d8d7", "#b83b60", "#974b73"),
                    labels = c("1062\nTeal\n", "1725\nSage\n", "1820\nDuck\negg", "1023\nRasp-\nberry", "1061\nPlum\n")) +
  guides(fill = guide_legend(label.position = "bottom",
                             title.position = "top",
                             title = "Temperature")) +
  labs(x = "",
       y = "",
       title = "HADCET Temperature Blanket Pattern",
       subtitle = "Wool: Stylecraft Special Aran 100g\nBorder: 1203 Silver") +
  coord_fixed() +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = unit(1.2, "cm"),
        legend.key.height = unit(1.2, "cm"),
        plot.title.position = "plot")
