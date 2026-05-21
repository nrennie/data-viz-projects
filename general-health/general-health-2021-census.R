# load helper functions
source("utils.R")

# read in data
census <- readxl::read_xlsx("Data/Census2021-health-la.xlsx", skip = 4)

# get local authority map shapefiles
la_map <- sf::st_read("Data/Local_Authority_Districts_December_2021/LAD_DEC_2021_GB_BGC.shp") 

# calculate percentage that have bad/very bad
bad_health <- census |> 
  mutate(Percentage = `Bad health \r\n(age-standardised \r\nproportions)` + `Very bad health \r\n(age-standardised \r\nproportions)`) |> 
  select(`Area code`, Percentage) 

# highest / lowest percentage
slice_max(bad_health, Percentage, n = 1)
slice_min(bad_health, Percentage, n = 1)
round(mean(bad_health$Percentage), 1)

# combine
map_data <- la_map |> 
  filter(stringr::str_starts(LAD21CD, "E") | stringr::str_starts(LAD21CD, "W")) |> 
  left_join(bad_health, by = c("LAD21CD" = "Area code"))

# text
social <- "<span style='font-family:\"Font Awesome 6 Brands\";color:black;'>&#xf099;</span><span style='color:white;'>.</span><span style='font-family:Roboto;color:black;'>@nrennie35</span><span style='color:white;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:black;'>&#xf4f6;</span><span style='color:white;'>.</span><span style='font-family:Roboto;color:black;'>fosstodon.org/@nrennie</span><span style='color:white;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:black;'>&#xf09b;</span><span style='color:white;'>.</span><span style='font-family:Roboto;color:black;'>nrennie</span><span style='color:white;'>..</span>"
cap <- paste0("**Data**: Office for National Statistics (ONS). General health, England and Wales: Census 2021.<br>**Graphic**: ", social)

st <- "Census 2021 respondents were asked to rate their general health as being either *very good*, *good*, *fair*, *bad*, or *very bad*. The map below shows the (age-standardised) percentage of respondents in each local authority who rated their health as being bad or very bad. Tower Hamlets and Merthyr Tydfil had the joint highest percentage of people reporting bad or very bad health at 9.5%. The average across all local authorities is 5.3%."

# plot
ggplot() +
  geom_sf(data = map_data, mapping = aes(fill = Percentage), colour = "black", linewidth = 0.1) +
  labs(title = "Percentage of people reporting bad or very bad general health in the 2021 Census", subtitle = st, caption = cap) +
  scale_fill_carto_c(palette = "BurgYl", limits = c(2, 10)) +
  guides(fill = guide_colorbar(title = "Percentage reporting bad or very bad health", title.position = "top", title.hjust = 0.5, barwidth = 15, barheight = 0.7)) +
  theme_map(base_size = 18)

# save
ggsave("Images/general-health-2021-census-a.png", width = 4, height = 6, units = "in")
