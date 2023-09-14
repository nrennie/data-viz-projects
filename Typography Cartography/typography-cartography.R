# Inspired by https://charts.substack.com/p/typewriter-chartography


# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(nrBrand)
library(showtext)


# Load fonts --------------------------------------------------------------

font_add_google("Special Elite", "elite")
showtext_auto()


# Set parameters ----------------------------------------------------------

bg_col = "#fafafa"
text_col = "grey10"
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = "elite"
)


# Data --------------------------------------------------------------------

# Load data from https://geoportal.statistics.gov.uk/datasets/ons::counties-and-unitary-authorities-april-2019-boundaries-uk-bgc/explore?location=57.255588%2C-5.251146%2C8.00
uk <- sf::st_read("../30DayMapChallenge/2022/data/UK/CTRY_DEC_2021_UK_BUC.shp") 
scot_sf <- uk %>% 
  select(CTRY21NM, geometry) %>% 
  filter(CTRY21NM == "Scotland")

# Get elevation data
elev_data <- elevatr::get_elev_raster(locations = scot_sf,
                                      z = 3,
                                      clip = "locations")
elev_mat <- terra::as.matrix(elev_data, wide = TRUE)
colnames(elev_mat) <- 1:ncol(elev_mat)
elev_df <- elev_mat |> 
  as_tibble() |> 
  mutate(y = row_number()) |> 
  pivot_longer(-y, names_to = "x") |> 
  mutate(x = as.numeric(x))
elev_plot <- elev_df |> 
  mutate(value = ntile(value, n = 4)) |> 
  mutate(value_letter = case_when(
    is.na(value) ~ "",
    value == 1 ~ "l",
    value == 2 ~ "I",
    value == 3 ~ "H",
    value == 4 ~ "M"
  ))


# Plot --------------------------------------------------------------------

ggplot() +
  geom_text(data = elev_plot, 
            mapping = aes(x = x, y = y, label = value_letter),
            family = "elite",
            colour = text_col,
            size = 4) +
  labs(title = "SCOTLAND",
       caption = social) +
  scale_y_reverse() +
  coord_fixed() +
  theme_void(base_size = 22) +
  theme(plot.background = element_rect(fill = bg_col, colour = bg_col),
        panel.background = element_rect(fill = bg_col, colour = bg_col),
        plot.margin = margin(10, 10, 10, 10),
        plot.title = element_text(family = "elite",
                                  size = 65,
                                  face = "bold",
                                  colour = text_col,
                                  margin = margin(t = 20, b = -20)),
        plot.caption = element_textbox_simple(
          colour = text_col,
          lineheight = 0.5,
          family = "elite",
          halign = 0.5,
          hjust = 0.5,
          margin = margin(b = 5, t = 15)
        ))


# Save plot ---------------------------------------------------------------

ggsave("Typography Cartography/images/typography-cartography.png",
       height = 6,
       width = 4,
       bg = bg_col,
       unit = "in")
