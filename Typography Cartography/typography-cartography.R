# Inspired by https://charts.substack.com/p/typewriter-chartography


# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggtext)
library(nrBrand)
library(showtext)


# Load fonts --------------------------------------------------------------

font_add_google("Special Elite", "elite")
showtext_auto()


# Function to format data and plot ----------------------------------------

make_map <- function(elev_data,
                     title,
                     size = 4,
                     text_size = 22,
                     bg_col = "#fafafa",
                     text_col = "grey10",
                     caption = TRUE) {
  # prep data
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
  # plot
  g <- ggplot() +
    geom_text(data = elev_plot, 
              mapping = aes(x = x, y = y, label = value_letter),
              family = "elite",
              colour = text_col,
              size = size) +
    labs(title = title) +
    scale_y_reverse() +
    coord_fixed() +
    theme_void(base_size = text_size) +
    theme(plot.background = element_rect(fill = bg_col, colour = bg_col),
          panel.background = element_rect(fill = bg_col, colour = bg_col),
          plot.margin = margin(10, 10, 10, 10),
          plot.title = element_text(family = "elite",
                                    size = text_size*3,
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
  if (caption) {
    social <- nrBrand::social_caption(
      bg_colour = bg_col,
      icon_colour = text_col,
      font_colour = text_col,
      font_family = "elite"
    )
    g <- g + labs(caption = social)
  }
  return(g)
}


# Plot Scotland -----------------------------------------------------------

# Load data from https://geoportal.statistics.gov.uk/datasets/ons::counties-and-unitary-authorities-april-2019-boundaries-uk-bgc/explore?location=57.255588%2C-5.251146%2C8.00
# Get elevation data
uk <- sf::st_read("../30DayMapChallenge/2022/data/UK/CTRY_DEC_2021_UK_BUC.shp") 
scot_sf <- uk %>% 
  select(CTRY21NM, geometry) %>% 
  filter(CTRY21NM == "Scotland")
elev_data <- elevatr::get_elev_raster(locations = scot_sf,
                                      z = 3,
                                      clip = "locations")
# create map and save
make_map(elev_data, title = "SCOTLAND")
ggsave("Typography Cartography/images/typography-cartography.png",
       height = 6,
       width = 4,
       bg = "#FAFAFA",
       unit = "in")

# create larger version
make_map(elev_data, title = "  SCOTLAND",
         size = 18, text_size = 90,
         bg_col = "white", text_col = "black",
         caption = FALSE) +
  theme(plot.title = element_text(margin = margin(t = 100, b = -140)))
ggsave("Typography Cartography/images/typography-cartography-scotland-big.png",
       width = 15.5,
       height = 23.5,
       bg = "white",
       unit = "in")

# Plot England ------------------------------------------------------------

eng_sf <- uk %>% 
  select(CTRY21NM, geometry) %>% 
  filter(CTRY21NM == "England")
elev_data <- elevatr::get_elev_raster(locations = eng_sf,
                                      z = 3,
                                      clip = "locations")
# create map and save
make_map(elev_data, title = "ENGLAND", 
         size = 6, text_size = 28,
         bg_col = "grey20", text_col = "#fafafa",
         caption = FALSE)
ggsave("Typography Cartography/images/typography-cartography-england.png",
       height = 7,
       width = 7,
       bg = "grey20",
       unit = "in")
