# Load packages -----------------------------------------------------------

library(readxl)
library(sf)
library(terra)
library(tidyterra)
library(tidyverse)
library(spatstat.geom)
library(spatstat.explore)
library(ggtext)
library(nrBrand)
library(ggnewscale)
library(showtext)


# Load fonts --------------------------------------------------------------

font_add_google("Rye", "rye")
font_add_google("Quattrocento", "quattrocento")
showtext_auto()

# Start recording ---------------------------------------------------------

camcorder::gg_record(
  dir = file.path("recording"),
  device = "png",
  width = 6,
  height = 8,
  units = "in",
  dpi = 300
)

# Load data ---------------------------------------------------------------

cases <- read_xlsx("John Snow Cholera Maps/data/John_Snow_1854_cases.xlsx")
pumps <- read_sf("John Snow Cholera Maps/data/SnowGIS/Pumps.shp")
deaths <- read_sf("John Snow Cholera Maps/data/SnowGIS/Cholera_Deaths.shp")
OSMap <- rast("John Snow Cholera Maps/data/SnowGIS/OSMap_Grayscale.tif")
OSMap_ovr <- rast("John Snow Cholera Maps/data/SnowGIS/OSMap_Grayscale.tif.ovr")


# Data wrangling ----------------------------------------------------------

deaths <- cbind(deaths, st_coordinates(deaths))


# Create a smooth raster file ---------------------------------------------

bb <- st_bbox(OSMap)
obs_window <- owin(bb[c(1, 3)], bb[c(2, 4)])
ppp_deaths <- ppp(deaths$X,
  deaths$Y,
  marks = deaths$Count,
  window = obs_window
)
idw_deaths <- idw(ppp_deaths, power = 0.05, at = "pixels")
sp_idw_deaths <- as.data.frame.im(idw_deaths) |> as_tibble()
obj_raster <- rast(sp_idw_deaths)


# Plot --------------------------------------------------------------------

# colours
bg_col <- "#ffffec"
text_col <- "#471904"
highlight_col <- "#ec7014"

# text
title <- "1854 Broad Street Cholera Outbreak"
st <- "John Snow’s iconic 1854 map of the clustering of cholera deaths in 
Soho London is one of the best and earliest uses of epidemiology and 
geospatial analysis to investigate an outbreak. It led to the identification 
of the probable infection source – a water pump in Broad Street. This remake of 
the classic chart uses ...<br>"
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = "quattrocento"
)
cap <- paste0(
  "**Data**: On the Mode of Communication of Cholera. Snow, John. 1855. <br>**Graphic**: ", social
)

# set colour limits
lower <- round(minmax(obj_raster)[1], 3) - 0.001
upper <- round(minmax(obj_raster)[2], 3) + 0.001

# plot bg street map
bg_map <- ggplot() +
  geom_spatraster(data = OSMap$OSMap_Grayscale, alpha = 0.7) +
  scale_fill_gradient(low = text_col, high = bg_col, guide = "none")

# plot smoothed raster file
smooth_map <- bg_map +
  new_scale_fill() +
  geom_spatraster(data = obj_raster, mapping = aes(alpha = after_stat(value))) +
  scale_alpha_continuous(guide = "none", range = c(0.5, 1)) +
  scale_fill_distiller(palette = "YlOrBr",
                       direction = 1,
                       name = "",
                       limits = c(lower, upper),
                       breaks = c(lower + 0.0015, upper - 0.0015),
                       labels = c("Fewer deaths", "More deaths"),
                       guide = guide_colourbar(
                         barwidth = 26,
                         title.position = "top",
                         frame.colour = text_col, 
                         label.hjust = 0.5,
                         barheight = 0.5, 
                         ticks = FALSE)) 

# add pump locations as points (and radius)
pumps_map <- smooth_map +
  geom_sf(data = pumps, size = 2, colour = text_col) +
  geom_sf(data = pumps, size = 8, pch = 21, colour = text_col)

# styling
pumps_map +
  labs(title = title, 
       subtitle = st, 
       caption = cap) +
  theme_void(base_size = 28, base_family = "quattrocento") +
  coord_sf(expand = FALSE) +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = bg_col, colour = bg_col),
        panel.background = element_rect(fill = bg_col, colour = bg_col),
        legend.title = element_text(margin = margin(t = -20)),
        legend.text = element_text(margin = margin(t = -5, b = 10)),
        plot.title = element_textbox_simple(
          colour = text_col,
          face = "bold",
          family = "rye",
          lineheight = 0.5,
          size = 54,
          margin = margin(b = 20)
        ),
        plot.subtitle = element_textbox_simple(
          colour = text_col,
          halign = 0,
          hjust = 0,
          lineheight = 0.5,
          family = "quattrocento",
          margin = margin(b = 20)
        ),
        plot.caption = element_textbox_simple(
          colour = text_col,
          lineheight = 0.5,
          family = "quattrocento",
          margin = margin(t = 10)
        ),
        plot.margin = margin(10, 20, 10, 20))


# Save high resolution image ----------------------------------------------

ggsave("John Snow Cholera Maps/images/john-snow-cholera-map-nrennie.png",
       height = 8,
       width = 6,
       unit = "in")


#TODO
#look at cases data - check data (seems incorrect)
#finish subtitle
#add some lines/boxes to make it look old
#add a zoomed in section
