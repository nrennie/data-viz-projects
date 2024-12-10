library(tidyverse)
library(sf)

country <- "scotland"
col_palette <- PrettyCols::prettycols("Celestial")
bg_col <- "#FAFAFA"
s <- 1234

set.seed(s)
country_sf <- rnaturalearth::ne_states(
  geounit = country
)
country_coords <- map_df(
  .x = country_sf$geometry,
  .f = ~ st_bbox(.x)
) |>
  mutate(
    across(everything(), as.numeric)
  ) |>
  mutate(
    id = row_number(),
    fill = sample(grDevices::colorRampPalette(col_palette)(nrow(country_sf)))
  )
ggplot() +
  geom_rect(
    data = country_coords,
    mapping = aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      group = id,
      fill = fill,
      colour = fill
    ),
    alpha = 0.5
  ) +
  scale_fill_identity() +
  scale_colour_identity() +
  coord_sf(crs = 4326) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    )
  )

ggsave(
  filename = glue::glue("Regions in Boxes/images/{country}.png"),
  height = 5, 
  width = 5,
  bg = bg_col
)

# TODO: add country title in boxes
# TODO: Add white boxes underneath (T/F)
