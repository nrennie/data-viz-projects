# packages and fonts
library(ggplot2)
library(showtext)
font_add_google("Commissioner")
showtext_auto()

# parameters
xmin = -2.756090527934968
xmax = -2.2810873492206873
ymin = 53.866138460195074
ymax = 54.13730291044705
res = 50
col_palette = MetBrewer::met.brewer("Morgenstern")
text_family = "Commissioner"
bg_col = "#ffc680"
box_col = "#FFE5AD"
text_size = 20
text_col = "#574156"
title = "Forest of Bowland"
subtitle = "Maximum Elevation: 560m"

# get data
elev_data <- suppressMessages(elevatr::get_elev_raster(
  locations = data.frame(x = c(xmin, xmax), y = c(ymin, ymax)),
  z = 10,
  prj = "EPSG:4326",
  clip = "locations"))
elev_terra <- terra::rast(elev_data)

# get colours
break_vals <- seq(floor(elev_data@data@min/res)*res, ceiling(elev_data@data@max/res)*res, by = res)
cols <- grDevices::colorRampPalette(col_palette)(length(break_vals))

# caption
social <- nrBrand::social_caption(
  bg_colour = box_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = "Commissioner"
)

# plot
ggplot() +
  tidyterra::geom_spatraster_contour_filled(data = elev_terra,
                                            breaks = break_vals) +
  tidyterra::geom_spatraster_contour(data = elev_terra,
                                            breaks = break_vals,
                                     colour = text_col) +
  ggplot2::geom_text(
    data = data.frame(c()),
    mapping = aes(x = elev_data@extent[1],
                  y = elev_data@extent[4] + 0.06,
                  label = toupper(title)),
    size = text_size * 1.5,
    hjust = 0,
    fontface = "bold",
    colour = text_col,
    family = text_family
  ) +
  ggplot2::geom_text(
    data = data.frame(c()),
    mapping = aes(x = elev_data@extent[1],
                  y = elev_data@extent[4] + 0.02,
                  label = subtitle),
    size = text_size,
    hjust = 0,
    colour = text_col,
    family = text_family
  ) +
  ggtext::geom_richtext(
    data = data.frame(c()),
    mapping = aes(x = elev_data@extent[1],
                  y = elev_data@extent[3] - 0.02,
                  label = social),
    size = text_size * 0.7,
    fill = box_col, 
    colour = box_col,
    hjust = 0,
    text.colour = text_col,
    family = text_family
  ) +
  scale_fill_manual(values = cols) +
  ggnewscale::new_scale_fill() +
  ggplot2::geom_rect(
    data = data.frame(x = elev_data@extent[1] + seq(0, by = 0.025, length.out = length(col_palette)),
                      y = rep(elev_data@extent[4] + 0.10, length(col_palette)),
                      fill = col_palette,
                      z = seq(min(break_vals), max(break_vals), length.out = length(col_palette))),
    mapping = aes(xmin = x, ymin = y, xmax = x + 0.02, ymax = y + 0.01, fill = fill),
  ) +
  scale_fill_identity() +
  ggplot2::theme_void(base_size = text_size, base_family = text_family) +
  ggplot2::theme(legend.position = "none",
                 plot.margin = ggplot2::margin(20, 20, 20, 20),
                 plot.title = element_text(margin = margin(b = 10), face = "bold"),
                 plot.subtitle = element_text(margin = margin(b = 10)),
                 plot.background = element_rect(fill = bg_col, colour = bg_col),
                 panel.background = element_rect(fill = box_col, colour = box_col)) 

# save
ggsave("Forest of Bowland/images/forest-of-bowland.png",
       height = 297,
       width = 210,
       bg = bg_col,
       unit = "mm")
