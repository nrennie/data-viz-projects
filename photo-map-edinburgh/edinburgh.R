library(ggspatial)
library(sf)
library(patchwork)
library(ggplot2)
library(ggtext)
library(nrBrand)
library(showtext)

font_add_google("Ubuntu")
showtext_auto()
showtext_opts(dpi = 300)

rescale <- 5
overlay_col <- "black"
text_col <- "white"


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = "transparent",
  icon_colour = text_col,
  font_colour = text_col,
  font_family = "Ubuntu",
  mastodon = NA,
  linkedin = NA,
  bluesky = NA
)
cap <- paste0("**Map**: OpenMapTiles | **Image**: ", social)


# Map ---------------------------------------------------------------------

bbx <- matrix(
  c(
    -3.2067627157441545, -3.1805566117589366,
    55.94639430594511, 55.9550161967448
  ),
  byrow = TRUE,
  ncol = 2
)
rownames(bbx) <- c("x", "y")
colnames(bbx) <- c("min", "max")

poly <- rbind(
  c(bbx[1, 1], bbx[2, 1]),
  c(bbx[1, 2], bbx[2, 1]),
  c(bbx[1, 2], bbx[2, 2]),
  c(bbx[1, 1], bbx[2, 2]),
  c(bbx[1, 1], bbx[2, 1])
)

sf_obj <- st_as_sf(st_sfc(st_polygon(list(poly)), crs = 4326))

main_plot <- ggplot(data = sf_obj) +
  geom_sf(fill = "transparent") +
  annotation_map_tile(zoom = 15, type = "cartolight", alpha = 1) +
  coord_sf(expand = FALSE) +
  theme_void()

main_plot

# sizing for image
sf_proj <- st_transform(sf_obj, crs = 3857)
bb <- st_bbox(sf_proj)
ratio <- (bb["xmax"] - bb["xmin"]) / (bb["ymax"] - bb["ymin"])
width_in <- 6
height_in <- width_in / ratio


# Image -------------------------------------------------------------------

img <- imager::load.image("Map of Edinburgh/photo.jpg")
img <- imager::resize(img, round(imager::width(img) / rescale), round(imager::height(img) / rescale))
img <- imager::grayscale(imager::rm.alpha(img))
m <- as.matrix(img)
colnames(m) <- seq_len(ncol(m))
rownames(m) <- seq_len(nrow(m))

img_ratio <- imager::width(img) / imager::height(img)

m_df <- m |>
  tibble::as_tibble() |>
  dplyr::mutate(x = dplyr::row_number()) |>
  tidyr::pivot_longer(-.data$x, names_to = "y") |>
  dplyr::mutate(y = as.numeric(.data$y))

if (img_ratio > ratio) {
  # chop off right hand side
  plot_df <- m_df |>
    dplyr::filter(x <= ceiling(imager::height(img) * ratio))
} else if (ratio < img_ratio) {
  # chop of top
  plot_df <- m_df |>
    dplyr::filter(y <= ceiling(imager::width(img) / ratio))
}

p <- ggplot2::ggplot() +
  ggplot2::geom_raster(
    data = plot_df,
    mapping = ggplot2::aes(x = .data$x, y = .data$y, alpha = 1 - .data$value),
    fill = overlay_col
  ) +
  ggplot2::scale_alpha(range = c(0.3, 0.6)) +
  labs(tag = cap) +
  ggplot2::scale_y_reverse() +
  ggplot2::coord_fixed(expand = FALSE) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "none",
    plot.tag.position = c(0, 0),
    plot.tag = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      vjust = 0,
      valign = 0,
      margin = margin(b = 5, l = 5),
      family = "sans"
    )
  )

p


# Combine and save --------------------------------------------------------

new_p <- main_plot + inset_element(
  p, 0, 0, 1, 1,
  align_to = "plot", clip = FALSE
) &
  theme_void() +
  theme(legend.position = "none",
        plot.tag.position = c(0, 0),
        plot.tag = element_textbox_simple(
          colour = text_col,
          hjust = 0,
          halign = 0,
          vjust = 0,
          valign = 0,
          margin = margin(b = 5, l = 5),
          size = 7,
          family = "Ubuntu"
        ))

ggsave("Map of Edinburgh/images/map.png",
  width = width_in,
  height = height_in,
  unit = "in"
)
