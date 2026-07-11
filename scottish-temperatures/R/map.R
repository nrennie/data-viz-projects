

# Make map ----------------------------------------------------------------

g_map <- ggplot() +
  geom_sf(
    data = scot_map,
    colour = text_col,
    fill = text_col,
    alpha = 0.2
  ) +
  geom_label(
    data = metadata,
    mapping = aes(
      x = lon, y = lat, label = station
    ),
    family = body_font,
    size = 6,
    hjust = 0,
    vjust = -0.3,
    fill = alpha(bg_col, 0.8),
    border.colour = "transparent",
    colour = text_col
  ) +
  geom_point(
    data = metadata,
    mapping = aes(
      x = lon, y = lat
    ),
    fill = bg_col,
    colour = bg_col,
    pch = 21,
    size = 7
  ) +
  geom_point(
    data = metadata,
    mapping = aes(
      x = lon, y = lat
    ),
    alpha = 0.5,
    pch = 21,
    size = 7,
    fill = text_col,
    colour = text_col
  ) +
  geom_point(
    data = metadata,
    mapping = aes(
      x = lon, y = lat
    ),
    size = 2,
    colour = text_col
  ) +
  scale_x_continuous(limits = c(-8, NA)) +
  coord_sf(expand = FALSE, clip = "off") +
  theme_void(base_size = 10, base_family = body_font) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
  )


# Make interactive --------------------------------------------------------

girafe(
  ggobj = g_map,
  bg = bg_col,
  options = list(
    opts_toolbar(hidden = c("saveaspng", "fullscreen"), saveaspng = FALSE)
  )
)