# Mini chart --------------------------------------------------------------

g_legend <- ggplot() +
  geom_rect(
    data = filter(plot_data, station == "Leuchars", month == "June"),
    mapping = aes(
      xmin = year - 0.5,
      xmax = year + 0.5,
      ymin = min_temp,
      ymax = max_temp,
    ),
    fill = text_col,
    alpha = 0.3
  ) +
  geom_step(
    data = filter(plot_data, station == "Leuchars", month == "June"),
    mapping = aes(
      x = year - 0.5,
      y = max_temp,
    ),
    colour = "#B2182B"
  ) +
  geom_step(
    data = filter(plot_data, station == "Leuchars", month == "June"),
    mapping = aes(
      x = year - 0.5,
      y = min_temp,
    ),
    colour = "#2166AC"
  ) +
  # records
  geom_point(
    data = filter(min_data, station == "Leuchars", month == "June"),
    mapping = aes(
      x = year, y = min_temp
    ),
    pch = 25,
    size = 4,
    colour = text_col,
    fill = "#2166AC"
  ) +
  geom_label(
    data = min_data |>
      filter(station == "Leuchars", month == "June") |>
      slice_max(year),
    mapping = aes(
      x = year, y = min_temp - 1.25,
      label = paste0(min_temp, "°C")
    ),
    fill = bg_col,
    vjust = 1,
    border.colour = bg_col,
    family = body_font,
    size = 5,
    colour = "#2166AC",
  ) +
  geom_point(
    data = filter(max_data, station == "Leuchars", month == "June"),
    mapping = aes(
      x = year, y = max_temp
    ),
    pch = 24,
    size = 4,
    colour = text_col,
    fill = "#B2182B"
  ) +
  geom_label(
    data = max_data |>
      filter(station == "Leuchars", month == "June") |>
      slice_max(year),
    mapping = aes(
      x = year, y = max_temp + 1.25,
      label = paste0(max_temp, "°C")
    ),
    fill = bg_col,
    vjust = 0,
    border.colour = bg_col,
    family = body_font,
    size = 5,
    colour = "#B2182B",
  ) +
  facet_wrap(~station,
    ncol = 1, axes = "all_x",
    strip.position = "top"
  ) +
  labs(x = NULL, y = NULL, title = "How to read these charts") +
  scale_x_continuous(
    limits = range(plot_data$year) + 0.5,
    expand = expansion(0, c(0, 1))
  ) +
  scale_y_continuous(
    limits = c(min(plot_data$min_temp), max(plot_data$max_temp)),
    expand = expansion(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 18, base_family = body_font) +
  theme(
    plot.margin = margin(5, 5, 10, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 160, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.5)
    ),
    strip.text.x.top = element_text(
      face = "bold",
      hjust = 0,
      margin = margin(t = 3, r = 3, l = 3, b = 3),
      size = rel(1)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )


# Add annotations ---------------------------------------------------------

g_legend2 <- ggdraw(g_legend) +
  draw_text(
    x = 0.95, y = 0.72,
    size = 15,
    hjust = 1,
    colour = text_col,
    family = body_font,
    lineheight = 0.8,
    text = str_wrap("Highest average miaxmum temperature recorded.", 15)
  ) +
  draw_text(
    x = 0.55, y = 0.65,
    size = 15,
    hjust = 1,
    colour = text_col,
    family = body_font,
    lineheight = 0.8,
    text = str_wrap("Average daily maximum temperature per month.", 17)
  ) +
  draw_text(
    x = 0.69, y = 0.15,
    size = 15,
    hjust = 1,
    colour = text_col,
    family = body_font,
    lineheight = 0.8,
    text = str_wrap("Lowest average minimum temperature recorded.", 18)
  ) +
  draw_text(
    x = 0.95, y = 0.19,
    size = 15,
    hjust = 1,
    colour = text_col,
    family = body_font,
    lineheight = 0.8,
    text = str_wrap("Average daily minimum temperature per month.", 15)
  ) +
  draw_text(
    x = 0.05, y = 0.8,
    size = 15,
    hjust = 0,
    colour = text_col,
    family = body_font,
    lineheight = 0.8,
    text = str_wrap("Location of weather station.", 15)
  ) +
  draw_text(
    x = 0.5, y = 0.4,
    size = 15,
    hjust = 1,
    colour = text_col,
    family = body_font,
    lineheight = 0.8,
    text = str_wrap("Average daily temperature range per month.", 15)
  )


# Add arrows --------------------------------------------------------------

g_legend3 <- g_legend2 +
  # Max arrow
  draw_grob(
    curveGrob(
      x1 = 0.87, y1 = 0.62,
      x2 = 0.93, y2 = 0.57,
      curvature = 0.3,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.53, y1 = 0.57,
      x2 = 0.57, y2 = 0.52,
      curvature = 0.3,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  ) +
  # Min arrow
  draw_grob(
    curveGrob(
      x1 = 0.62, y1 = 0.21,
      x2 = 0.65, y2 = 0.25,
      curvature = 0.3,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.95, y1 = 0.26,
      x2 = 0.98, y2 = 0.35,
      curvature = 0.3,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  ) +
  # Location
  draw_grob(
    curveGrob(
      x1 = 0.09, y1 = 0.73,
      x2 = 0.09, y2 = 0.65,
      curvature = 0,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  ) +
  # Range
  draw_grob(
    curveGrob(
      x1 = 0.51, y1 = 0.39,
      x2 = 0.55, y2 = 0.39,
      curvature = 0.0,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  )

# Make interactive --------------------------------------------------------

girafe(
  ggobj = g_legend3,
  options = list(
    opts_toolbar(hidden = c("saveaspng", "fullscreen"), saveaspng = FALSE)
  )
)
