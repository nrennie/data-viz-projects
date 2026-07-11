g_chart <- ggplot() +
  geom_rect_interactive(
    data = plot_data,
    mapping = aes(
      xmin = year - 0.5,
      xmax = year + 0.5,
      ymin = min_temp,
      ymax = max_temp, data_id = month
    ),
    fill = text_col,
    alpha = 0.3
  ) +
  geom_step_interactive(
    data = plot_data,
    mapping = aes(
      x = year - 0.5,
      y = max_temp, data_id = month
    ),
    colour = "#B2182B"
  ) +
  geom_step_interactive(
    data = plot_data,
    mapping = aes(
      x = year - 0.5,
      y = min_temp, data_id = month
    ),
    colour = "#2166AC"
  ) +
  # records
  geom_point_interactive(
    data = min_data,
    mapping = aes(
      x = year, y = min_temp, data_id = month
    ),
    pch = 25,
    size = 2,
    colour = text_col,
    fill = "#2166AC"
  ) +
  geom_label_interactive(
    data = min_data |>
      group_by(station, month) |>
      slice_max(year) |>
      ungroup(),
    mapping = aes(
      x = year, y = min_temp - 1.35,
      label = paste0(min_temp, "°C"), data_id = month
    ),
    fill = bg_col,
    border.colour = bg_col,
    family = body_font,
    size = 3,
    vjust = 1,
    colour = "#2166AC",
  ) +
  geom_point_interactive(
    data = max_data,
    mapping = aes(
      x = year, y = max_temp, data_id = month
    ),
    pch = 24,
    size = 2,
    colour = text_col,
    fill = "#B2182B"
  ) +
  geom_label_interactive(
    data = max_data |>
      group_by(station, month) |>
      slice_max(year) |>
      ungroup(),
    mapping = aes(
      x = year, y = max_temp + 1.35,
      label = paste0(max_temp, "°C"), data_id = month
    ),
    vjust = 0,
    fill = bg_col,
    border.colour = bg_col,
    family = body_font,
    size = 3,
    colour = "#B2182B",
  ) +
  facet_wrap(~station,
    ncol = 2,
    strip.position = "top"
  ) +
  labs(x = NULL, y = NULL) +
  scale_x_continuous(expand = expansion(0, c(0, 1))) +
  scale_y_continuous(expand = expansion(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12, base_family = body_font) +
  theme(
    plot.margin = margin(5, 10, 5, 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    strip.text.x.top = element_text(
      face = "bold",
      hjust = 0,
      margin = margin(t = 3, r = 3, l = 3, b = 3),
      size = rel(1.2)
    ),
    strip.clip = "off",
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(0, "lines"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
