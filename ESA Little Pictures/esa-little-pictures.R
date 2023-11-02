# Load data ---------------------------------------------------------------

sst_data <- readr::read_csv("https://raw.githubusercontent.com/littlepictures/datasets/main/sst/monthly_global_sst_mean.csv")


# Data wrangling ----------------------------------------------------------

plot_data <- sst_data |>
  dplyr::rename(
    sst = `Sea Surface Temperature`,
    date = month
  ) |>
  tidyr::separate_wider_delim(date, "/",
    names = c("month", "year"),
    cols_remove = FALSE
  ) |>
  dplyr::mutate(across(month:year, as.numeric),
    date = paste0("01/", date),
    date = lubridate::dmy(date)
  ) |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    min_sst = min(sst),
    max_sst = max(sst)
  )


# Plot --------------------------------------------------------------------

ggplot2::ggplot(
  data = plot_data,
) +
  ggplot2::geom_ribbon(
    mapping = ggplot2::aes(x = year, ymax = min_sst, ymin = max_sst)
  ) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(0, 0)) +
  ggplot2::theme_void() +
  ggplot2::theme(plot.background = ggplot2::element_rect(
    fill = "white", colour = "white"
  ))


# Export for Inkscape  ----------------------------------------------------

ggplot2::ggsave(
  filename = "ESA Little Pictures/esa-little-pictures.pdf",
  width = 6,
  height = 6
)
