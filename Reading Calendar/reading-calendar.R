library(tidyverse)
library(ggtext)
library(showtext)
library(PrettyCols)
library(glue)

# load data
books <- readr::read_csv("Reading Calendar/data/storygraph.csv") |>
  janitor::clean_names()

# data wrangling
books_data <- books |>
  select(dates_read, read_status) |>
  filter(read_status == "read") |>
  separate_wider_delim(dates_read,
    delim = "-", names = c("start_date", "end_date"),
    too_few = "align_start"
  ) |>
  drop_na() |>
  mutate(
    start_date = ymd(start_date),
    end_date = ymd(end_date),
    start_year = year(start_date),
    end_year = year(end_date)
  ) |>
  filter(
    start_year %in% 2021:2024,
    end_year %in% 2021:2024
  ) |>
  mutate(
    start_date = ymd(glue("1900/{month(start_date)}/{mday(start_date)}")),
    end_date = ymd(glue("1900/{month(end_date)}/{mday(end_date)}")),
    col_start_year = as.character(start_year)
  )

multi_year1 <- books_data |>
  filter(
    start_year != end_year
  ) |>
  mutate(
    end_date = ymd("19001231")
  )

multi_year2 <- books_data |>
  filter(
    start_year != end_year
  ) |>
  mutate(
    start_date = ymd("19000101"),
    start_year = start_year + 1
  )

plot_data <- books_data |>
  filter(
    start_year == end_year
  ) |>
  rbind(multi_year1) |>
  rbind(multi_year2) |>
  mutate(
    start_year = as.character(start_year),
    end_year = as.character(end_year)
  )

# colours
bg_col <- "#001E29"

# plot
ggplot() +
  geom_rect(
    data = plot_data,
    mapping = aes(
      xmin = start_date,
      xmax = end_date,
      ymin = 0,
      ymax = 1,
      fill = col_start_year,
      colour = col_start_year
    ),
    alpha = 0.6,
    linewidth = 0.5
  ) +
  scale_x_date(
    limits = ymd(c("19000101", "19001231"))
  ) +
  scale_fill_pretty_d(palette = "Celestial") +
  scale_colour_pretty_d(palette = "Celestial") +
  facet_wrap(~start_year, ncol = 1) +
  coord_cartesian(expand = FALSE) +
  theme_void() +
  theme(
    strip.text = element_blank(),
    legend.position = "none",
    plot.caption.position = "plot",
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    plot.margin = margin(10, 10, 10, 10)
  )

# Save
ggsave("Reading Calendar/images/reading-calendar.png", width = 5, height = 5, units = "in")
