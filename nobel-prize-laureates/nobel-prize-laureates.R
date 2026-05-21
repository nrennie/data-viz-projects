library(tidyverse)
library(ggtext)
library(showtext)

# load data
# data from: https://public.opendatasoft.com/explore/dataset/nobel-prize-laureates/table/
# accessed: 4 July 2023
nobel <- readr::read_delim(
  file = "Nobel Prize Laureates/nobel-prize-laureates.csv",
  delim = ";"
)

# load fonts
font_add_google("Passion One", "Passion")
font_add_google("Ubuntu", "Ubuntu")
showtext_auto()

# Plot by category --------------------------------------------------------

# make a function
nobel_plot <- function(plot_category,
                       bg_col = "grey10",
                       primary_col = "white",
                       secondary_col = "grey45",
                       data = nobel) {
  if (!(plot_category %in% unique(data$Category))) {
    stop("Invalid category")
  }
  # data processing
  nobel_data <- data |>
    select(Firstname, Surname, Year, Gender, Category) |>
    filter(
      Gender != "org",
      Category == plot_category
    ) |>
    mutate(label = paste(Firstname, Surname)) |>
    arrange(Year) |>
    select(label, Gender)
  # plot data
  plot_data <- nobel_data |>
    mutate(
      theta = seq(pi / 4, (7 / 4) * pi, length.out = nrow(nobel_data)),
      x = 5 * cos(theta),
      y = 5 * sin(theta),
      angle = 180 + 360 * (theta / (2 * pi))
    ) |>
    arrange(desc(Gender))
  # text
  social <- nrBrand::social_caption(
    bg_colour = bg_col,
    icon_colour = primary_col,
    font_colour = primary_col,
    font_family = "Ubuntu"
  )
  cap <- paste0("**Data**: public.opendatasoft.com/explore/dataset/nobel-prize-laureates<br>**Graphic**: ", social)
  # plot
  ggplot() +
    geom_text(
      data = plot_data,
      mapping = aes(
        x = x, y = y, angle = angle, label = label, colour = Gender
      ),
      hjust = 1,
      family = "Ubuntu",
      size = 3.5
    ) +
    annotate("text",
      x = 10, y = 0, hjust = 1, label = "Nobel Prize Laureates",
      colour = primary_col,
      family = "Passion",
      size = 22
    ) +
    annotate("text",
      x = 10, y = -1, hjust = 1, label = plot_category,
      colour = primary_col,
      family = "Ubuntu",
      size = 15
    ) +
    labs(caption = cap) +
    scale_x_continuous(limits = c(-9, 12)) +
    scale_y_continuous(limits = c(-7.5, 7.5)) +
    scale_colour_manual(values = c("male" = secondary_col, "female" = primary_col)) +
    coord_fixed() +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = bg_col, colour = bg_col),
      panel.background = element_rect(fill = bg_col, colour = bg_col),
      plot.caption = element_textbox_simple(
        family = "Ubuntu",
        colour = primary_col,
        hjust = 0,
        halign = 0,
        lineheight = 0.5,
        size = 13,
        margin = margin(l = 10)
      )
    )
}

# save different versions
for (i in unique(nobel$Category)) {
  p <- nobel_plot(i,
                  bg_col = "grey10",
                  primary_col = "white",
                  secondary_col = "grey45",)
  fname <- paste0("Nobel Prize Laureates/images/nobel_", i, ".png")
  ggsave(
    filename = fname,
    plot = p,
    width = 5,
    height = 4,
    unit = "in"
  )
}


# All categories ----------------------------------------------------------

nobel_plot_all <- function(
    bg_col = "grey20",
    primary_col = "white",
    secondary_col = "grey45",
    data = nobel) {
  # data wrangling
  nobel_data <- data |>
    select(Firstname, Surname, Year, Gender, Category) |>
    filter(Gender != "org") |>
    mutate(label = paste(Firstname, Surname)) |>
    arrange(Year) |>
    select(label, Gender, Category)
  # plot data
  categories <- nobel |>
    select(Category) |>
    group_by(Category) |>
    summarise(n = n()) |>
    arrange(n)
  categories$radius <- seq(from = 3, by = 3, length.out = nrow(categories))
  
  plot_data <- tibble(
    x = c(),
    y = c(),
    label = c(),
    angle = c(),
    Gender = c()
  )
  
  for (i in 1:nrow(categories)) {
    cat_data <- nobel_data %>%
      filter(Category == categories$Category[i]) %>%
      mutate(
        theta = seq(pi / 4, (7 / 4) * pi, length.out = nrow(.)),
        x = categories$radius[i] * cos(theta),
        y = categories$radius[i] * sin(theta),
        angle = 180 + 360 * (theta / (2 * pi))
      ) %>%
      arrange(desc(Gender)) |>
      select(x, y, label, angle, Gender)
    plot_data <- rbind(plot_data, cat_data)
  }
  
  # text
  social <- nrBrand::social_caption(
    bg_colour = bg_col,
    icon_colour = primary_col,
    font_colour = primary_col,
    font_family = "Ubuntu"
  )
  cap <- paste0("**Data**: public.opendatasoft.com/explore/dataset/nobel-prize-laureates<br>**Graphic**: ", social)
  
  # plot
  ggplot() +
    geom_text(
      data = plot_data,
      mapping = aes(
        x = x, y = y, angle = angle, label = str_wrap(label, 30), colour = Gender
      ),
      hjust = 1,
      family = "Ubuntu",
      lineheight = 0.2,
      size = 3.4
    ) +
    annotate("text",
             x = 0, y = 0, hjust = 0, label = "Nobel Prize Laureates",
             colour = primary_col,
             family = "Passion",
             size = 36
    ) +
    labs(caption = cap) +
    scale_x_continuous(limits = c(-19, 19)) +
    scale_y_continuous(limits = c(-19, 19)) +
    scale_colour_manual(values = c("male" = secondary_col, "female" = primary_col)) +
    coord_fixed() +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = bg_col, colour = bg_col),
      panel.background = element_rect(fill = bg_col, colour = bg_col),
      plot.caption = element_textbox_simple(
        family = "Ubuntu",
        colour = primary_col,
        hjust = 0,
        halign = 0,
        lineheight = 0.5,
        size = 22,
        margin = margin(l = 10, b = 10)
      )
    )

}

# save dark version
nobel_plot_all(bg_col = "grey10",
               primary_col = "white",
               secondary_col = "grey45")
fname <- paste0("Nobel Prize Laureates/images/nobel_dark_mode.png")
ggsave(
  filename = fname,
  width = 9,
  height = 9,
  unit = "in"
)

# save light version
nobel_plot_all(bg_col = "grey95",
               primary_col = "black",
               secondary_col = "grey70")
fname <- paste0("Nobel Prize Laureates/images/nobel_light_mode.png")
ggsave(
  filename = fname,
  width = 9,
  height = 9,
  unit = "in"
)
