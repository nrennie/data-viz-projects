# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(nrBrand)
library(glue)


# Load fonts --------------------------------------------------------------

font_add_google("MedievalSharp")
showtext_auto()


# Parameters --------------------------------------------------------------

sonnet_num <- "sonnetCXLIX"
sonnet_num <- "sonnetCLIII"
sonnet_num <- "sonnetXVIII"

bg_col <- "#F2E8D9"
col_palette <- c("#16425B", "#48A9A6", "#D4B483", "#C1666B")
text_col <- "#484541"
text_family <- "MedievalSharp"
text_size <- 3
border <- 0
s <- 1234


# Load data ---------------------------------------------------------------

metadata <- readr::read_csv("https://raw.githubusercontent.com/nrennie/shakespeare/refs/heads/main/data/metadata.csv")
sonnet <- readr::read_csv(glue("https://raw.githubusercontent.com/nrennie/shakespeare/refs/heads/main/data/{sonnet_num}.csv"))

title_data <- metadata |>
  filter(File == sonnet_num) |>
  select(Title) |>
  separate_wider_delim(Title, delim = ". ", names = c("Num", "Name"))
title <- title_data$Name
subtitle <- glue("Sonnet {title_data$Num}")


# Plot --------------------------------------------------------------------

# processing
max_n <- max(nchar(sonnet$line))
sonnet_data <- tolower(sonnet$line)
sonnet_data <- stringr::str_pad(sonnet_data, max_n, side = "right")
sonnet_data <- stringr::str_split_fixed(sonnet_data, "", n = max_n)
colnames(sonnet_data) <- seq_len(max_n)
sonnet_data <- tibble::as_tibble(sonnet_data)

# set colours
set.seed(s)
unique_chars <- unique(unlist(sonnet_data))
unique_chars <- unique_chars[unique_chars %in% letters]
plot_cols <- tibble::tibble(
  value = letters,
  fill_col = sample(
    grDevices::colorRampPalette(col_palette)(length(letters))
  )
)

# text
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = text_col,
  font_colour = text_col,
  font_family = text_family,
  mastodon = NA
)
cap <- paste0(
  "**Data**: shakespeare.mit.edu<br>**Graphic**:", social
)

# plot data
plot_data <- sonnet_data |>
  dplyr::mutate(y = dplyr::row_number(), .before = 1) |>
  tidyr::pivot_longer(
    cols = -y,
    names_to = "x",
    values_to = "value"
  ) |>
  dplyr::mutate(x = as.numeric(x)) |>
  dplyr::left_join(plot_cols, by = "value") |>
  dplyr::mutate(fill_col = tidyr::replace_na(fill_col, bg_col))

# plot
g <- ggplot() +
  geom_tile(
    data = plot_data,
    mapping = aes(x = x, y = y, fill = fill_col),
    height = 0.9,
    width = 0.8
  ) +
  geom_text(
    data = dplyr::filter(plot_data, !(value %in% letters)),
    mapping = aes(x = x, y = y, label = value),
    colour = text_col,
    size = text_size,
    family = text_family
  ) +
  scale_y_reverse() +
  scale_fill_identity() +
  labs(
    title = title,
    subtitle = subtitle,
    caption = cap
  ) +
  theme_void(base_size = 20) +
  theme(
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = bg_col, colour = bg_col),
    plot.margin = margin(border, border, border, border, unit = "in"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 10),
      lineheight = 0.5,
      family = text_family,
      face = "bold"
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 5),
      lineheight = 0.5,
      family = text_family
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 5),
      lineheight = 0.5,
      family = text_family
    )
  )

fname <- glue("Shakespeare's Sonnets/images/{sonnet_num}.png")
ggsave(fname, g, width = 4, height = 4)
