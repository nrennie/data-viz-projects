# Load packages -----------------------------------------------------------

library(tidyverse)
library(ggview)
library(showtext)
library(ggtext)
library(nrBrand)
library(glue)
library(cowplot)
library(grid)


# Load data ---------------------------------------------------------------

data_url <- "https://raw.githubusercontent.com/nrennie/data/refs/heads/main/reddit-al-dente-pasta/al_dente_pasta.csv"
raw_data <- read_csv(data_url) |> 
  mutate(pasta = if_else(pasta == "Farfalle", "Farfalle*", pasta))


# Load fonts --------------------------------------------------------------

font_add("FA",
         regular = "fonts/Font Awesome 6 Free-Solid-900.otf"
)
font_add_google("Oswald")
font_add_google("Nunito")
showtext_auto()
showtext_opts(dpi = 300)
title_font <- "Oswald"
body_font <- "Nunito"


# Define colours and fonts-------------------------------------------------

bg_col <- "#F2F4F8"
text_col <- "#151C28"
highlight_col <- "#7F055F"


# Text --------------------------------------------------------------------

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font,
  mastodon = NA
)
cap <- source_caption(source = "reddit.com (r/Cooking)", graphic = social, sep = " | ")
note <- paste0("*Farfalle is described by the user a 'a design flaw someone decided to mass produce' and so no time can be given.<br>", cap)


# Plot --------------------------------------------------------------------

ggplot(data = raw_data) +
  geom_rect(
    mapping = aes(
      xmin = box_min, xmax = box_max,
      ymin = 0, ymax = 1
    ),
    fill = highlight_col,
    alpha = 0.5
  ) +
  geom_segment(
    mapping = aes(x = actual_decimal, xend = box_min, y = 0.5),
    colour = text_col
  ) +
  geom_point(
    mapping = aes(x = actual_decimal, y = 0.5),
    pch = 23,
    colour = text_col,
    fill = "#E4572E",
    size = 3
  ) +
  facet_wrap(~ reorder(pasta, -difference_decimal), ncol = 1, strip.position = "left") +
  scale_x_continuous(limits = c(0, 16), expand = expansion(0, 0)) +
  labs(
    x = "Cooking time (minutes)", title = "You're probably overcooking your pasta",
    subtitle = glue("Reddit user, sthduh, timed how long 31 different pasta shapes took to reach *al dente*<span style='font-size:8pt;font-family:\"FA\";color:#E4572E;'>&#xf219;</span> , and compared them to the <span style='color:{highlight_col};'>suggested cooking times on the box</span>."
    ),
    caption = note
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = body_font) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_text(hjust = 1, margin = margin(t = 5), size = rel(0.9)),
    axis.title.y = element_blank(),
    strip.background = element_blank(),
    strip.text.y.left = element_text(
      face = "bold",
      size = rel(0.9),
      angle = 0,
      hjust = 1
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.margin = margin(5, 15, 5, 5),
    plot.background = element_rect(fill = bg_col, colour = bg_col),
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.title = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 5),
      family = title_font,
      face = "bold",
      size = rel(1.5)
    ),
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 5, t = 0),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      colour = text_col,
      hjust = 0,
      halign = 0,
      margin = margin(b = 0, t = 10),
      family = body_font
    ),
    panel.spacing = unit(0.1, "lines")
  ) +
  canvas(
    width = 5, height = 7,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p

ggdraw(p) +
  draw_text(
    x = 0.23, y = 0.16,
    size = 10,
    hjust = 0,
    colour = text_col,
    family = body_font,
    lineheight = 0.8,
    text = str_wrap("More overcooked", 17)
  ) +
  draw_text(
    x = 0.23, y = 0.84,
    size = 10,
    hjust = 0,
    colour = text_col,
    family = body_font,
    lineheight = 0.8,
    text = str_wrap("Less overcooked", 17)
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.24, y1 = 0.79,
      x2 = 0.24, y2 = 0.82,
      curvature = 0,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.24, y1 = 0.21,
      x2 = 0.24, y2 = 0.18,
      curvature = 0,
      gp = gpar(col = text_col, lwd = 2, fill = text_col),
      arrow = arrow(type = "closed", length = unit(0.07, "inches"))
    )
  ) +
  canvas(
    width = 5, height = 7,
    units = "in", bg = bg_col,
    dpi = 300
  ) -> p0

save_ggplot(
  plot = p0,
  file = "Reddit al dente pasta/images/pasta.png"
)


