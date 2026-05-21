library(NCmisc)
library(tidyverse)
library(camcorder)
library(showtext)
library(janitor)
library(patchwork)
library(ggtextures)

font_add_google("Raleway", "raleway")
showtext_auto()

# get all functions used
fpath <- glue::glue(dirname(here::here()), "/tidytuesday/2023/")
all_files <- list.files(path = fpath, recursive = TRUE, pattern = "\\.[Rr]$")
to_search <- all_files |> 
  as_tibble() |> 
  mutate(value = paste0(fpath, value)) |> 
  pull(value)
all_functions <- map(.x = to_search, .f = ~ list.functions.in.file(.x))

# tidy up date and count 
func_data <- unlist(all_functions) |> 
  unname() |> 
  tabyl() |> 
  as_tibble() |> 
  rename(func = `unname(unlist(all_functions))`) |> 
  filter(!(func %in% c("library", "social_caption"))) |> 
  slice_max(n, n = 5, with_ties = FALSE) |> 
  mutate(rank = row_number())
func_data

# count packages
pkg_data <- unlist(all_functions) |> 
  names() |> 
  stringr::str_remove("package:") |> 
  stringr::str_replace_all("[:digit:]", "") |> 
  tabyl() |> 
  as_tibble() |> 
  rename(pkg = `stringr::str_replace_all(stringr::str_remove(names(unlist(all_functions)),     "package:"), "[:digit:]", "")`) |> 
  arrange(desc(n)) |> 
  mutate(pkg = case_when(
    pkg == "ggplot" ~ "ggplot2",
    TRUE ~ pkg
  )) |> 
  filter(!(pkg %in% c("nrBrand", "character()")),
         str_detect(pkg, "c\\(", negate = TRUE)) |> 
  slice_max(n, n = 5, with_ties = FALSE) |> 
  mutate(rank = row_number())
pkg_data

# image file
imgs <- data.frame(img = "TidyTuesday Wrapped/tidytuesday.png")


# Variables ---------------------------------------------------------------

bg_col <- "#fff200"
text_col <- "#000000"
body_font <- "raleway"


# Plot --------------------------------------------------------------------

aRt::rings(
  x_ring = c(1.8, 3.4, 2.3),
  y_ring = c(0.4, 0.8, 2.1),
  r_ring = c(0.3, 0.4, 0.5),
  x0 = c(1.7, 3.3, 2.2),
  y0 = c(0.3, 0.7, 1.9),
  r = c(0.5, 0.7, 0.8),
  n = c(80, 80, 80),
  bg_col = bg_col,
  s = 2023
  ) +
  # add text with the numbers 1 to 5
  geom_text(data = func_data,
            mapping = aes(x = rep(2.5, 5),
                          y = seq(3.5, 5, length.out = 5),
                          label = paste0(rank, " ", func)),
            colour = text_col,
            hjust = 0,
            size = 11,
            fontface = "bold",
            family = body_font) +
  annotate(
    "text",
    x = 2.5,
    y = 3.2,
    hjust = 0,
    label = "Top functions",
    family = body_font,
    colour = text_col,
    size = 10
  ) +
  # add text with the names of the functions, and the number of times its used
  geom_text(data = pkg_data,
            mapping = aes(x = rep(0.9, 5),
                          y = seq(3.5, 5, length.out = 5),
                          label = paste0(rank, " ", pkg)),
            colour = text_col,
            hjust = 0,
            size = 11,
            fontface = "bold",
            family = body_font) +
  annotate(
    "text",
    x = 0.9,
    y = 3.2,
    hjust = 0,
    label = "Top packages",
    family = body_font,
    colour = text_col,
    size = 10
  ) +
  # Total scripts
  annotate(
    "text",
    x = 0.9,
    y = 5.5,
    hjust = 0,
    label = ".R files written",
    family = body_font,
    colour = text_col,
    size = 10
  ) +
  annotate(
    "text",
    x = 0.9,
    y = 5.75,
    hjust = 0,
    label = length(all_files),
    family = body_font,
    colour = text_col,
    fontface = "bold",
    size = 11
  ) +
  # Total task
  annotate(
    "text",
    x = 2.5,
    y = 5.5,
    hjust = 0,
    label = "Top task",
    family = body_font,
    colour = text_col,
    size = 10
  ) +
  annotate(
    "text",
    x = 2.5,
    y = 5.75,
    hjust = 0,
    label = "Visualisation",
    family = body_font,
    colour = text_col,
    fontface = "bold",
    size = 11
  ) +
  # add image
  geom_textured_rect(data = imgs, 
                     aes(xmin = 1.65, xmax = 3.25,
                         ymax = 0.4, ymin = 2, image = img), 
                     fill = "transparent",
                     nrow = 1,
                     ncol = 1,
                     img_width = unit(1, "null"),
                     img_height = unit(1, "null"),
                     position = "identity") +
  # set axis limits and reverse y axis
  scale_x_continuous(limits = c(0.9, 4)) +
  scale_y_reverse(limits = c(6.0, -0.2)) +
  # add a caption
  labs(caption = "#TidyTuesday Wrapped") +
  # set the theme
  theme_void(base_family = "raleway") +
  theme(plot.background = element_rect(fill = bg_col, colour = bg_col),
        panel.background = element_rect(fill = bg_col, colour = bg_col),
        plot.margin = margin(-20, 5, 5, 10),
        plot.caption = element_text(colour = text_col,
                                    margin = margin(t = 0, b = -30),
                                    face = "bold",
                                    hjust = 0.95,
                                    size = 24))

# save
ggsave("TidyTuesday Wrapped/images/tidytuesday-wrapped-2023.png", width = 2.5, height = 5, units = "in")
