library(tidyverse)
library(ggtext)
library(showtext)

# load data
# accessed: 27 Jan 2024
books <- readr::read_csv("Reading Shelf/data/goodreads_library_export.csv") |>
  janitor::clean_names()

# load fonts
font_add_google(name = "Henny Penny", family = "henny")
font_add_google(name = "Cabin", family = "cabin")
showtext_auto()

# variables
yr <- 2023

# colours
bg_col <- "#DBD3AD"
text_col <- "#7E5920"
highlight_col1 <- "#E84855" # above
highlight_col2 <- "#616283" # below

# data processing
book_data <- books |>
  select(title, my_rating, average_rating, author, additional_authors, date_read, number_of_pages) |>
  mutate(year = year(ymd(date_read))) |>
  filter(year == yr) |>
  mutate(additional_authors = replace_na(additional_authors, "")) |>
  unite("author", author:additional_authors, sep = ", ") |>
  arrange(date_read) |>
  mutate(
    end_pages = cumsum(number_of_pages),
    start_pages = c(0, head(cumsum(number_of_pages), -1)),
    mid_pages = 0.5 * (start_pages + end_pages),
    rating_comp = case_when(
      my_rating < average_rating ~ "below average",
      my_rating == average_rating ~ "average",
      my_rating > average_rating ~ "above average"
    ),
    days_since = yday(date_read),
    days = diff(c(0, days_since))
  )

# text
social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col1,
  font_colour = text_col,
  font_family = "cabin"
)
st <- glue::glue("I read a total of {nrow(plot_data)} books in {yr}, spanning 
                 {format(max(plot_data$end_pages), big.mark = ',')} pages. Of those books, I rated 
                 {table(plot_data$rating_comp)['above average']} 
                 of them <span style='color:{highlight_col1}'>above</span> the 
                 average Goodreads rating, and 
                 {table(plot_data$rating_comp)['below average']} of them 
                 <span style='color:{highlight_col2}'>below</span>. The widths 
                 of each book in this chart represent the number of pages, and the 
                 heights, how long I spent reading it.")

# tidy text
plot_data <- book_data |>
  mutate(tidy_title = gsub(":.*", "", title), .after = title) |>
  mutate(
    tidy_title = gsub("\\(.*", "", tidy_title),
    tidy_title = str_to_title(tidy_title)
  ) |>
  mutate(
    author = trimws(author, which = "right", whitespace = ", "),
    author = gsub("  ", " ", author),
    author = case_when(
      str_detect(author, "Arthur Conan Doyle") ~ "Arthur Conan Doyle",
      TRUE ~ author
    )
  ) |> 
  mutate(
    label = glue::glue(
      "<span style='font-family: henny'>{tidy_title}. </span>
      <span style='font-family: cabin'>{author}. </span>"
    )
  ) |> 
  # wrap based on title length and days
  mutate(label_len = nchar(tidy_title) + nchar(author)) |> 
  mutate(id = row_number())

# add label function
geom_booklabel <- function(row_num) {
  data <- filter(plot_data, id == row_num)
  width <- data$days / 80
  size <- ifelse(data$days > 30, 6, 4)
  geom_textbox(
    data = data,
    mapping = aes(
      x = mid_pages,
      y = 0.5,
      label = label
    ),
    hjust = 0,
    halign = 0,
    vjust = 0.5,
    valign = 0.5,
    lineheight = 0.35,
    fill = "transparent",
    box.colour = "transparent",
    text.colour = "white",
    orientation = "left-rotated",
    size = size,
    maxwidth = width,
    box.padding = unit(c(0.5, 0.5, 0.5, 0.5), "pt")
  ) 
}


# plot
base_plot <- ggplot() +
  # books
  geom_rect(
    data = plot_data,
    mapping = aes(
      xmin = start_pages,
      xmax = end_pages,
      ymin = 0,
      ymax = days,
      fill = rating_comp
    ),
    colour = bg_col
  ) +
  # bookshelf
  geom_rect(
    data = data.frame(),
    mapping = aes(
      xmin = -100,
      xmax = max(plot_data$end_pages) + 100,
      ymin = -1.5,
      ymax = 0
    ),
    fill = text_col
  ) +
  # styling
  scale_fill_manual(
    values = c(
      "above average" = highlight_col1,
      "below average" = highlight_col2
    )
  ) +
  scale_size_identity() +
  labs(
    title = glue::glue("Nicola's {yr} Bookshelf"),
    caption = social,
    subtitle = st
  ) +
  theme_void(base_family = "cabin") +
  theme(
    legend.position = "none",
    plot.background = element_rect(colour = bg_col, fill = bg_col),
    panel.background = element_rect(colour = bg_col, fill = bg_col),
    plot.title = element_textbox_simple(
      family = "henny",
      hjust = 1,
      halign = 1,
      colour = text_col,
      face = "bold",
      size = 54,
      margin = margin(t = 10, r = 15)
    ),
    plot.subtitle = element_textbox_simple(
      hjust = 1,
      halign = 1,
      colour = text_col,
      maxwidth = 0.75,
      size = 24,
      lineheight = 0.5,
      family = "cabin",
      margin = margin(b = 10, t = 10, r = 15)
    ),
    plot.caption = element_textbox_simple(
      hjust = 0.5,
      halign = 0.5,
      colour = text_col,
      size = 20,
      lineheight = 0.5,
      family = "cabin",
      margin = margin(b = 10, t = 0)
    ),
    plot.margin = margin(5, 0, 0, 0)
  )

# add labels
for (i in 1:max(plot_data$id)) {
  base_plot <- base_plot + 
  geom_booklabel(row_num = i) 
}
base_plot

# save
ggsave("Reading Shelf/images/reading-shelf.png", width = 6, height = 4, units = "in")
