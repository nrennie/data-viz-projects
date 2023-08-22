library(tidyverse)
library(ggtext)
library(showtext)

# load data
# accessed: 22 Aug 2023
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

# tidy text
plot_data <- book_data |>
  mutate(tidy_title = gsub(":.*", "", title), .after = title) |>
  mutate(
    tidy_title = gsub("\\(.*", "", tidy_title),
    tidy_title = str_to_title(tidy_title)
  ) |>
  mutate(
    author = trimws(author, which = "right", whitespace = ", "),
    author = gsub("  ", " ", author)
  ) |> 
  mutate(tidy_title = case_when(
    days < 10 ~ str_wrap(tidy_title, 4),
    days >= 10 & days < 23 ~ str_wrap(tidy_title, 15),
    TRUE ~ str_wrap(tidy_title, 25)
  )) |> 
  mutate(author = case_when(
    days < 12 ~ str_wrap(author, 4),
    days >= 10 & days < 23 ~ str_wrap(author, 18),
    TRUE ~ str_wrap(author, 25)
  ))

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
                 each book in this chart represent the number of pages, and the 
                 heights how long I spent reading it.")

# plot
ggplot() +
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
  # titles and authors
  geom_text(
    data = plot_data,
    mapping = aes(
      x = start_pages + 40,
      y = 0.5,
      label = tidy_title
    ),
    angle = 90,
    size = 6.5,
    hjust = 0,
    vjust = 1,
    lineheight = 0.35,
    family = "henny",
    colour = "white"
  ) +
  geom_text(
    data = plot_data,
    mapping = aes(
      x = end_pages - 40,
      y = 0.5,
      label = author
    ),
    angle = 90,
    size = 6,
    hjust = 0,
    lineheight = 0.3,
    vjust = 0,
    fontface = "italic",
    family = "cabin",
    colour = "white",
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
      maxwidth = 0.7,
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

# save
ggsave("Reading Shelf/images/reading-shelf.png", width = 6, height = 4, units = "in")
