library(pixR)
library(tidyverse)
library(treemapify)

# Process data ------------------------------------------------------------

bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-02-21/bob_ross.csv")
bob_ross <- bob_ross |>
  mutate(id = row_number())

# list of images
imgs <- bob_ross$img_src

# function to extract counts of colours
extract_data <- function(i) {
  output <- to_treemap(img_path = imgs[i], n = 5, rescale = 50, plot = FALSE)
  output$id <- i
  return(output)
}

# map over images
all_imgs <- map_df(
  .x = 1:length(imgs),
  .f = ~ extract_data(.x)
)

# save to file
write_csv(all_imgs, "Bob Ross Paintings/data.csv")


# Plotting ----------------------------------------------------------------

all_imgs <- readr::read_csv("Bob Ross Paintings/data.csv")

# standardise
norm_imgs <- all_imgs |>
  group_by(id) |>
  mutate(tot_n = sum(n)) |>
  ungroup() |>
  mutate(prop = n / tot_n) |>
  left_join(
    select(bob_ross, id, season, episode),
    by = "id"
  )

# plot
p <- ggplot(
  data = norm_imgs,
  mapping = aes(
    area = prop, fill = hex,
    label = hex
  )
) +
  geom_treemap() +
  facet_grid(episode~season) +
  scale_fill_identity() +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    plot.background = element_rect(fill = "white", colour = "white")
  )

# save
ggsave("Bob Ross Paintings/images/bob-ross-paintings.png",
  plot = p,
  height = 6,
  width = 15,
  unit = "in"
)
