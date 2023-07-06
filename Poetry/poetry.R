library(ggplot2)

poetry_plot <- function(fname,
                        bg_col = "grey90",
                        col_palette = c("#413C58", "#D1495B", "#EDAE49", "#00798C", "#003D5B"),
                        text_col = "grey10",
                        text_family = "sans",
                        text_size = 12,
                        border = 0.5,
                        s = 1234) {
  # processing
  poem <- readLines(fname)
  max_n <- max(nchar(poem))
  poem <- tolower(poem)
  poem <- stringr::str_pad(poem, max_n, side = "right")
  poem <- stringr::str_split_fixed(poem, "", n = max_n) 
  colnames(poem) <- seq_len(max_n)
  poem <- tibble::as_tibble(poem)
  
  # set colours
  set.seed(1234)
  unique_chars <- unique(unlist(poem))
  unique_chars <- unique_chars[unique_chars %in% letters]
  plot_cols <- tibble::tibble(value = unique_chars,
                              fill_col = sample(
                                grDevices::colorRampPalette(col_palette)(length(unique_chars)))
  )
  
  # plot data
  plot_data <- poem |> 
    dplyr::mutate(y = dplyr::row_number(), .before = 1) |> 
    tidyr::pivot_longer(cols = -y, 
                        names_to = "x",
                        values_to = "value") |> 
    dplyr::mutate(x = as.numeric(x)) |> 
    dplyr::left_join(plot_cols, by = "value") |> 
    dplyr::mutate(fill_col = tidyr::replace_na(fill_col, bg_col))
  
  # plot
  ggplot() +
    geom_tile(data = plot_data,
              mapping = aes(x = x, y = y, fill = fill_col),
              height = 0.9,
              width = 0.8) +
    geom_text(data = dplyr::filter(plot_data, !(value %in% letters)),
              mapping = aes(x = x, y = y, label = value),
              colour = text_col,
              size = text_size,
              family = text_family) +
    scale_y_reverse() +
    scale_fill_identity() +
    theme_void() +
    theme(plot.background = element_rect(fill = bg_col, colour = bg_col),
          panel.background = element_rect(fill = bg_col, colour = bg_col),
          plot.margin = margin(border, border, border, border, unit = "in"))
}

# save example
library(showtext)
font_add_google("Commissioner")
showtext_auto()
rise_plot <- poetry_plot(fname = "Poetry/data/still-i-rise.txt",
                         text_family = "Commissioner")
ggsave("Poetry/images/poetry.png",
       plot = rise_plot,
       height = 11.7,
       width = 8.3,
       unit = "in")
