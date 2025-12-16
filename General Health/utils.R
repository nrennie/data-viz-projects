# load packages and fonts
library(tidyverse)
library(rcartocolor)
library(ggtext)
library(showtext)
font_add_google("Roboto")
font_add_google("Roboto Slab")
showtext_auto()

#' Theme for {ggplot2} plots
#' @param main_family Name of font family to use for most of the text in the plot. Default "Roboto".
#' @param title_family Name of font family to use for title of the plot. Default "Roboto Slab".
#' @param bg_colour Background colour of plot. Default "white".
#' @param text_colour Text colour in plot. Default "black".
#' @param base_size Base size of text. Default 13.
theme_map <- function(main_family = "Roboto",
                      title_family = "Roboto Slab",
                      bg_colour = "white",
                      text_colour = "black",
                      base_size = 13) {
  ggplot2::theme_void(base_family = main_family, base_size = base_size) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggtext::element_textbox_simple( family = title_family, colour = text_colour, lineheight = 0.5, face = "bold"),
      plot.subtitle = ggtext::element_textbox_simple(colour = text_colour, hjust =  0, lineheight = 0.5, margin = ggplot2::margin(t = 15)),
      plot.caption = ggtext::element_textbox_simple(colour = text_colour, hjust =  0, lineheight = 0.5, margin = ggplot2::margin(t = 10)),
      legend.title = ggplot2::element_text(hjust = 0.5),
      plot.background = ggplot2::element_rect(fill = bg_colour, colour = bg_colour),
      panel.background = ggplot2::element_rect(fill = bg_colour, colour = bg_colour),
      plot.margin = ggplot2::margin(15, 15, 15, 15)
    )
}
