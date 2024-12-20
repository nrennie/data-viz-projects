# Load packages -----------------------------------------------------------

library(tidyverse)
library(osmdata)
library(sf)
library(cowplot)
library(nrBrand)
library(ggtext)


# Social caption ----------------------------------------------------------

social_caption <- function(linkedin = "nicola-rennie",
                           bluesky = "nrennie",
                           github = "nrennie",
                           icon_color = "black",
                           font_color = "black",
                           font_family = "sans") {
  icon_df <- data.frame(
    icons = c("&#xf08c;", "&#xe671;", "&#xf09b;"),
    socials = c(linkedin, bluesky, github)
  )
  icon_df <- na.omit(icon_df)

  # Inner function to join icon and text
  glue_icon <- function(icon, social) {
    glue::glue(
      "<span style='font-family:\"Font Awesome 6 Brands\"; color:{icon_color};'>{icon} </span>&nbsp; <span style='font-family:{font_family}; color:{font_color};'>{social} </span>&emsp;"
    )
  }

  # Map over all icons
  caption <- purrr::map2(
    .x = icon_df$icons,
    .y = icon_df$socials,
    .f = ~ glue_icon(.x, .y)
  ) |>
    purrr::as_vector() |>
    stringr::str_flatten()

  return(caption)
}


# Source caption ----------------------------------------------------------

source_caption <- function(source, graphic, sep = "<br>") {
  caption <- glue::glue(
    "**Data**: {source}{sep}**Graphic**: {graphic}"
  )
  return(caption)
}


# Plot text ---------------------------------------------------------------

plot_text <- function(location_text,
                      coords,
                      linkedin = "nicola-rennie",
                      bluesky = "nrennie",
                      github = "nrennie",
                      icon_color = "black",
                      font_color = "black",
                      font_size = 12,
                      title_font_family = "sans",
                      font_family = "sans") {
  place <- stringr::str_to_upper(stringr::str_extract(location_text, "^[^,]+"))
  lat <- round(coords["lat"], 4)
  long <- round(coords["long"], 4)
  if (lat > 0) {
    lat <- glue::glue("{lat}° N")
  } else {
    lat <- glue::glue("{-1*lat}° S")
  }
  if (long > 0) {
    long <- glue::glue("{long}° E")
  } else {
    long <- glue::glue("{-1*long}° W")
  }
  coord_text <- glue::glue("{lat}, {long}")
  source <- source_caption(
    source = "OpenStreetMap",
    sep = " | ",
    graphic = social_caption(
      linkedin = linkedin,
      bluesky = bluesky,
      github = github,
      icon_color = icon_color,
      font_color = font_color,
      font_family = font_family
    )
  )
  output <- glue::glue("<span style='font-family:{title_font_family}; font-size:{font_size*4}pt;'>{place}</span><br><br><span style='font-size:{font_size*1.8}pt;'>{coord_text}</span><br><br><br><span style='font-size:{font_size}pt;'>{source}</span>")
  return(output)
}


# Street map  -------------------------------------------------------------

street_map <- function(location,
                       local_crs,
                       dist,
                       x_nudge = 0,
                       y_nudge = 0,
                       bg_col = "white",
                       line_col = "black",
                       font_size = 12,
                       body_font = "Ubuntu",
                       title_font = "Carter") {
  bbx <- osmdata::getbb(location)
  highways <- bbx |>
    opq() |>
    add_osm_feature(
      key = "highway",
      value = c(
        "motorway",
        "trunk",
        "primary",
        "secondary",
        "tertiary",
        "motorway_link",
        "trunk_link",
        "primary_link",
        "secondary_link",
        "tertiary_link"
      )
    ) |>
    osmdata_sf()
  streets <- bbx |>
    opq() |>
    add_osm_feature(
      key = "highway",
      value = c(
        "residential",
        "living_street",
        "service",
        "unclassified",
        "pedestrian",
        "footway",
        "track",
        "path"
      )
    ) |>
    osmdata_sf()


  # Circle ------------------------------------------------------------------

  center <- c(long = mean(bbx[1, ]) + x_nudge, lat = mean(bbx[2, ]) + y_nudge)
  center_proj <-
    tibble(lat = center["lat"], long = center["long"]) |>
    st_as_sf(coords = c("long", "lat"), crs = 4326)
  circle <- tibble(lat = center["lat"], long = center["long"]) |>
    st_as_sf(coords = c("long", "lat"), crs = 4326) |>
    st_transform(crs = local_crs) |>
    st_buffer(dist = dist) |>
    st_transform(crs = 4326)
  streets_lines <- suppressWarnings(
    st_intersection(circle, streets$osm_lines)
  )
  highways_lines <- suppressWarnings(
    st_intersection(circle, highways$osm_lines)
  )


  # Text --------------------------------------------------------------------

  cap <- plot_text(location,
    coords = center,
    font_color = line_col,
    icon_color = line_col,
    font_size = font_size,
    title_font_family = title_font,
    font_family = body_font
  )

  # Plot --------------------------------------------------------------------

  p <- ggplot() +
    geom_sf(
      data = streets_lines,
      col = line_col,
      linewidth = 0.6,
      alpha = 0.7
    ) +
    geom_sf(
      data = highways_lines,
      col = line_col,
      linewidth = 0.8,
      alpha = 0.9
    ) +
    geom_sf(
      data = circle,
      color = line_col,
      linewidth = 2,
      fill = NA
    ) +
    labs(
      caption = cap
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(
        fill = bg_col, color = bg_col
      ),
      panel.background = element_rect(
        fill = bg_col, colour = bg_col
      ),
      plot.margin = margin(5, 10, 5, 10),
      plot.caption.position = "plot",
      plot.caption = element_textbox_simple(
        margin = margin(b = -20, t = 10),
        hjust = 0.5,
        halign = 0.5,
        color = line_col,
        family = body_font,
        size = font_size
      )
    )
  return(p)
}
