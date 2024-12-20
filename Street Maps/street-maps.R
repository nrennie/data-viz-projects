# Load functions ----------------------------------------------------------

source("Street Maps/functions.R")


# Fonts -------------------------------------------------------------------

sysfonts::font_add_google("Ubuntu")
sysfonts::font_add_google("Carter One", "Carter")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)


# Glasgow -----------------------------------------------------------------

p <- street_map(
  location = "Glasgow, UK",
  local_crs = 6384,
  dist = 4500,
  bg_col = "#3D2645",
  line_col = "#D7263D",
  font_size = 10,
  body_font = "Ubuntu",
  title_font = "Carter"
)

ggplot2::ggsave(p,
  filename = "Street Maps/images/glasgow.png",
  bg = "#3D2645", height = 11, width = 8.5, units = "in"
)


# Armadale ----------------------------------------------------------------

p <- street_map(
  location = "Armadale, West Lothian, UK",
  local_crs = 6384,
  dist = 3500,
  bg_col = "#DACC3E",
  line_col = "#0D5C63",
  font_size = 10,
  body_font = "Ubuntu",
  title_font = "Carter"
)

ggplot2::ggsave(p,
  filename = "Street Maps/images/armadale.png",
  bg = "#DACC3E", height = 11, width = 8.5, units = "in"
)


# St Andrews --------------------------------------------------------------

p <- street_map(
  location = "St Andrews, UK",
  local_crs = 6384,
  dist = 3500,
  bg_col = "#004E64",
  line_col = "#00A5CF",
  font_size = 10,
  body_font = "Ubuntu",
  title_font = "Carter"
)

ggplot2::ggsave(p,
  filename = "Street Maps/images/st_andrews.png",
  bg = "#004E64", height = 11, width = 8.5, units = "in"
)


# Lancaster ---------------------------------------------------------------

p <- street_map(
  location = "Lancaster, UK",
  local_crs = 6384,
  dist = 4500,
  x_nudge = -0.07,
  y_nudge = -0.03,
  bg_col = "#FF9914",
  line_col = "#28502E",
  font_size = 10,
  body_font = "Ubuntu",
  title_font = "Carter"
)

ggplot2::ggsave(p,
  filename = "Street Maps/images/lancaster.png",
  bg = "#FF9914", height = 11, width = 8.5, units = "in"
)


# Banbury -----------------------------------------------------------------

p <- street_map(
  location = "Banbury, UK",
  local_crs = 6384,
  dist = 3500,
  bg_col = "#70A288",
  line_col = "#04395E",
  font_size = 10,
  body_font = "Ubuntu",
  title_font = "Carter"
)

ggplot2::ggsave(p,
  filename = "Street Maps/images/banbury.png",
  bg = "#70A288", height = 11, width = 8.5, units = "in"
)


# Dunfermline -------------------------------------------------------------

p <- street_map(
  location = "Dunfermline, UK",
  local_crs = 6384,
  dist = 3500,
  bg_col = "#F2B880",
  line_col = "#9B287B",
  font_size = 10,
  body_font = "Ubuntu",
  title_font = "Carter"
)

ggplot2::ggsave(p,
  filename = "Street Maps/images/dunfermline.png",
  bg = "#F2B880", height = 11, width = 8.5, units = "in"
)


# Edinburgh ---------------------------------------------------------------

p <- street_map(
  location = "Edinburgh, UK",
  local_crs = 6384,
  dist = 4500,
  x_nudge = 0.1,
  y_nudge = 0.03,
  bg_col = "#A4031F",
  line_col = "#FBB13C",
  font_size = 10,
  body_font = "Ubuntu",
  title_font = "Carter"
)

ggplot2::ggsave(p,
  filename = "Street Maps/images/edinburgh.png",
  bg = "#A4031F", height = 11, width = 8.5, units = "in"
)
