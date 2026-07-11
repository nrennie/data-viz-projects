# Packages ----------------------------------------------------------------

library(tidyverse)
library(glue)


# Stations ----------------------------------------------------------------

station_names <- c(
  "Lerwick", "Stornoway", "Tiree", "Dunstaffnage",
  "Braemar", "Leuchars", "Wick Airport", "Eskdalemuir"
)


# Functions ---------------------------------------------------------------

get_data <- function(station) {
  # Set up
  station_id <- str_remove_all(tolower(station), " ")
  raw_url <- glue("https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/{station_id}data.txt")
  raw_data <- readLines(raw_url)

  num_lines <- if_else(
    station == "Braemar", 6, 5
  )
  ll_lines <- if_else(
    station == "Braemar", 3, 2
  )
  # Metadata
  raw_metadata <- raw_data[1:num_lines]
  clean_metadata <- tibble(
    station = raw_metadata[1],
    m = str_match(raw_metadata[ll_lines], "Lat\\s+(-?\\d+\\.?\\d*)\\s+Lon\\s+(-?\\d+\\.?\\d*)"),
    lat = as.numeric(m[, 2]),
    lon = as.numeric(m[, 3])
  ) |>
    select(-m)

  # Weather data
  raw_weather_data <- raw_data[(num_lines + 3):length(raw_data)]
  clean_data <- raw_weather_data |>
    str_squish() |>
    tibble(value = _) |>
    separate_wider_delim(value,
      names_sep = "_", delim = " ",
      too_few = "align_start"
    ) |>
    mutate(across(-any_of("value_8"), parse_number)) |>
    mutate(station = station)
  colnames(clean_data) <- c(
    "year", "month", "max_temp", "min_temp",
    "frost", "rain", "sun", "notes", "station"
  )

  return(list(
    metadata = clean_metadata,
    weather = clean_data
  ))
}


# Extract and save data ---------------------------------------------------

all_data <- map(
  .x = station_names,
  .f = ~ get_data(.x)
)

metadata <- map(
  .x = all_data,
  .f = ~ .x[[1]]
) |>
  bind_rows()
write_csv(metadata, "scottish-temperatures/data/metadata.csv")

weather <- map(
  .x = all_data,
  .f = ~ .x[[2]]
) |>
  bind_rows()
write_csv(weather, "scottish-temperatures/data/weather.csv")
