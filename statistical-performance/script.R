# Packages ----------------------------------------------------------------

library(tidyverse)


# Load data ---------------------------------------------------------------

raw_data <- read_csv("statistical-performance/data/spi.csv")


# Prep data ---------------------------------------------------------------

table_data <- raw_data |> 
  rename(
    group = region,
    name = country,
    Income = income,
    `Overall score` = overall_score
  ) |> 
  filter(year == 2024) |> 
  select(group, name, Income, `Overall score`) |> 
  arrange(group, `Overall score`)


# Write CSV ---------------------------------------------------------------

readr::write_csv(table_data, "statistical-performance/data.csv", na = "")
