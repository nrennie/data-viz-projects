# Packages ----------------------------------------------------------------

library(tidyverse)


# Load data ---------------------------------------------------------------

raw_data <- read_csv("statistical-performance/data/spi.csv")


# Prep data ---------------------------------------------------------------

table_data <- raw_data |> 
  rename(
    group = region,
    name = country,
    Income = income
  ) |> 
  mutate(Income = str_remove(Income, " income")) |> 
  filter(year %in% 2016:2024) |> 
  select(group, name, Income, year, overall_score) |> 
  arrange(year) |> 
  pivot_wider(names_from = "year", values_from = overall_score) |> 
  mutate(`2024 score` = `2024`,
         `Since 2016` = `2024` - `2016`) |> 
  arrange(group, `2024 score`) |> 
  select(group, name, Income, `2024 score`, `2016`:`2024`, `Since 2016`)


# Write CSV ---------------------------------------------------------------

readr::write_csv(table_data, "statistical-performance/data.csv", na = "")
