# Load packages -----------------------------------------------------------

library(tidyverse)
library(showtext)
library(camcorder)
library(ggtext)
library(ggridges)
library(glue)
library(scales)


# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2024-09-03")
qname_levels_single_response_crosswalk <- tuesdata$qname_levels_single_response_crosswalk
stackoverflow_survey_questions <- tuesdata$stackoverflow_survey_questions
stackoverflow_survey_single_response <- tuesdata$stackoverflow_survey_single_response


# Load fonts --------------------------------------------------------------

font_add_google("Montserrat")
showtext_auto()
showtext_opts(dpi = 300)
body_font <- "Montserrat"


# Start recording ---------------------------------------------------------

gg_record(
  dir = file.path("recording"),
  device = "png",
  width = 1200,
  height = 1200,
  units = "px",
  dpi = 300
)

# Define colours ----------------------------------------------------------

bg_col <- "#D7E0E7"
ds_col <- "#914DFF"
da_col <- "#FF914D"
text_col <- "#1c3f60"


# Data wrangling ----------------------------------------------------------

plot_data <- stackoverflow_survey_single_response |>
  filter(dev_type %in% c(5, 6)) |>
  left_join(
    filter(
      qname_levels_single_response_crosswalk,
      qname == "dev_type", level %in% c(5, 6)
    ),
    by = c("dev_type" = "level")
  ) |>
  drop_na(label) |>
  rename(job = label) |>
  select(job, age, ed_level, years_code, currency, comp_total)


# Plots -------------------------------------------------------------------

## Age ----------------------------------------------------------------------

plot_data |>
  select(job, age) |>
  filter() |>
  drop_na() |>
  left_join(
    filter(
      qname_levels_single_response_crosswalk,
      qname == "age"
    ),
    by = c("age" = "level")
  ) |>
  mutate(
    label = factor(
      label,
      levels = c(
        "Under 18 years old", "18-24 years old", "25-34 years old",
        "35-44 years old", "45-54 years old", "55-64 years old",
        "65 years or older", "Prefer not to say"
      )
    )
  ) |>
  count(job, label) |>
  ggplot() +
  geom_col(
    mapping = aes(
      y = label,
      x = n,
      fill = job
    ),
    position = "fill"
  ) +
  scale_fill_manual(
    values = c(da_col, ds_col)
  ) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(
    breaks = c(0, 0.5, 1),
    labels = c("0%", "50%", "100%")
  ) +
  labs(
    x = "Percentage of age group", y = "",
    subtitle = glue("For respondents under the age of 55, more of them are
       <span style='color:{ds_col}'>data scientists or machine learning engineers</span>
       compared to <span style='color:{da_col}'>data or business analysts</span>.")
  ) +
  theme_minimal(base_family = body_font) +
  theme(
    text = element_text(
      colour = text_col
    ),
    legend.position = "none",
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10, l = 10),
      lineheight = 0.5,
      family = body_font
    ),
    panel.grid = element_blank(),
    plot.margin = margin(5, 10, 5, 0)
  )

ggplot2::ggsave(
  filename = "Stack Overflow Survey/so_age.png",
  width = 1200,
  height = 1200,
  units = "px"
)


## Salary -----------------------------------------------------------------

salary_data <- plot_data |>
  select(job, comp_total, currency) |>
  drop_na() |>
  filter(
    currency == "GBP\tPound sterling",
    comp_total <= 100000000
  )

salary_data |> 
  group_by(job) |> 
  summarise(m = median(comp_total))

salary_data |> 
  summarise(m = median(comp_total))

ggplot(
  data = salary_data
) +
  geom_density(
    data = select(salary_data, -job),
    mapping = aes(
      x = comp_total,
      y = after_stat(count),
      fill = "All",
    ),
    color = "transparent"
  ) +
  geom_density(
    mapping = aes(
      x = comp_total,
      y = after_stat(count),
      fill = job
    ),
    color = "transparent"
  ) +
  facet_wrap(~job, ncol = 1) +
  scale_x_continuous(
    breaks = c(
      0, 250000, 500000
    ),
    labels = c(
      "0", "250,000", "500,000"
    ),
    expand = c(0, 0),
    limits = c(0, NA)
  ) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(
    values = c("grey50", da_col, ds_col)
  ) +
  labs(
    x = "Annual compensation (£)", y = "",
    subtitle = glue("For those paid in Pounds Sterling, the median annual income 
    is £67,500 for a <span style='color:{ds_col}'>data scientist or machine learning engineer</span>
    and £48,500 for a <span style='color:{da_col}'>data or business analyst</span>, compared to 
    £57,000 for <span style='color:grey40'>all</span>.")
  ) +
  theme_minimal(base_family = body_font) +
  theme(
    text = element_text(
      colour = text_col
    ),
    strip.text = element_blank(),
    legend.position = "none",
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10, l = 10),
      lineheight = 0.5,
      family = body_font
    ),
    axis.text.x = element_text(
      size = rel(0.8),
      hjust = 1
    ),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(5, 10, 5, 0)
  )

ggplot2::ggsave(
  filename = "Stack Overflow Survey/so_salary.png",
  width = 1200,
  height = 1200,
  units = "px"
)


## Education level --------------------------------------------------------

plot_data |>
  select(job, ed_level) |>
  filter() |>
  drop_na() |>
  left_join(
    filter(
      qname_levels_single_response_crosswalk,
      qname == "ed_level"
    ),
    by = c("ed_level" = "level")
  ) |>
  mutate(
    label = factor(
      label,
      levels = c(
        "Primary/elementary school",
        "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)",
        "Some college/university study without earning a degree",
        "Associate degree (A.A., A.S., etc.)",
        "Bachelor’s degree (B.A., B.S., B.Eng., etc.)",
        "Master’s degree (M.A., M.S., M.Eng., MBA, etc.)",
        "Professional degree (JD, MD, Ph.D, Ed.D, etc.)",
        "Something else"
      ),
      labels = c(
        "Primary school",
        "Secondary school",
        "Some university study without a degree",
        "Associate degree",
        "Bachelor’s degree",
        "Master’s degree",
        "Professional degree",
        "Something else"
      )
    )
  ) |>
  count(job, label) |>
  ggplot() +
  geom_col(
    mapping = aes(
      y = label,
      x = n,
      fill = job
    ),
    position = "fill"
  ) +
  scale_fill_manual(
    values = c(da_col, ds_col)
  ) +
  scale_y_discrete(limits = rev, labels = wrap_format(18)) +
  scale_x_continuous(
    breaks = c(0, 0.5, 1),
    labels = c("0%", "50%", "100%")
  ) +
  labs(
    x = "Percentage of education level group", y = "",
    subtitle = glue("People educated beyond Bachelor's degree level are even more
    likely to be <span style='color:{ds_col}'>data scientists or machine learning engineers</span>
       compared to <span style='color:{da_col}'>data or business analysts</span>.")
  ) +
  theme_minimal(base_family = body_font) +
  theme(
    text = element_text(
      colour = text_col
    ),
    legend.position = "none",
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10, l = 10),
      lineheight = 0.5,
      family = body_font
    ),
    axis.text.y = element_text(
      size = rel(0.8),
      margin = margin(r = -5)
    ),
    panel.grid = element_blank(),
    plot.margin = margin(5, 10, 5, 0)
  )

ggplot2::ggsave(
  filename = "Stack Overflow Survey/so_education.png",
  width = 1200,
  height = 1200,
  units = "px"
)


## Coding experience ------------------------------------------------------

code_data <- plot_data |>
  select(job, years_code) |>
  drop_na() 

code_data |> 
  group_by(job) |> 
  summarise(m = mean(years_code))

code_data |> 
  summarise(m = mean(years_code))

ggplot(
  data = code_data
) +
  geom_density(
    data = select(code_data, -job),
    mapping = aes(
      x = years_code,
      y = after_stat(count),
      fill = "All",
    ),
    color = "transparent"
  ) +
  geom_density(
    mapping = aes(
      x = years_code,
      y = after_stat(count),
      fill = job
    ),
    color = "transparent"
  ) +
  facet_wrap(~job, ncol = 1) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, NA)
  ) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(
    values = c("grey50", da_col, ds_col)
  ) +
  labs(
    x = "Years of coding experience (including education)", y = "",
    subtitle = glue("A <span style='color:{ds_col}'>data scientist or machine learning engineer</span>
    has 12.8 years of coding experience on average, compared 11.2 for a <span style='color:{da_col}'>data or business analyst</span>, 
                    with the <span style='color:grey40'>overall</span> average being 12.3.")
  ) +
  theme_minimal(base_family = body_font) +
  theme(
    text = element_text(
      colour = text_col
    ),
    strip.text = element_blank(),
    legend.position = "none",
    plot.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    panel.background = element_rect(
      fill = bg_col, colour = bg_col
    ),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(
      colour = text_col,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 10, t = 10, l = 10),
      lineheight = 0.5,
      family = body_font
    ),
    axis.text.x = element_text(
      size = rel(0.8),
      hjust = 1
    ),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(5, 10, 5, 0)
  )
ggplot2::ggsave(
  filename = "Stack Overflow Survey/so_coding.png",
  width = 1200,
  height = 1200,
  units = "px"
)
