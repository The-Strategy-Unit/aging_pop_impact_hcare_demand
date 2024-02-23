# README
# Assemble population data by area supplied as an input to the app


# packages ----
library("dplyr")
library("here")
library("purrr")
library("readr")
library("stringr")
library("tidyr")

# helpers ----
source(here("R", "helper_app_inputs.R"))

# read ----
pop_dat <- read_rds(here("data", "app_pop_100_inputs.rds"))

# assemble ----
# switch to German variable names
age_levels <- str_c("Bev_", 0:100, "_", 0:100 + 1)

app_dat <- pop_dat |>
  rename(Variante = id, mw = sex, Simulationsjahr = year) |>
  mutate(
    age = str_c("Bev_", age, "_", age + 1),
    age = factor(age, levels = age_levels)
  ) |>
  mutate(mw = case_match(mw, "f" ~ "w", .default = "m")) |>
  group_by(area_code, area_name, Variante, mw, age) |>
  pivot_wider(names_from = "age", values_from = "pop") |>
  ungroup() |>
  mutate(Bev = rowSums(pick(starts_with("Bev"))), .before = Bev_0_1) |>
  arrange(area_code, area_name, Variante, Simulationsjahr, mw)

# filter on variants
app_dat <- app_dat |>
  filter(area_code %in% test_areas) |>
  filter(Variante %in% vars_app) |>
  mutate(Variante = factor(Variante, levels = vars_levels)) |>
  mutate(Variante = as.integer(Variante)) |>
  arrange(area_code, area_name, Variante)

# repeat years
# 1) I'm creating a new variant (numbered zero) that has population numbers for
# 2010 to 2017 - these are historic so just one set (no variants), repeats the
# numbers from 2018
# 2) I'm adding rows to all variants for 2044 to 2050 that repeat the numbers
# from 2043
rep_years <- function(df) {
  df_start <- df |>
    filter(Simulationsjahr == 2018L, Variante == 1L)

  # repeating 8 years
  df_start <- map(seq_len(8), ~ df_start) |>
    list_rbind() |>
    arrange(area_code, area_name) |>
    group_by(area_code, area_name) |>
    # repeating 8 years x 2 sexes
    slice(1:16) |>
    mutate(Simulationsjahr = rep(2010:2017, each = 2)) |>
    ungroup() |>
    mutate(Variante = 0)

  df_end <- df |>
    filter(Simulationsjahr == 2043L)

  # repeating 7 years
  df_end <- map(seq_len(7), ~ df_end) |>
    list_rbind() |>
    arrange(area_code, area_name) |>
    group_by(area_code, area_name, Variante) |>
    # repeating 7 years x 2 sexes
    slice(1:14)  |>
    mutate(Simulationsjahr = rep(2044:2050, each = 2)) |>
    ungroup()

  df_out <- df_start |>
    bind_rows(df) |>
    bind_rows(df_end) |>
    arrange(area_code, area_name, Variante)

  return(df_out)
}

app_dat <- rep_years(app_dat)

app_dat <- app_dat |>
  mutate(across(starts_with("Bev"), \(x) round(x, digits = 3)))

# save ----
app_dat |>
  group_by(area_code, area_name) |>
  group_walk(\(x, y) {
    write_csv(
      x,
      here(
        "data",
        "app_inputs",
        paste0("test_population_", y$area_code[[1]], ".csv")
      )
    )
  })
