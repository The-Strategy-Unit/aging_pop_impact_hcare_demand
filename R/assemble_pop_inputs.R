# README
# Assemble population data by area supplied as an input to the app


# packages ----
library("dplyr")
library("here")
library("jsonlite")
library("purrr")
library("readr")
library("stringr")
library("tidyr")

# helpers ----
source(here("R", "helper_app_inputs.R"))

# read ----
pop_dat <- read_rds(here("data", "app_pop_100_inputs.rds"))

# assemble ----
app_dat <- pop_dat |>
  filter(id %in% vars_app) |>
  rename(variant = id, mf = sex) |>
  mutate(
    variant = factor(variant, levels = vars_levels),
    variant = paste0("v", as.integer(variant)),
    # round population to integer
    pop = round(pop)
  ) |>
  # important
  arrange(area_code, area_name, variant, year, age)

# repeat years
# I'm creating a new variant v0 that has population numbers for
# 2010 to 2017. These data are historic so there are no variants
# Currently, I just duplicate 2018 data as a temporary placeholder
# This will need updating with the true population numbers
rep_years <- function(df, start = 2010, end = 2017) {
  base_dat <- df |>
    filter(year == 2018L, variant == "v1")

  no_years <- end - start + 1

  new_dat <- base_dat |>
    mutate(variant = "v0")

  out_dat <- new_dat |>
    bind_rows(df) |>
    arrange(area_code, area_name, variant)

  return(out_dat)
}

app_dat <- rep_years(app_dat)

# make a list
app_ls <- app_dat |> group_split(area_code, area_name)
names(app_ls) <- unique(app_dat$area_code)

# format df ready to save as JSON
format_json <- function(df) {
  df |>
    nest(.by = c(starts_with("area"), variant, year), .key = "data") |>
    mutate(data = map(
      data, \(x) {
        x |>
          pivot_wider(names_from = mf, values_from = pop)
      }
    )) |>
    # add totals
    mutate(totals = map(
      data, \(x) {
        x |>
          select(-age) |>
          summarise(across(c(f, m), \(y) sum(y))) |>
          jsonlite::unbox()
      }
    )) |>
    relocate(data, .after = totals) |>
    select(-area_name)
}

app_ls <- map(app_ls, \(x) format_json(x))

# save ----
walk2(
  app_ls, names(app_ls),
  \(x, y) {
    jsonlite::write_json(
      x,
      here(
        "data",
        "app_inputs",
        "pyramids",
        paste0(y, ".json")
      )
    )
  }
)
