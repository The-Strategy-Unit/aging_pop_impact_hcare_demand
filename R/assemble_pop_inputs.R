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
pop_proj <- read_rds(here("data", "app_pop_100_inputs.rds"))
pop_hist <- read_rds(here("data", "app_historic_pop_inputs.rds"))

# assemble ----
pop_proj <- pop_proj |>
  filter(id %in% vars_app) |>
  left_join(lookup_vars_id, join_by(id == proj_id)) |>
  rename(mf = sex, variant = vars_id) |>
  mutate(
    variant = factor(variant, levels = vars_id_levels),
    # round population to integer
    pop = round(pop)
  ) |>
  # important
  select(-id) |>
  arrange(area_code, area_name, variant, year, age) |>
  # important! pre-2023 will be replaced by mid-year estimates
  filter(year >= 2023L)

pop_hist <- pop_hist |>
  rename(mf = sex) |>
  mutate(
    variant = factor("v0", levels = vars_id_levels),
    # round population to integer
    pop = round(pop)
  )

app_dat <- bind_rows(pop_hist, pop_proj)

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

app_ls <- map(
  app_ls,
  \(x) {
    format_json(x)
  },
  .progress = list(
    clear = TRUE,
    format = "Format JSON files: {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}", # nolint: line_length_linter.
    type = "iterator"
  )
)

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
