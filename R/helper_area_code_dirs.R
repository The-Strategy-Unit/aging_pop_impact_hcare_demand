# README
# Create area code directories


# packages ----
library("dplyr")
library("here")
library("purrr")
library("readr")
library("stringr")
library("tidyr")

# read ----
lad23 <- read_csv(here("data", "local_authority_districts_2023.csv"))
lookup_lad23_cty <- read_csv(here("data", "lookup_lad2023_cty.csv"))
lookup_lad23_icb <- read_csv(here("data", "lookup_lad2023_icb.csv"))

# create dirs ----
dirs_lad <- unique(lad23$lad23cd)
dirs_cty <- unique(lookup_lad23_cty$cty23cd)
dirs_icb <- unique(lookup_lad23_icb$icb23cd)
dir_eng <- "E92000001"

dirs <- c(dirs_lad, dirs_cty, dirs_icb, dir_eng)

# run this only once
# map(dirs, \(x) dir.create(here("data", "2022", x)))

# A-Z list of areas ----
nats <- tibble(cd = "E92000001", nm = "England")
icbs <- lookup_lad23_icb |>
  distinct(icb23cd, icb23nm) |>
  rename_with(\(x) str_extract(x, "[a-z]{2}$")) |>
  mutate(nm = str_c(str_extract(nm, "(?<=\\s).*(?=\\sIntegrated)"), " ICB"))
lads <- lookup_lad23_icb |>
  distinct(lad23cd, lad23nm) |>
  rename_with(\(x) str_extract(x, "[a-z]{2}$"))
ctys <- lookup_lad23_cty |>
  distinct(cty23cd, cty23nm) |>
  rename_with(\(x) str_extract(x, "[a-z]{2}$"))

az_areas <- bind_rows(icbs, lads, ctys) |>
  arrange(nm)

# add England as first/default area
az_areas <- nats |>
  bind_rows(az_areas)

az_areas |> write_csv(here("data", "app_inputs", "area_names_and_codes.csv"))

# hierarchical list of areas ----
# not used
# England not included
hier_areas <- lookup_lad23_icb |>
  left_join(lookup_lad23_cty, join_by(lad23cd, lad23nm)) |>
  select(icb23cd, icb23nm, cty23cd, cty23nm, lad23cd, lad23nm) |>
  group_by(icb23cd, icb23nm) |>
  nest(.key = "by_icb") |>
  mutate(by_icb = map(by_icb, \(x) {
    x |>
      group_by(cty23cd, cty23nm) |>
      nest(.key = "by_cty") |>
      ungroup()
  }
  )) |>
  ungroup() |>
  unnest(cols = c(by_icb)) |>
  unnest(cols = c(by_cty))
