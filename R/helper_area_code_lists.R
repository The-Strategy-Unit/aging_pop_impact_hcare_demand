# README
# Read and save lists of local authority districts and lookups from lads to
# countys. Code lists and lookups sourced from ONS Open Geogrpahy Portal
# https://geoportal.statistics.gov.uk/


# packages ----
library("dplyr")
library("here")
library("readr")
library("stringr")

# read ONS codes ----
lad18 <- read_csv(here(
  "data_raw",
  "LAD_(Dec_2018)_Names_and_Codes_in_the_United_Kingdom.csv"
))
lad22 <- read_csv(here(
  "data_raw",
  "LAD_(Dec_2022)_Names_and_Codes_in_the_United_Kingdom.csv"
))
lad23 <- read_csv(here(
  "data_raw",
  "LAD_(Apr_2023)_Names_and_Codes_in_the_United_Kingdom.csv"
))
cty18 <- read_csv(here(
  "data_raw",
  "Local_Authority_District_to_County_(December_2018)_Lookup_in_England.csv"
))
cty22 <- read_csv(here(
  "data_raw",
  "Local_Authority_District_to_County_(December_2022)_Lookup_in_England.csv"
))
cty23 <- read_csv(here(
  "data_raw",
  "Local_Authority_District_to_County_(April_2023)_Lookup_in_England.csv"
))

# clean codes ----
# districts
lad18 <- lad18 |>
  filter(str_detect(LAD18CD, "E[0-1][6-9]")) |>
  select(-LAD18NMW, -FID) |>
  rename_with(tolower) |>
  write_csv(here("data", "local_authority_districts_2018.csv"))
lad22 <- lad22 |>
  filter(str_detect(LAD22CD, "E[0-1][6-9]")) |>
  select(-LAD22NMW, -ObjectId) |>
  rename_with(tolower) |>
  write_csv(here("data", "local_authority_districts_2022.csv"))
lad23 <- lad23 |>
  filter(str_detect(LAD23CD, "E[0-1][6-9]")) |>
  select(-LAD23NMW, -ObjectId) |>
  rename_with(tolower) |>
  write_csv(here("data", "local_authority_districts_2023.csv"))

# countys
cty18 <- cty18 |>
  filter(str_detect(CTY18CD, "^E10")) |>
  select(-FID) |>
  rename_with(tolower) |>
  write_csv(here("data", "lookup_lad2018_cty.csv"))
cty22 <- cty22 |>
  filter(str_detect(CTY22CD, "^E10")) |>
  select(-ObjectId) |>
  rename_with(tolower) |>
  write_csv(here("data", "lookup_lad2022_cty.csv"))
cty23 <- cty23 |>
  filter(str_detect(CTY23CD, "^E10")) |>
  select(-ObjectId) |>
  rename_with(tolower) |>
  write_csv(here("data", "lookup_lad2023_cty.csv"))

# retired countys
retired <- setdiff(cty18$cty18cd, cty23$cty23cd)

retired_cty <- cty18 |>
  filter(str_detect(cty18cd, str_c(retired, collapse = "|"))) |>
  distinct(cty18cd, cty18nm)

retired_cty |> write_csv(here("data", "retired_ctys_2018_2023.csv"))
