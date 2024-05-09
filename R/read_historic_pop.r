# README
# Read-in historic mid-year population estimates, for years 2000 to 2022
# https://www.nomisweb.co.uk
# Read-in historic estimates of the very old including centenarians, for years 2002 to 2022 # nolint: line_length_linter.
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/ageing/datasets/midyearpopulationestimatesoftheveryoldincludingcentenariansengland # nolint: line_length_linter.

# IMPORTANT
# i) mye estimates for lads are for 0-85+ in 2000
# ii) very old estimates not available for 2000 and 2001
# i) we use England figures to apportion lad 85+ population to ages 85-90+
# ii) we use England figures for 2002 to apportion lad 90+ population in 2000
# and 2001 to ages 90-100+
# the only place these estimates are used is to specify bar length in the
# population pyramid element of the app


# packages ----
library("dplyr")
library("here")
library("readr")
library("stringr")
library("tidyr")

# read ----
# timeseries mye (from NOMIS)
ts_mye <- read_csv(here(
  "data_raw",
  "nomis_mye_lad_2000_to_2022_20240509.csv"
), na = c("", "NA", "-"), skip = 5)
# England mye 85-90+ for 2000 (from NOMIS)
eng_00 <- read_csv(here(
  "data_raw",
  "nomis_mye_england_85to90plus_2000_20240509.csv"
), na = c("", "NA", "-"), skip = 6)
# lads mye 85+ for 2000 (from NOMIS)
lad_00 <- read_csv(here(
  "data_raw",
  "nomis_mye_lad_85plus_2000_20240509.csv"
), na = c("", "NA", "-"), skip = 5, n_max = 375)
# timeseries very old population estimates (from ONS)
ts_voc <- read_csv(here(
  "data_raw",
  "englandevo2022.csv"
), na = c("", "NA", "-"), skip = 3, n_max = 66)

# clean ---
start_year <- 2000L

ts_mye <- ts_mye |>
  rename(
    area_name = `local authority: district / unitary (as of April 2021)`,
    area_code = mnemonic,
    row_id = row
  ) |>
  filter(
    !is.na(area_name),
    str_detect(area_code, "^E0")
  ) |>
  mutate(row_id = as.integer(row_id)) |>
  mutate(
    tbl_id = cumsum(row_id == 1),
    year = if_else(tbl_id %% 2 == 1, (tbl_id + 1) / 2, tbl_id / 2),
    year = year + start_year - 1,
    sex = if_else(tbl_id %% 2 == 1, "m", "f"),
  ) |>
  select(-row_id, -tbl_id, -area_name) |>
  pivot_longer(starts_with("Age"), names_to = "age", values_to = "pop") |>
  mutate(age = str_extract(age, "[0-9]+")) |>
  mutate(
    across(c("year", "age"), as.integer),
    pop = as.double(pop)
  )

ts_voc <- ts_voc |>
  select(-1) |>
  rename(year = 1) |>
  filter(!is.na(year)) |>
  mutate(tbl_id = cumsum(year == 2002)) |>
  filter(tbl_id %in% c(2, 3)) |>
  mutate(sex = if_else(tbl_id == 2L, "m", "f")) |>
  select(-tbl_id, -`90 & over`, -`90-99`, -starts_with("1"), `100 & over`) |>
  rename(`100` = `100 & over`) |>
  pivot_longer(matches("[1,9]"), names_to = "age", values_to = "pop") |>
  mutate(across(c("year", "age"), as.integer))

# repeat very old for missing years 2000 and 2001
ts_voc <- ts_voc |>
  group_by(sex, age) |>
  complete(year = full_seq(2000:2001, 1L), pop = first(pop)) |>
  ungroup()

eng_00 <- eng_00 |>
  filter(!is.na(Age)) |>
  pivot_longer(Male:Female, names_to = "sex", values_to = "pop") |>
  mutate(
    year = 2000L,
    age = as.integer(str_extract(Age, "[0-9]{2}")),
    sex = if_else(sex == "Male", "m", "f")
  ) |>
  select(year, age, sex, pop)

lad_00 <- lad_00 |>
  rename(
    area_name = `local authority: district / unitary (as of April 2021)`,
    area_code = mnemonic
  ) |>
  filter(
    !is.na(area_name),
    str_detect(area_code, "^E0")
  ) |>
  pivot_longer(Male:Female, names_to = "sex", values_to = "pop") |>
  mutate(
    age = 85L,
    year = 2000L,
    sex = if_else(sex == "Male", "m", "f")
  ) |>
  select(-area_name)

# save ----
historic_pop_obj_ls <- list(ts_mye, ts_voc, eng_00, lad_00)
names(historic_pop_obj_ls) <- c("ts_mye", "ts_voc", "eng_00", "lad_00")

write_rds(historic_pop_obj_ls, here("data", "historic_pop_obj_ls.rds"))
