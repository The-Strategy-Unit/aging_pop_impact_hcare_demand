# README
# Build a set of historic area populations (0-90+) for use in the app
# requires reconciling area codes with local government changes

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
library("testthat")
library("tidyr")

# read ----
historic_pop_obj_ls <- read_rds(here("data", "historic_pop_obj_ls.rds"))
lookup_lad18_lad23 <- read_csv(here("data", "lookup_lad2018_lad2023.csv"))
lookup_lad23_cty <- read_csv(here("data", "lookup_lad2023_cty.csv"))
lookup_lad23_icb <- read_csv(here("data", "lookup_lad2023_icb.csv"))
lad23 <- read_csv(here("data", "local_authority_districts_2023.csv"))

# wrangle ----
# apportion lad 85+ population to ages 85-90+
lad_tot <- historic_pop_obj_ls[["lad_00"]] |>
  group_by(area_code, year, sex) |>
  complete(age = full_seq(86:90, 1L), pop = last(pop)) |>
  rename(total = pop) |>
  ungroup()

eng_pct <- historic_pop_obj_ls[["eng_00"]] |>
  group_by(year, sex) |>
  mutate(total = sum(pop)) |>
  group_by(year, sex, age) |>
  summarise(pct = pop / total) |>
  ungroup()

lad_new <- lad_tot |>
  left_join(eng_pct, join_by(year, sex, age)) |>
  mutate(pop = pct * total) |>
  select(-pct, -total) |>
  arrange(area_code, year, sex, age)

ts_mye <- historic_pop_obj_ls[["ts_mye"]] |>
  left_join(lad_new, join_by(area_code, year, sex, age)) |>
  mutate(pop = if_else(is.na(pop.x), pop.y, pop.x)) |>
  select(-pop.x, -pop.y)

# apportion lad 90+ to ages 90-100+
voc_pct <- historic_pop_obj_ls[["ts_voc"]] |>
  group_by(year, sex) |>
  mutate(total = sum(pop)) |>
  group_by(year, sex, age) |>
  summarise(pct = pop / total)

ts_mye_90plus <- ts_mye |>
  filter(age == 90) |>
  group_by(area_code, year, sex) |>
  complete(age = full_seq(91:100, 1L), pop = last(pop)) |>
  rename(total = pop) |>
  left_join(voc_pct, join_by(year, sex, age)) |>
  group_by(area_code, year, sex) |>
  mutate(pop = pct * total) |>
  ungroup() |>
  select(-pct, -total) |>
  arrange(area_code, year, sex, age)

ts_mye_90minus <- ts_mye |>
  filter(age < 90)

ts_mye_new <- ts_mye_90minus |>
  bind_rows(ts_mye_90plus) |>
  arrange(area_code, year, sex, age)

# test ----
source(here("R/tests", "test_build_historic_pop.R"))

# reconcile local government changes ----
# WARNING this can easily become a rabbit hole!
# historic mye timseries has 309 lads - map to 296 lads (ONS Apr 2023)
mye_lad <- ts_mye_new |>
  left_join(lookup_lad18_lad23, join_by(area_code == lad18cd)) |>
  mutate(
    area_code = case_when(!is.na(new_ladcd) ~ new_ladcd, TRUE ~ area_code)
  ) |>
  select(-contains("lad"), -yrofchg) |>
  group_by(across(-pop)) |>
  summarise(pop = sum(pop, na.rm = TRUE)) |>
  ungroup() |>
  # pull area name
  left_join(lad23, join_by("area_code" == "lad23cd")) |>
  rename(area_name = lad23nm) |>
  select(area_code, area_name, everything())

# compile countys
mye_cty <- lookup_lad23_cty |>
  left_join(mye_lad, join_by(lad23cd == area_code)) |>
  group_by(across(starts_with("cty")), year, sex, age) |>
  summarise(pop = sum(pop)) |>
  ungroup() |>
  rename(area_code = cty23cd, area_name = cty23nm)

# compile icbs
mye_icb <- lookup_lad23_icb |>
  left_join(mye_lad, join_by(lad23cd == area_code)) |>
  group_by(across(starts_with("icb")), year, sex, age) |>
  summarise(pop = sum(pop)) |>
  ungroup() |>
  rename(area_code = icb23cd, area_name = icb23nm)

# compile England
mye_eng <- mye_lad |>
  group_by(year, sex, age) |>
  summarise(pop = sum(pop)) |>
  ungroup() |>
  mutate(
    area_code = "E92000001",
    area_name = "England",
    .before = everything()
  )

# save ----
historic_pop_inputs_ls <- list(mye_lad, mye_cty, mye_icb, mye_eng)
names(historic_pop_inputs_ls) <- c("mye_lad", "mye_cty", "mye_icb", "mye_eng")

write_rds(historic_pop_inputs_ls, here("data", "historic_pop_inputs_ls.rds"))
