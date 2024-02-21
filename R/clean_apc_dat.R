# README
# Clean apc data


# packages ----
library("dplyr")
library("here")
library("readr")
library("tidyr")

# read ----
apc_dat <- read_rds(here("data", "apc_dat_2022.rds"))
lookup_lad18_lad23 <- read_csv(here("data", "lookup_lad2018_lad2023.csv"))
lookup_lad23_cty <- read_csv(here("data", "lookup_lad2023_cty.csv"))
lookup_lad23_icb <- read_csv(here("data", "lookup_lad2023_icb.csv"))
lad23 <- read_csv(here("data", "local_authority_districts_2023.csv"))

# clean ----
apc_dat <- apc_dat |>
  # remove under 17s
  filter(age >= 17) |>
  # set upper age group to 90+
  mutate(age = case_when(age >= 90 ~ 90, TRUE ~ as.double(age))) |>
  rename(area_code = lacd) |>
  mutate(across(c(age, n), as.integer)) |>
  group_by(area_code, sex, age, admigrp) |>
  summarise(n = sum(n), bds = sum(bds)) |>
  ungroup() |>
  pivot_longer(cols = c(n, bds), names_to = "units", values_to = "n") |>
  mutate(hsagrp = paste(sep = "_", "apc", admigrp, units)) |>
  select(-admigrp) |>
  # omit daycase and regular attender beddays
  filter(!hsagrp %in% c("apc_daycase_bds", "apc_reg_bds"))

# reconcile local government changes ----
# WARNING this can easily become a rabbit hole!
# apc for 2022 has 313 lads - map to 296 lads (ONS Apr 2023)
apc_lad <- apc_dat |>
  select(-units) |>
  complete(
    area_code, hsagrp,
    nesting(sex, age),
    fill = list(n = NA),
    explicit = FALSE
  ) |>
  left_join(lookup_lad18_lad23, join_by("area_code" == "lad18cd")) |>
  mutate(
    area_code = case_when(!is.na(new_ladcd) ~ new_ladcd, TRUE ~ area_code)
  ) |>
  select(-contains("lad"), -yrofchg) |>
  group_by(across(-n)) |>
  summarise(n = sum(n, na.rm = TRUE)) |>
  ungroup() |>
  # pull area name
  left_join(lad23, join_by("area_code" == "lad23cd")) |>
  rename(area_name = lad23nm) |>
  select(area_code, area_name, everything())

# compile countys
apc_cty <- lookup_lad23_cty |>
  left_join(apc_lad, join_by("lad23cd" == "area_code")) |>
  group_by(across(starts_with("cty")), hsagrp, sex, age) |>
  summarise(n = sum(n)) |>
  ungroup() |>
  rename(area_code = cty23cd, area_name = cty23nm)

# compile icbs
apc_icb <- lookup_lad23_icb |>
  left_join(apc_lad, join_by("lad23cd" == "area_code")) |>
  group_by(across(starts_with("icb")), hsagrp, sex, age) |>
  summarise(n = sum(n)) |>
  ungroup() |>
  rename(area_code = icb23cd, area_name = icb23nm)

# compile England
apc_eng <- apc_lad |>
  group_by(hsagrp, sex, age) |>
  summarise(n = sum(n)) |>
  ungroup() |>
  mutate(
    area_code = "E92000001",
    area_name = "England",
    .before = everything()
  )

# save ----
apc_all <- bind_rows(apc_lad, apc_cty, apc_icb, apc_eng)
write_rds(apc_all, here("data", "apc_clean_2022.rds"))

apc_all |>
  group_by(grp_var = area_code) |>
  group_walk(
    \(x, y) write_rds(x, here("data", "2022", y$grp_var, "apc_clean.rds"))
  )
