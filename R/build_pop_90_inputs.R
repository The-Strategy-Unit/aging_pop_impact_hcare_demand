# README
# Build a set of area populations (0-90+) for use in the app and the model
# requires reconciling area codes with local government changes


# packages ----
library("dplyr")
library("here")
library("purrr")
library("readr")
library("stringr")
library("testthat")
library("tidyr")

# build ----
snpp_2018b <- read_rds(here("data", "snpp_2018b.rds"))
snpp_2018b_custom_vars <- read_rds(
  here("data", "snpp_2018b_custom_variants.rds")
)
npp_2018b <- read_rds(here("data", "npp_2018b.rds"))
lookup_lad18_lad23 <- read_csv(here("data", "lookup_lad2018_lad2023.csv"))
lad23 <- read_csv(here("data", "local_authority_districts_2023.csv"))
lookup_lad23_cty <- read_csv(here("data", "lookup_lad2023_cty.csv"))
retired_cty <- read_csv(here("data", "retired_ctys_2018_2023.csv"))
lookup_lad23_icb <- read_csv(here("data", "lookup_lad2023_icb.csv"))

# remove custom variants that already exist as published snpp variants
# remove principal (ppp), low migration (ppl), and high migration (pph)
cus_vars <- snpp_2018b_custom_vars |>
  filter(!id %in% c("ppp", "ppl", "pph"))

# assemble single list with x4 published snpp variants + 15 non-duplicate custom
# variants, 4 standard variants + 15 custom variants + 1 principal = 20
app_vars <- snpp_2018b |>
  bind_rows(cus_vars)

# reconcile local government changes ----
# WARNING this can easily become a rabbit hole!
# snpp 2018b has 326 lads; this needs to become 296 lads (ONS Apr 2023)
# snpp 2018b has 27 ctys; this needs to become 21 ctys (ONS Apr 2023)

# county councils
app_vars_cty <- app_vars |>
  filter(str_detect(area_code, "^E10")) |>
  anti_join(
    retired_cty, join_by("area_code" == "cty18cd")
  )

# districts
app_vars_lad <- app_vars |>
  # remove all county councils
  filter(str_detect(area_code, "^E10", negate = TRUE)) |>
  left_join(lookup_lad18_lad23, join_by("area_code" == "lad18cd")) |>
  mutate(
    area_code = case_when(!is.na(new_ladcd) ~ new_ladcd, TRUE ~ area_code),
    area_name = case_when(!is.na(new_ladnm) ~ new_ladnm, TRUE ~ area_name)
  ) |>
  select(-starts_with("new"), -yrofchg, -lad18nm) |>
  group_by(across(-pop)) |>
  summarise(pop = sum(pop)) |>
  ungroup()

# compile England
app_vars_eng <- npp_2018b |>
  select(-area) |>
  filter(year <= 2043) |>
  mutate(age = case_when(age > 90 ~ 90, TRUE ~ as.integer(age))) |>
  group_by(across(-pop)) |>
  summarise(pop = sum(pop)) |>
  ungroup() |>
  # match id names in snpp
  mutate(id = case_when(
    id == "ppp" ~ "principal_proj",
    id == "pph" ~ "var_proj_high_intl_migration",
    id == "ppl" ~ "var_proj_low_intl_migration",
    .default = id
  )) |>
  mutate(area_code = "E92000001", area_name = "England", .after = "id")

# compile icbs
app_vars_icb <- lookup_lad23_icb |>
  left_join(app_vars_lad, join_by("lad23cd" == "area_code")) |>
  group_by(across(starts_with("icb")), id, sex, age, year) |>
  summarise(pop = sum(pop)) |>
  ungroup() |>
  rename(area_code = icb23cd, area_name = icb23nm)

# test ----
source(here("R/tests", "test_build_pop_90_inputs.R"))

# save ----
app_vars_all <- bind_rows(
  app_vars_lad,
  app_vars_cty,
  app_vars_icb,
  app_vars_eng
)

# used as input to activity rates element in the app
write_rds(app_vars_all, here("data", "app_pop_90_inputs.rds"))

# used as input to the model
app_vars_all |>
  pivot_wider(names_from = "year", values_from = "pop") |>
  group_by(area_code, area_name) |>
  group_walk(\(x, y) {
    write_rds(x, here("data", "2022", y$area_code, "pop_dat.rds"))
  })
