# README
# Build a set of area populations (0-100+) for use in the app, requires
# reconciling area codes with local government changes
# snpp 2018b stop at age 90+, npp 2018b stop at age 104 (105-109, 110+)
# We take the 90+ distribution from npp and apply it to snpp to create modeled
# snpp populations for ages 0-100+
# Which npp variant should the 90+ distribution come from? Ideally, there exists
# a 1:1 mapping of npp variants to snpp variants. However no mapping exists,so
# we create our own


# packages ----
library("dplyr")
library("dtplyr")
library("here")
library("purrr")
library("readr")
library("stringr")
library("testthat")
library("tidyr")

# model syoa ----
apply_90plus_dist <- function(npp, snpp) {
  # mapping from snpp variants to npp variants
  var_map <- read_csv(here("data", "lookup_proj_vars.csv"))

  npp_dat <- read_rds(here("data", npp))
  snpp_dat <- read_rds(here("data", snpp))

  # extract 90+ distribution from npp
  npp_plus90 <- npp_dat |>
    filter(year <= 2043) |>
    group_by(id) |>
    nest() |>
    ungroup() |>
    mutate(
      npp_p90 = map(data, \(x) {
        x |>
          filter(age >= 90) |>
          mutate(age = case_when(
            age > 100 ~ 100L,
            TRUE ~ as.integer(age)
          )) |>
          group_by(across(-c(pop))) |>
          summarise(pop = sum(pop)) |>
          group_by(across(-c(age, pop))) |>
          mutate(pop_pct = pop / sum(pop)) |>
          ungroup() |>
          select(year, sex, age, pop_pct)
      })
    ) |>
    select(-data)

  # apply npp 90+ distribution to snpp
  out_dat <- snpp_dat |>
    group_by(id) |>
    nest() |>
    mutate(
      snpp_p90 = map(data, \(x) {
        x |>
          filter(age == 90L) |>
          group_by(area_code, area_name, sex, year) |>
          complete(age = 90:100)
      })
    ) |>
    left_join(var_map, join_by("id" == "proj_id")) |>
    left_join(npp_plus90, join_by("proj_map" == "id")) |>
    mutate(
      snpp_p90 = map2(snpp_p90, npp_p90, \(x, y) {
        x |>
          left_join(y, join_by("year", "sex", "age")) |>
          group_by(area_code, area_name, sex, year) |>
          mutate(pop = pop_pct * sum(pop, na.rm = TRUE)) |>
          ungroup() |>
          select(-pop_pct)
      })
    ) |>
    select(-npp_p90, -starts_with("proj"), -ex_id) |>
    ungroup() |>
    mutate(
      data = map(data, \(x) {
        x |>
          filter(age != 90)
      })
    ) |>
    mutate(
      data = map2(data, snpp_p90, \(x, y) bind_rows(x, y))
    ) |>
    select(-snpp_p90) |>
    unnest(c(data))

  # run some tests
  # source() code is evaluated in the global environment by default
  # set local = TRUE to evaluate the code in the calling environment
  source(here("R/tests", "test_build_pop_100_inputs.R"), local = TRUE)

  # write out
  write_rds(out_dat, here("data", "app_pop_100_inputs.rds"))
}

apply_90plus_dist("npp_2018b.rds", "app_pop_90_inputs.rds")
