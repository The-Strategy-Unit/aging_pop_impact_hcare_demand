# README
# Create a set of x17 custom snpp variants
# x4 variant projections were published for 2018b snpp
# x17 variant projections were published for 2018b npp
# Custom snpp variants are created by applying the % difference between npp
# variants and the npp principal (by age/sex/year) to the snpp principal


# packages ----
library("dplyr")
library("here")
library("purrr")
library("readr")
library("stringr")
library("tidyr")
library("testthat")

# create custom variants ----
custom_vars_snpp <- function(npp, snpp) {
  npp_dat <- read_rds(here("data", npp))
  snpp_dat <- read_rds(here("data", snpp))

  npp_dat <- read_rds(here("data", "npp_2018b.rds"))
  snpp_dat <- read_rds(here("data", "snpp_2018b.rds"))

  ref <- npp_dat |>
    filter(year %in% as.character(2018:2043)) |> # snpp end at 2043
    mutate(age = case_when(age > 90 ~ 90L, TRUE ~ as.integer(age))) |>
    group_by(across(c(-area, -pop))) |> # re-sum by age
    summarise(pop = sum(pop)) |>
    ungroup()

  mx <- ref |>
    left_join(
      ref |> filter(id == "ppp") |> rename(ppp = pop) |> select(-id),
      by = c("year", "sex", "age")
    ) |>
    mutate(mx = pop / ppp) |>
    ungroup() |>
    filter(id != "ppp") |>
    select(-ppp, -pop)

  custom <- map(mx |> group_split(id), \(x) {
    x |>
      left_join(
        snpp_dat |>
          filter(id == "principal_proj") |>
          select(-id),
        join_by("year", "sex", "age"), multiple = "all"
      ) |>
      mutate(new_pop = mx * pop, .before = pop) |>
      select(-pop, -mx) |>
      rename(pop = new_pop)
  }) |>
    list_rbind()

  # run some tests
  source(here("R/tests", "test_build_snpp_2018b_custom_variants.R"), local = TRUE) # nolint: line_length_linter.

  # write
  write_rds(custom, here("data", "snpp_2018b_custom_variants.rds"))
}

custom_vars_snpp("npp_2018b.rds", "snpp_2018b.rds")
