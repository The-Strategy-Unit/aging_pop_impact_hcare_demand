# README ----
# Assemble results from running the health status adjustment model
# omit_grps variable excludes specified hsagrps from the output

# packages ----
library("dplyr")
library("here")
library("purrr")
library("readr")
library("tidyr")
options(dplyr.summarise.inform = FALSE)
options(readr.show_progress = FALSE)

# helpers ----
source(here("R", "helper_utility_fns.R"))
source(here("R", "hsa_build_model.R"))
source(here("R", "hsa_run_model.R"))

# functions ----
# demo_fac
# hsa_fac
# hsa_mode_fac

# demo_fac() ----
# assemble demographic factors
# param: proj, type: string, population projection variant to use
# param: base_year, type: integer, base year for model
# param: end_year, type: integer, future year to produce activity estimate for
demo_fac <- function(area_code, proj, base_year, end_year) {

  path_self <- path_closure({{area_code}}, {{base_year}})

  act <- load_activity_data(path_self, activity_type = "all")

  demo <- load_demographic_factors(path_self, base_year, end_year)
  demo <- demo |>
    filter(id == {{ proj }})

  act |>
    left_join(demo, join_by("sex", "age")) |>
    mutate(end_n = n * demo_adj) |>
    group_by(id, hsagrp, sex) |>
    summarise(base_n = sum(n), end_n = sum(end_n)) |>
    ungroup() |>
    mutate(end_p = end_n / base_n) |>
    arrange(hsagrp, sex)
}

# hsa_fac() ----
# assemble hsa adjusted factors
# param: proj, type: string, population projection variant to use
# param: base_year, type: integer, base year for model
# param: end_year, type: integer, future year to produce activity estimate for
# returns: a datframe of modeled activity numbers and per cent change from base
# year by hsagrp and sex, rtype: rtype: df (vector columns)
hsa_fac <- function(
  area_code,
  proj,
  base_year,
  end_year,
  model_runs,
  rng_state,
  method = c("interp", "gams")
) {

  # check method argument
  method <- rlang::arg_match(method)

  path_self <- path_closure({{area_code}}, {{base_year}})

  omit_grps <- c(
    "apc_birth_n",
    "apc_birth_bds",
    "apc_mat_n",
    "apc_mat_bds",
    "apc_paeds-elec_n",
    "apc_paeds-elec_bds",
    "apc_paeds-emer_n",
    "apc_paeds-emer_bds",
    "apc_reg_n",
    "apc_xfer_n",
    "apc_xfer_bds"
  )

  act <- load_activity_data(path_self, activity_type = "all")

  demo <- load_demographic_factors(path_self, base_year, end_year)
  demo <- demo |>
    filter(id == {{ proj }})

  hsa <- run_hsa(
    area_code,
    proj,
    base_year,
    end_year,
    model_runs,
    rng_state,
    method = method
  )

  act |>
    left_join(demo, join_by("sex", "age")) |>
    left_join(hsa, join_by("hsagrp", "sex", "age")) |>
    filter(!hsagrp %in% omit_grps) |>
    # replace missing (empty lists) hsa factors with 1
    mutate(
      f = map_if(f,
        .p = \(x) length(x) == 0,
        .f = \(x) c(rep(1, model_runs))
      )
    ) |>
    mutate(f = map2(f, demo_adj, \(x, y) x * y)) |>
    mutate(end_n = map2(n, f, \(x, y) x * y)) |>
    select(hsagrp, sex, age, n, end_n) |>
    group_by(hsagrp, sex) |>
    nest(.key = "data") |>
    ungroup() |>
    mutate(base_n = map_dbl(data, \(x) sum(x$n))) |>
    mutate(end_n = map(data, \(x) rowSums(sapply(x$end_n, unlist)))) |>
    select(hsagrp, sex, base_n, end_n) |>
    mutate(end_p = map2(base_n, end_n, \(x, y) y / x))
}

# hsa_mode_fac() ----
# assemble hsa adjusted modal factors
# param: proj, type: string, population projection variant to use
# param: base_year, type: integer, base year for model
# param: end_year, type: integer, future year to produce activity estimate for
hsa_mode_fac <- function(
  area_code,
  proj,
  base_year,
  end_year,
  method = c("interp", "gams")
) {

  # check method argument
  method <- rlang::arg_match(method)

  path_self <- path_closure({{area_code}}, {{base_year}})

  omit_grps <- c(
    "apc_birth_n",
    "apc_birth_bds",
    "apc_mat_n",
    "apc_mat_bds",
    "apc_paeds-elec_n",
    "apc_paeds-elec_bds",
    "apc_paeds-emer_n",
    "apc_paeds-emer_bds",
    "apc_reg_n",
    "apc_xfer_n",
    "apc_xfer_bds"
  )

  act <- load_activity_data(path_self, activity_type = "all")

  demo <- load_demographic_factors(path_self, base_year, end_year)
  demo <- demo |>
    filter(id == {{ proj }})

  hsa <- run_hsa_mode(area_code, proj, base_year, end_year, method = method)

  act |>
    left_join(demo, join_by("sex", "age")) |>
    left_join(hsa, join_by("hsagrp", "sex", "age")) |>
    filter(!hsagrp %in% omit_grps) |>
    # replace NA hsa factors with demographic factors
    mutate(f = case_when(is.na(f) ~ demo_adj, TRUE ~ demo_adj * f)) |>
    mutate(end_n = n * f) |>
    select(hsagrp, sex, age, n, end_n) |>
    group_by(hsagrp, sex) |>
    summarise(base_n = sum(n), end_n = sum(end_n)) |>
    ungroup() |>
    mutate(end_p = end_n / base_n)
}
