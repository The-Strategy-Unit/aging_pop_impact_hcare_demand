# README
# Run the health status adjustment model
# age range for health status adjustment is [55, 90]


# packages ----
library("dplyr")
library("here")
library("purrr")
library("readr")
library("rlang")
library("tidyr")
options(dplyr.summarise.inform = FALSE)
options(readr.show_progress = FALSE)

# helpers ----
source(here("R", "helper_utility_fns.R"))
source(here("R", "hsa_build_model.R"))

# functions ----
# run_hsa
# run_hsa_mode

# run_hsa() ----
# run the health status adjustment model
# param: proj, type: string, population projection variant to use
# param: base_year, type: integer, base year for model
# param: end_year, type: integer, future year to produce activity estimate for
# param: model_runs, type: integer, number of times to run model
# param: rng_state, type: integer vector, RNG state
# param: method, type: string, either 'interp' or 'gams', interpolate from the
# gams or use the gams to predict activity, 'gams' will be slower
# returns: a dataframe of hsa factors, rtype: df (list column)
run_hsa <- function(
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

  # load life table lookup
  load_proj_lookup()

  ex_id <- lookup_proj |>
    filter(proj_id == {{ proj }}) |>
    pull(ex_id)

  ex_chg <- load_life_expectancy_series(base_year, end_year)
  ex_chg <- split(
    ex_chg |>
      filter(var == ex_id),
    ~sex
  )
  ex_chg <- map(ex_chg, \(x) x |> pull(ex_chg))

  hsa_params <- create_hsa_params(
    end_year,
    var = ex_id,
    model_runs,
    rng_state
  )

  # age range for health status adjustment
  hsa_age_range <- seq.int(55, 90)

  # adjusted ages (females and males), length = model runs for each age in
  # hsa_age_range
  adj <- map2(ex_chg, hsa_params, \(x, y) map(x, \(x) x * y))
  adjusted_ages <- map(adj, \(x) {
    map2(x, hsa_age_range, \(x, y) {
      list(age = y - x)
    })
  })

  chron_age_rts <- load_activity_rt_tbl(path_self)

  chron_age_rts <- split(
    chron_age_rts |>
      filter(age %in% hsa_age_range),
    ~sex
  )

  if (method == "gams") {
    p <- predict_activity_gam(path_self, adjusted_ages)
  } else {
    p <- predict_activity_interpolate(path_self, adjusted_ages)
  }

  f <- chron_age_rts$f |>
    group_by(hsagrp) |>
    nest(.key = "data") |>
    ungroup() |>
    mutate(p = p$f) |>
    unnest(c(data, p)) |>
    mutate(f = map2(p, gam_rt, \(x, y) x / y)) |>
    select(-gam_rt, -p)

  m <- chron_age_rts$m |>
    group_by(hsagrp) |>
    nest(.key = "data") |>
    ungroup() |>
    mutate(p = p$m) |>
    unnest(c(data, p)) |>
    mutate(f = map2(p, gam_rt, \(x, y) x / y)) |>
    select(-gam_rt, -p)

  bind_rows(f, m)
}

# run_hsa_mode() ----
# run the model to return only the modal estimate
# param: proj, type: string, population projection variant to use
# param: base_year, type: integer, base year for model
# param: end_year, type: integer, future year to produce activity estimate for
# param: method, type: string, either 'interp' or 'gams', interpolate from the
# gams or use the gams to predict activity, 'gams' will be slower
# returns: a dataframe of hsa factors, rtype: df (vector column)
run_hsa_mode <- function(
  area_code,
  proj,
  base_year,
  end_year,
  method = c("interp", "gams")
) {

  # check method argument
  method <- rlang::arg_match(method)

  path_self <- path_closure({{area_code}}, {{base_year}})

  # load life table lookup
  load_proj_lookup()

  ex_id <- lookup_proj |>
    filter(proj_id == {{ proj }}) |>
    pull(ex_id)

  ex_chg <- load_life_expectancy_series(base_year, end_year)
  ex_chg <- split(
    ex_chg |>
      filter(var == ex_id),
    ~sex
  )
  ex_chg <- map(ex_chg, \(x) x |> pull(ex_chg))

  hsa_mode <- create_hsa_mode(end_year, ex_id)

  # age range for health status adjustment
  hsa_age_range <- seq.int(55, 90)

  # adjusted ages (females and males), length = model runs for each age in
  # hsa_age_range
  adj <- map2(ex_chg, hsa_mode, \(x, y) map(x, \(x) x * y))
  adjusted_ages <- map(adj, \(x) {
    map2(x, hsa_age_range, \(x, y) {
      list(age = y - x)
    })
  })

  chron_age_rts <- load_activity_rt_tbl(path_self)

  chron_age_rts <- split(
    chron_age_rts |>
      filter(age %in% hsa_age_range),
    ~sex
  )

  if (method == "gams") {
    p <- predict_activity_gam(path_self, adjusted_ages)
  } else {
    p <- predict_activity_interpolate(path_self, adjusted_ages)
  }

  f <- chron_age_rts$f |>
    group_by(hsagrp) |>
    nest(.key = "data") |>
    ungroup() |>
    mutate(p = p$f) |>
    unnest(c(data, p)) |>
    unnest(p) |>
    mutate(f = p / gam_rt) |>
    select(-gam_rt, -p)

  m <- chron_age_rts$m |>
    group_by(hsagrp) |>
    nest(.key = "data") |>
    ungroup() |>
    mutate(p = p$m) |>
    unnest(c(data, p)) |>
    unnest(p) |>
    mutate(f = p / gam_rt) |>
    select(-gam_rt, -p)

  bind_rows(f, m)
}
