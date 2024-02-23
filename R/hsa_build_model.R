# README
# Applies logic for health status adjustment
# age range for health status adjustment is [55, 90]


# packages ----
library("dplyr")
library("here")
library("mgcv")
library("purrr")
library("readr")
library("rlang")
library("tidyr")

# helpers ----
source(here("R", "hsa_build_gams.R"))

# functions -----
# path_data
# load_proj_lookup
# load_demographic_factors
# load_life_expectancy_series
# random_split_norm
# create_hsa_params
# create_hsa_mode
# load_activity_data
# load_activity_rt_tbl
# predict_activity_gam
# predict_activity_interpolate

# path_data() ----
# helper function for path to data directory
# param: filename, type: string, name of a data file to load
path_data <- function(filename) {
  here("data", filename)
}

# load_proj_lookup() ----
# load lookup for population projection variant to life table variant
load_proj_lookup <- function() {
  # assign to global environment
  lookup_proj <<- read_csv(
    path_data("lookup_proj_vars.csv"),
    show_col_types = FALSE
  )
}

# load_demographic_factors() ----
# load population data and calculate demographic change factors
# change factors are returned for all variants
# param: start_year, type: int, start year for change factors
# param: end_year, type: int, end year for change factors
# returns: a dataframe of demographic change factors, rtype: df
load_demographic_factors <- function(path_self, start_year, end_year) {

  lookup_proj <- load_proj_lookup()

  read_rds(path_self("pop_dat.rds")) |>
    mutate(demo_adj = !!as.name(end_year) / !!as.name(start_year)) |>
    select(id, sex, age, demo_adj) |>
    left_join(lookup_proj, join_by("id" == "proj_id"))
}

# load_life_expectancy_series() ----
# load life expectancy data and calculate change
# changes are returned for all variants
# param: start_year, type: int, start year for change
# param: end_year, type: int, end year for change
# returns: a dataframe of life expectancy changes, rtype: df
load_life_expectancy_series <- function(start_year, end_year) {

  # age range for health status adjustment
  hsa_age_range <- seq.int(55, 90)

  ex_dat <- read_rds(path_data("npp_2018b_ex.rds")) |>
    # use period life expectancies
    # select rows for ages we are interested in
    filter(
      type == "period",
      age %in% hsa_age_range
    )

  ex_dat |>
    filter(year %in% c({{ start_year }}, {{ end_year }})) |>
    group_by(var, sex, age) |>
    pivot_wider(names_from = year, values_from = ex) |>
    summarise(ex_chg = !!as.name(end_year) - !!as.name(start_year)) |>
    ungroup()
}

# random_split_norm() ----
# helper function to draw random values from a split normal distribution
# param: n, type: integer, number of observations
# param: mode, type: double, mode
# param: sd1, type: double, left-hand-side standard deviation
# param: sd2, type: double, right-hand-side standard deviation
# param: rng_state, type: integer vector, RNG state
# returns: n random number values sampled from the split normal distribution,
# rtype: vector
random_split_norm <- function(n, mode, sd1, sd2, rng_state) {

  # get the probability of the mode
  A <- sqrt(2 / pi) / (sd1 + sd2) # nolint: object_name_linter.
  a_sqrt_tau <- A * sqrt(2 * pi)
  p <- (a_sqrt_tau * sd1) / 2

  # generate n random uniform values
  set.seed(seed = rng_state)
  u <- runif(n = n)

  # whether u is less than the mode or not
  a1 <- u <= p

  # make a single sd vector
  sd <- if_else(a1, sd1, sd2)
  x <- if_else(a1, 0, a_sqrt_tau * sd2 - 1)

  return(mode + sd * qnorm(p = (u + x) / (a_sqrt_tau * sd)))
}

# create_hsa_params() ----
# draw random values from a split normal distribution
# param: end_year, type: integer, future year to produce activity estimate for
# param: var, type: string, life table variant, either 'ppp', 'lle' or 'hle'
# param: model_runs, type: integer, number of times to run model
# param: rng_state, type: integer vector, RNG state
# returns: parameters for the health status adjustment, rtype: list of 2
# (females and males), length = model_runs
create_hsa_params <- function(end_year, var, model_runs, rng_state) {

  sp_norm_params <- read_csv(path_data("split_normal_parameters.csv")) |>
    filter(year == {{ end_year }}, var == {{ var }})

  pmap(
    sp_norm_params[, c("mode", "sd1", "sd2")],
    random_split_norm,
    n = model_runs,
    rng_state = rng_state
  )
}

# create_hsa_mode() ----
# return the mode of a split normal distribution
# param: end_year, type: integer, future year to produce activity estimate for
# param: var, type: string, life table variant, either 'ppp', 'lle' or 'hle'
# return: parameters for the health status adjustment, rtype: list of 2
# (females and males), length = 1
create_hsa_mode <- function(end_year, var) {

  sp_norm_params <- read_csv(path_data("split_normal_parameters.csv")) |>
    filter(year == {{ end_year }}, var == {{ var }})

  map(sp_norm_params$mode, \(x) x)
}

# load_activity_data() ----
# load activity data
# param: activity_type, type: string, either 'all', 'aae', 'apc' or 'opc'
# returns: a dataframe of activity data, rtype: df
load_activity_data <- function(path_self, activity_type = c("all", "aae", "apc", "opc")) {

  # check activity_type argument
  activity_type <- rlang::arg_match(activity_type)

  if (activity_type == "all") {
    x <- c("aae", "apc", "opc")
  } else {
    x <- activity_type
  }

  x <- map(x, \(x) read_rds(path_self(paste0(x, "_clean.rds"))))

  bind_rows(x) |> 
    mutate(age = as.integer(age))
}

# load_activity_rt_tbl() ----
# load predicted activity rates from gams
load_activity_rt_tbl <- function(path_self) {
  read_csv(path_self("hsa_activity_rt_tbl.csv"), show_col_types = FALSE)
}

# predict_activity_gam() ----
# predict activity rates using gams for health status adjusted ages
# do this n times, where n = model_runs
# param: adjusted_ages, type: list of 2 (f/m), list of 36 (ages 55-90), vector
# (length = model runs) of health status adjusted ages
# returns: a list of predicted activity rates for supplied adjusted ages,
# rtype: list of 2(f/m), list of 16 (hsagrps), list of 36 (ages 55-90), vector
# (length = model runs)
predict_activity_gam <- function(path_self, adjusted_ages) {

  gams <- read_rds(path_self("hsa_gams.rds"))
  gams <- gams |>
    unnest(gams) |>
    split(~sex)

  # predict.gam returns an array (convert to vector)
  f <- map(gams$f$gams, \(x) {
    map(adjusted_ages[[1]], \(y) {
      as.vector(mgcv::predict.gam(x, newdata = y))
    })
  })

  m <- map(gams$m$gams, \(x) {
    map(adjusted_ages[[2]], \(y) {
      as.vector(mgcv::predict.gam(x, newdata = y))
    })
  })

  list(f = f, m = m)
}

# predict_activity_interpolate() ----
# predict activity rates by interpolating the gams (as oppose to prediction for
# new values) for health status adjusted ages (do this n times, where n =
# model_runs)
# param: adjusted_ages, type: list of 2 (f/m), list of 36 (ages 55-90), vector
# (length = model runs) of health status adjusted ages
# returns: a list of predicted activity rates for supplied adjusted ages,
# rtype: list of 2(f/m), list of 16 (hsagrps), list of 36 (ages 55-90), vector
# (length = model runs)
predict_activity_interpolate <- function(path_self, adjusted_ages) {

  act_df <- load_activity_rt_tbl(path_self) |>
    group_by(hsagrp, sex) |>
    nest(.key = "data") |>
    mutate(
      user_approxfun = map(data, \(x) {
        approxfun(x = x$age, y = x$gam_rt, method = "linear", rule = 2)
      })
    )

  act_df <- split(act_df, ~sex)

  f <- map(act_df$f$user_approxfun, \(x) {
    map_depth(adjusted_ages[[1]], 2, \(y) x(v = y))
  })
  m <- map(act_df$m$user_approxfun, \(x) {
    map_depth(adjusted_ages[[2]], 2, \(y) x(v = y))
  })

  f <- map_depth(f, 2, unlist, use.names = FALSE)
  m <- map_depth(m, 2, unlist, use.names = FALSE)

  list(f = f, m = m)
}
