# README
# Create generalised additive models GAMs needed for health status adjustment
# age range for GAMs is [17, 90]
# hsagrps omitted from the health-status adjustment (i.e. no req. for GAMs) are
# aae: NULL
# apc: apc_birth_n, apc_birth_bds, apc_mat_n, apc_mat_bds,
#      apc_paeds-ordelec_n, apc_paeds-ordelec_bds, apc_paeds-emer_n,
#      apc_paeds-emer_bds
# opc: NULL


# packages ----
library("dplyr")
library("here")
library("mgcv")
library("purrr")
library("readr")
library("tidyr")

# helpers
source(here("R", "helper_utility_fns.R"))

# functions -----
# create_activity_type_gams
# create_activity_rt_tbl
# create_gams
# run_gams

# create_activity_type_gams() ----
# helper function for creating gams
# param: activity_type, type: string, either 'aae', 'apc' or 'opc'
# param: pop_df, type: df, dataframe with population by age & sex
# param: omit_hsagrps, type: string vector, activity groups to omit from
# health status adjustment
# returns: dataframe of gams for a single activity type, rtype: df
create_activity_type_gams <- function(
  path_self,
  activity_type,
  pop_df,
  omit_hsagrps
) {

  act_df <- read_rds(path_self(filename = paste0(activity_type, "_clean.rds")))

  if (!is.null(omit_hsagrps)) {
    act_df <- act_df |> filter(!hsagrp %in% omit_hsagrps)
  }

  # turn implicit missing age values into explicit missing age values
  act_df <- act_df |>
    group_by(area_code, hsagrp, sex, age) |>
    summarise(across(n, sum)) |>
    ungroup() |>
    complete(nesting(hsagrp, sex), age = 0:90) |>
    mutate(across(n, \(x) replace(x, is.na(x), 0)))

  # fit gams to this age range
  act_df <- act_df |> filter(age >= 17)
  act_df <- act_df |> filter(age <= 90)

  act_df <- act_df |>
    left_join(pop_df, join_by("sex", "age"))

  act_df$rt <- act_df$n / act_df$base_year

  # create gams
  gams <- act_df |>
    group_by(area_code, hsagrp, sex) |>
    nest(data = c("age", "n", "base_year", "rt")) |>
    mutate(
      gams = map(data, \(x) {
        mgcv::gam(
          rt ~ s(age, bs = "bs", k = 10),
          method = "GCV.Cp",
          family = gaussian(),
          data = x
        )
      })
    ) |>
    ungroup() |>
    select(area_code, hsagrp, sex, data, gams)
}

# create_activity_rt_tbl() ----
# helper function for saving predicted activity rates from gams
# param: filename, type: string, filename for saving gams
# param: gams, type: df, dataframe holding the gams
create_activity_rt_tbl <- function(filename, gams) {

  gam_ages <- tibble(age = seq.int(17, 90))

  gams |>
    unnest(gams) |>
    mutate(age = list(gam_ages)) |>
    # predict.gam returns an array (convert to vector)
    mutate(
      gam_rt = map2(gams, age, \(x, y) {
        as.vector(mgcv::predict.gam(x, y))
      })
    ) |>
    select(area_code, hsagrp, sex, age, gam_rt) |>
    unnest(c(age, gam_rt)) |>
    write_csv(filename)
}

# create_gams() ----
# create gams for an area
# param: area_code, type: string, ONS area geography code
# param: base_year, type: int, base year for gams
# returns: dataframe of gams for all activity types, rtype: df
create_gams <- function(area_code, base_year) {

  path_self <- path_closure({{area_code}}, {{base_year}})

  # load the population data
  pop_df <- read_rds(path_self(filename = "pop_dat.rds"))
  pop_df <- pop_df |>
    filter(id == "principal_proj") |>
    select(id, sex, age, !!as.name(base_year)) |>
    rename(base_year = !!as.name(base_year))

  omit_hsagrps <- tribble(
    ~key, ~value,
    "aae", NULL,
    "apc", "apc_birth_n",
    "apc", "apc_birth_bds",
    "apc", "apc_mat_n",
    "apc", "apc_mat_bds",
    "apc", "apc_paeds-ordelec_n",
    "apc", "apc_paeds-ordelec_bds",
    "apc", "apc_paeds-emer_n",
    "apc", "apc_paeds-emer_bds",
    "opc", NULL
  )

  dat <- omit_hsagrps |>
    group_by(key) |>
    summarise(omit = list(value))

  # create the gams
  gams <- dat |>
    mutate(
      gams = map2(key, omit, \(x, y) {
        create_activity_type_gams(
          path_self,
          activity_type = x,
          pop_df,
          omit_hsagrps = y
        )
      })
    ) |>
    select(-omit)

  create_activity_rt_tbl(filename = path_self("hsa_activity_rt_tbl.csv"), gams)

  return(gams)
}

# run_gams() ----
# create and save gams for an area
# param: area_code, type: string, ONS area geography code
# param: base_year, type: int, model baseline year
# returns: the filename where the gams have been saved to, rtype: string
run_gams <- function(area_code, base_year) {

  path_self <- path_closure({{area_code}}, {{base_year}})

  gams <- create_gams(area_code, base_year)
  filename <- path_self("hsa_gams.rds")

  # save the gams to disk
  gams |> write_rds(filename)

  return(filename)
}
