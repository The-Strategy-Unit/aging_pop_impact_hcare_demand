# README
# Assemble results data by area supplied as an input to the app


# packages ----
library("dplyr")
library("here")
library("jsonlite")
library("purrr")
library("stringr")
library("tictoc")

# helpers ----
source(here("R", "hsa_run_model.R"))
source(here("R", "hsa_assemble_results.R"))
source(here("R", "helper_utility_fns.R"))
source(here("R", "helper_app_inputs.R"))
load_proj_lookup()

# read ----
area_codes_df <- read_csv(
  here("data", "app_inputs", "area_names_and_codes.csv"),
  show_col_types = FALSE
)
area_code <- sort(unique(area_codes_df$cd))

# run models ----
# run models for this set of parameters
model_params <- expand_grid(lookup_proj, area_code) |>
  filter(proj_id %in% vars_app) |>
  select(area_code, proj = proj_id) |>
  group_by(across(everything())) |>
  expand(base_year = 2022L, end_year = seq(2025L, 2040L, 5L)) |>
  mutate(
    model_runs = 1e3,
    rng_state = 014796
  ) |>
  ungroup()

# 360 areas x 11 variants x 4 end years = 15,840 models
# plus x1000 simulations for the full model
# 3 versions of the model
# i) pure demographic model
# ii) hsa mode only model
# iii) hsa full model

# i) pure demographic model
model_params_nosim  <- model_params |>
  select(-model_runs, -rng_state)

tic.clearlog()
tic("Pure demographic model")
demo_res <- pmap(
  model_params_nosim,
  demo_fac,
  # add a progress bar
  .progress = list(
    clear = TRUE,
    format = "Pure demographic model: {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}", # nolint: line_length_linter.
    type = "iterator"
  )
)
toc(log = TRUE, quiet = TRUE)

# ii) hsa mode only model
tic("HSA mode only model")
hsa_mode_res <- pmap(
  model_params_nosim,
  hsa_mode_fac,
  method = "interp",
  .progress = list(
    clear = TRUE,
    format = "HSA mode only model: {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}", # nolint: line_length_linter.
    type = "iterator"
  )
)
toc(log = TRUE, quiet = TRUE)

# iii) hsa full model
tic("HSA full model")
hsa_res <- pmap(
  model_params,
  hsa_fac,
  method = "interp",
  .progress = list(
    clear = TRUE,
    format = "HSA full model: {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}", # nolint: line_length_linter.
    type = "iterator"
  )
)
toc(log = TRUE, quiet = TRUE)

# save timings
timings_run <- tic.log(format = TRUE)
timings_run |>
  enframe() |>
  unnest(cols = c(value)) |>
  write_csv(here("R", "timings_model_runs.csv"))

# assemble  ----
# group results for sex
demo_res <- map(demo_res, grp_sex_demo)
hsa_mode_res <- map(hsa_mode_res, grp_sex_hsa_mode)
hsa_res <- map(hsa_res, grp_sex_hsa)

# make a single list
res_ls <- pmap(
  list(demo_res, hsa_mode_res, hsa_res),
  \(x, y, z) {
    x |>
      left_join(y, join_by(hsagrp)) |>
      left_join(z, join_by(hsagrp))
  }
)

# format df ready to save as JSON
format_json <- function(df_res) {
  df_res |>
    select(-(ends_with("_n"))) |>
    unnest(end_p) |>
    left_join(hsagrp_labs, join_by(hsagrp)) |>
    mutate(
      pod = str_sub(hsagrp, 1, 3), hsagrp = str_sub(hsagrp, 5, -1L),
      hsagrp = factor(hsagrp, levels = hsagrp_levels)
    ) |>
    rename(group = hsagrp, label = hsagrp_lab) |>
    mutate(
      end_p = 100 * end_p - 100,
      end_p_nohsa = 100 * end_p_nohsa - 100
    ) |>
    select(pod, group, label, end_p, end_p_md, end_p_nohsa)
}

res_ls <- map(res_ls, \(x) format_json(x))

# compute binning ----
comp_bins <- function(df_res) {
  df_res |>
    group_by(across(-end_p)) |>
    nest(.key = "data") |>
    mutate(data = map(data, \(x) pretty_fd(x))) |>
    ungroup()
}

res_ls <- map(
  res_ls,
  \(x) {
    comp_bins(x)
  },
  .progress = list(
    clear = TRUE,
    format = "Compute bins: {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}", # nolint: line_length_linter.
    type = "iterator"
  )
)

# attach params ----
params_ls <- split(
  model_params_nosim |>
    select(-base_year) |>
    rename(variant = proj) |>
    mutate(
      variant = factor(variant, levels = vars_levels),
      variant = paste0("v", as.integer(variant))
    )
  , seq_len(nrow(model_params_nosim))
)

app_ls <- map2(res_ls, params_ls, \(x, y) bind_cols(y, x))
names(app_ls) <- map(app_ls, \(x) pluck(x$area_code, 1))
app_ls <- split(app_ls, names(app_ls)) |> map(bind_rows)

# save ----
walk2(
  app_ls, names(app_ls),
  \(x, y) {
    jsonlite::write_json(
      x,
      here(
        "data",
        "app_inputs",
        "histograms",
        paste0(y, ".json")
      )
    )
  }
)
