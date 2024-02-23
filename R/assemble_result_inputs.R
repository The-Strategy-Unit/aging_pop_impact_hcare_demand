# README
# Assemble results data by area supplied as an input to the app


# packages ----
library("dplyr")
library("here")
library("jsonlite")
library("purrr")
library("stringr")

# helpers ----
source(here("R", "helper_utility_fns.R"))
source(here("R", "helper_app_inputs.R"))
source(here("R", "hsa_run_model.R"))
source(here("R", "hsa_assemble_results.R"))

# run the model for a single area ----
load_proj_lookup()
area_code <- "E09000030"

# run models for this set of parameters
model_params <- lookup_proj |>
  mutate(area_code = area_code, .before = everything()) |>
  filter(proj_id %in% vars_app) |>
  mutate(proj_id = factor(proj_id, levels = vars_app)) |>
  select(area_code, proj = proj_id) |>
  group_by(across(everything())) |>
  expand(base_year = 2022L, end_year = seq(2025L, 2040L, 5L)) |>
  mutate(
    model_runs = 1e3,
    rng_state = 014796
  ) |>
  ungroup()

demo_res <- pmap(model_params |> select(-model_runs, -rng_state), demo_fac)
hsa_res <- pmap(model_params, hsa_fac)

# assemble results for a single area ----
demo_res <- map(demo_res, grp_sex_demo)
hsa_res <- map(hsa_res, grp_sex_hsa)

demo_res <- attach_params(demo_res)
hsa_res <- attach_params(hsa_res)

demo_res <- bind_rows(demo_res, .id = "id")
hsa_res <- bind_rows(hsa_res, .id = "id")

res <- hsa_res |>
  left_join(
    demo_res,
    join_by("id", "area_code", "hsagrp", "proj_var", "end_year")
  )

res <- res |>
  select(-(ends_with("_n"))) |>
  mutate(
    proj_id = as.numeric(proj_var),
    id = as.numeric(id)
  ) |>
  unnest(end_p) |>
  left_join(hsagrp_labs, join_by("hsagrp")) |>
  mutate(
    pod = str_sub(hsagrp, 1, 3),
    hsagrp = str_sub(hsagrp, 5, -1L),
    hsagrp = factor(hsagrp, levels = hsagrp_levels),
    end_p = 100 * end_p - 100,
    end_p_nohsa = 100 * end_p_nohsa - 100
  ) |>
  select(
    area_code,
    id,
    proj_id,
    proj_var,
    end_year,
    pod,
    hsagrp,
    hsagrp_lab,
    end_p,
    end_p_nohsa
  ) |>
  arrange(id)

# compute binning ----
json_res <- res |>
  group_by(across(-end_p)) |>
  nest() |>
  arrange(id) |>
  mutate(data = map(data, \(x) pretty_fd(x))) |>
  ungroup()

# save ----
jsonlite::write_json(
  json_res,
  here(
    "data",
    "app_inputs",
    paste0("test_results_", json_res$area_code[[1]], ".json")
  )
)
