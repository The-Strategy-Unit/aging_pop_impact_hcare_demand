# README
# Assemble activity data by area supplied as an input to the app


# packages ----
library("dplyr")
library("ggplot2")
library("here")
library("purrr")
library("readr")
library("stringr")
library("tidyr")

# helpers ----
source(here("R", "hsa_build_gams.R"))
source(here("R", "helper_app_inputs.R"))
source(here("R", "helper_theme_1045.R"))
theme_set(theme_1045())

# parameters ----
base_yr <- 2022L
rerun_gams <- TRUE # run GAMS Yes/No

# read ----
aae_dat <- read_rds(here("data", "aae_clean_2022.rds"))
apc_dat <- read_rds(here("data", "apc_clean_2022.rds"))
opc_dat <- read_rds(here("data", "opc_clean_2022.rds"))
pop_dat <- read_rds(here("data", "app_pop_90_inputs.rds"))

# assemble ----
# filter on base year
pop_dat <- pop_dat |>
  filter(year == base_yr, id == "principal_proj", age >= 17) |>
  select(-id)

act_dat <- bind_rows(aae_dat, apc_dat, opc_dat)

urt_dat <- act_dat |>
  filter(hsagrp %in% hsagrps_app) |>
  left_join(pop_dat, join_by("area_code", "area_name", "sex", "age")) |>
  mutate(urt = n / pop) |>
  left_join(hsagrp_labs, join_by("hsagrp")) |>
  mutate(hsagrp = factor(hsagrp, levels = hsagrp_levels)) |>
  arrange(area_code, area_name, hsagrp)

# review ----
# plot area rates alongside england rates
plot_urts <- function(urt_eng, urt_area) {
  ggplot() +
    geom_line(
      aes(x = age, y = urt, group = sex),
      color = "#a9adb0",
      data = urt_eng
    ) +
    geom_line(
      aes(x = age, y = urt, group = sex, color = sex),
      show.legend = FALSE,
      data = urt_area
    ) +
    facet_wrap(vars(hsagrp), scales = "free_y") +
    scale_color_manual(values = c("#fd484e", "#2c74b5"))
}

area_codes <- unique(urt_dat$area_code)
area_codes <- area_codes[!area_codes == "E92000001"]

plot_ls <- map2("E92000001", area_codes,
  \(x, y) {
    i <- urt_dat |> filter(area_code == x)
    j <- urt_dat |> filter(area_code == y)
    plot_urts(i, j)
  }
)

names(plot_ls) <- area_codes

# save plots
walk2(
  plot_ls, names(plot_ls),
  \(x, y) {
    ggsave(
      here("data", "2022", y, paste0(y, "_review_urt.png")),
      x,
      width = 400,
      height = 300,
      units = "mm"
    )
  }
)

# run gams ----
area_codes <- unique(urt_dat$area_code)
gams_ls <- vector("list")

if (rerun_gams) {
  for (i in seq_along(area_codes)) {
    run_gams(area_codes[i], base_yr)
  }
}

for (i in seq_along(area_codes)) {
  x <- read_csv(
    here("data", base_yr, area_codes[i], "hsa_activity_rt_tbl.csv"),
    show_col_types = FALSE
  )
  gams_ls[[i]] <- x
}

names(gams_ls) <- area_codes

# assemble ----
# make a list
app_ls <- urt_dat |> group_split(area_code, area_name)
names(app_ls) <- unique(urt_dat$area_code)

# format df ready to save as JSON
format_json <- function(df_urt, df_gams) {
  df_urt |>
    left_join(
      df_gams |>
        rename(s = gam_rt),
      join_by(area_code, hsagrp, sex, age)
    ) |>
    select(area_code, hsagrp, hsagrp_lab, sex, age, urt, s) |>
    left_join(lookup_pod, join_by(hsagrp)) |>
    rename(group = hsagrp, label = hsagrp_lab) |>
    nest(.by = c(starts_with("area"), pod, group, label), .key = "data") |>
    mutate(data = map(
      data, \(x) {
        x |>
          pivot_longer(c(urt, s), names_to = "var", values_to = "rt") |>
          pivot_wider(
            names_from = c(sex, var),
            names_glue = "{sex}{var}",
            values_from = rt
          ) |>
          rename_with(\(x) str_remove(x, "urt"), .cols = ends_with("urt")) |>
          select(age, f, m, fs, ms)
      }
    ))
}

app_ls <- map2(
  app_ls, gams_ls,
  \(x, y) {
    format_json(x, y)
  },
  .progress = list(
    clear = TRUE,
    format = "Format JSON files: {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta}", # nolint: line_length_linter.
    type = "iterator"
  )
)

# save ----
walk2(
  app_ls, names(app_ls),
  \(x, y) {
    jsonlite::write_json(
      x,
      here(
        "data",
        "app_inputs",
        "line_charts",
        paste0(y, ".json")
      )
    )
  }
)
