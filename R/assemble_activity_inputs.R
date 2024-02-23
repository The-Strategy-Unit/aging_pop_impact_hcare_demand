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
source(here("R", "helper_app_inputs.R"))
source(here("R", "helper_theme_1045.R"))
theme_set(theme_1045())

# parameters ---
base_yr <- 2022L

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
  left_join(pop_dat, join_by("area_code", "area_name", "sex", "age")) |>
  mutate(urt = n / pop)

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
      here("data", "2022", y, "app_review_urt.png"),
      width = 400,
      height = 300,
      units = "mm"
    )
  }
)

# save ----
test_dat <- urt_dat |>
  filter(area_code %in% test_areas) |>
  filter(hsagrp %in% hsagrps_app) |>
  left_join(hsagrp_labs, join_by("hsagrp")) |>
  mutate(pod = str_sub(hsagrp, 1, 3), hsagrp = str_sub(hsagrp, 5, -1L)) |>
  mutate(hsagrp = factor(hsagrp, levels = hsagrp_levels)) |>
  select(area_code, pod, hsagrp, hsagrp_lab, sex, age, urt) |>
  arrange(area_code, pod, hsagrp, sex, age)

test_dat |>
  group_by(area_code) |>
  group_walk(\(x, y) {
    write_csv(
      x,
      here(
        "data",
        "app_inputs",
        paste0("test_activity_", y$area_code[[1]], ".csv")
      )
    )
  })
