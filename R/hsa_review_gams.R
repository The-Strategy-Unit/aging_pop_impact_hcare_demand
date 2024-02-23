# README
# Review GAMs by plotting fitted values against observed values


# packages ----
library("dplyr")
library("ggplot2")
library("here")
library("mgcv")
library("patchwork")
library("readr")
library("tidyr")

# helpers ----
source(here("R", "hsa_build_gams.R"))
source(here("R", "helper_theme_1045.R"))

# review_gams() ----
# produce a small multiple plot to check fit of gams
# param: area_code, type: string, local authority code
# param: base_year, type: int, model baseline year
# returns: plot of gams fit, rtype: ggplot object
review_gams <- function(area_code, base_year) {

  path_self <- path_closure({{area_code}}, {{base_year}})

  gams <- read_rds(path_self("hsa_gams.rds"))

  gams <- gams |>
    unnest(gams) |>
    # predict.gam returns an array (convert to vector)
    mutate(data = map2(
      data, gams,
      \(x, y) mutate(x, gam_rt = as.vector(mgcv::predict.gam(y)))
    )) |>
    select(-gams) |>
    unnest(data)

  gams_f <- gams |> filter(sex == "f")
  gams_m <- gams |> filter(sex == "m")

  p1 <- ggplot(gams_f) +
    geom_point(aes(x = age, y = rt), color = "#fd484e") +
    geom_line(aes(x = age, y = gam_rt), color = "#fd484e") +
    geom_vline(aes(xintercept = 55), linetype = "22", color = "#686f73") +
    facet_wrap(vars(hsagrp), scales = "free_y") +
    labs(subtitle = "Females") +
    theme_1045()

  p2 <- ggplot(gams_m) +
    geom_point(aes(x = age, y = rt), color = "#2c74b5") +
    geom_line(aes(x = age, y = gam_rt), color = "#2c74b5") +
    geom_vline(aes(xintercept = 55), linetype = "22", color = "#686f73") +
    scale_y_continuous(name = NULL) +
    labs(subtitle = "Males") +
    facet_wrap(vars(hsagrp), scales = "free_y") +
    theme_1045()

  p3 <- p1 + p2

  # save
  ggsave(
    path_self("review_gams.png"),
    p3,
    width = 400,
    height = 300,
    units = "mm"
  )

  return(p3)
}
