# README
# Read 2018-based sub-national population projections
# x4 variant projections were published alongside the principal


# packages ----
library("dplyr")
library("here")
library("purrr")
library("readr")
library("stringr")
library("tidyr")

# read snpp 2018b ----
read_snpp_files <- function(dir) {
  snpp_files <- list.files(
    here(
      dir
    ),
    "^(2018 SNPP).*(females|males).*(.csv$)",
    recursive = TRUE,
    full.names = TRUE
  )
  id <- str_match(snpp_files, "2018b_(.*?)/2018")[, 2]
  l <- map(snpp_files, \(x) read_csv(x)) |>
    setNames(id)
  dat <- bind_rows(l, .id = "id") |>
    rename_all(tolower) |>
    pivot_longer(cols = `2018`:`2043`, names_to = "year", values_to = "pop") |>
    filter(age_group != "All ages") |>
    select(-component) |>
    mutate(sex = str_sub(sex, 1L, 1L)) |>
    # remove metropolitan counties and regions (keep only local authorities)
    filter(str_detect(area_code, "^E11|^E12", negate = TRUE)) |>
    mutate(
      age_group = case_match(age_group,
        "90 and over" ~ "90",
        .default = age_group
      )
    ) |>
    mutate(across(c(age_group, year), as.integer)) |>
    rename(age = age_group) |>
    write_rds(here("data", "snpp_2018b.rds"))
}

read_snpp_files("data_raw")
