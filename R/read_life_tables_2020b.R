# README
# Read life tables from 2020-based national population projections
# principal projection only published


# packages ----
library("dplyr")
library("here")
library("purrr")
library("readr")
library("readxl")
library("stringr")
library("tidyr")

# read life tables 2020b ----
read_lt20_files <- function(dir) {
  lt20_files <- list.files(
    here(
      dir
    ),
    "20ex(.xlsx)$",
    recursive = TRUE,
    full.names = TRUE
  )
  # obtain req sheet names
  ex20_sheets <- map(lt20_files, \(x) excel_sheets(path = x)) |>
    map(keep, str_detect, "period|cohort")
  l <- excel_sheets(lt20_files) |>
    str_subset(pattern = "period|cohort") |>
    # iterate over sheets
    map(\(x) {
      read_xlsx(lt20_files, x, skip = 4) |>
        mutate(base = "2020b") |>
        pivot_longer(
          cols = `1981`:`2070`,
          names_to = "year",
          values_to = "ex"
        ) |>
        mutate(
          sex = tolower(str_extract(x, "females|males")),
          type = str_extract(x, "period|cohort"),
          year = as.integer(year)
        )
    }) |>
    bind_rows() |>
    mutate(sex = str_sub(sex, 1L, 1L)) |>
    write_rds(here("data", "npp_2020b_ex.rds"))
}

read_lt20_files("data_raw")
