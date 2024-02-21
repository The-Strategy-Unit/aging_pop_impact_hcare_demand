# README
# Read life tables from 2018-based national population projections
# x2 variant life tables were published alongside the principal


# packages ----
library("dplyr")
library("here")
library("purrr")
library("readr")
library("readxl")
library("stringr")
library("tidyr")

# read life tables 2018b ----
read_lt18_files <- function(dir) {
  lt18_files <- list.files(
    here(
      dir
    ),
    "18ex(.xls)$",
    recursive = TRUE,
    full.names = TRUE
  ) |>
    as.list()

  # obtain req sheet names
  ex18_sheets <- map(lt18_files, \(x) excel_sheets(path = x)) |>
    map(keep, str_detect, "period|cohort")
  id <- str_extract(lt18_files, "[a-z]{3}(?=18ex)")
  # iterate over files
  l <- map(lt18_files, \(x) {
    excel_sheets(x) |>
      str_subset(pattern = "period|cohort") |>
      # iterate over sheets
      map(\(y) {
        read_xls(x, y, skip = 9) |>
          filter(!row_number() == 1L) |>
          rename_with(.cols = 1, ~"age") |>
          mutate(base = "2018b") |>
          pivot_longer(
            cols = `1981`:`2068`,
            names_to = "year",
            values_to = "ex"
          ) |>
          mutate(
            sex = tolower(str_extract(y, "Females|Males")),
            type = str_extract(y, "period|cohort"),
            year = as.integer(year)
          )
      }) |>
      bind_rows()
  })
  l <- setNames(l, id)
  df <- map_df(l, \(x) as_tibble(x), .id = "var") |>
    mutate(sex = str_sub(sex, 1L, 1L)) |>
    write_rds(here("data", "npp_2018b_ex.rds"))
}

read_lt18_files("data_raw")
