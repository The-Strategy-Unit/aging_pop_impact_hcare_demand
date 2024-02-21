# README
# Read 2018-based national population projections
# x17 variant projections were published alongside the principal


# packages ----
library("dplyr")
library("here")
library("purrr")
library("readr")
library("readxl")
library("stringr")
library("tidyr")

# read npp 2018b ----
change_file_ext <- function(x) {
  system2("powershell", args = c("-file", x))
}
# only need to run this once to change file extension
# change_file_ext("cmd_line_dark_arts.ps1")

read_npp_files <- function(dir) {
  npp_files <- list.files(
    here(
      dir
    ),
    "2018(.xls)$",
    recursive = TRUE,
    full.names = TRUE
  )
  id <- str_extract(npp_files, "(?<=npp_2018b/).*(?=_opendata)")
  l <- map(npp_files, \(x) read_xls(x, sheet = "Population")) |>
    setNames(id)
  dat <- bind_rows(l, .id = "id") |>
    rename_all(tolower) |>
    pivot_longer(cols = `2018`:`2118`, names_to = "year", values_to = "pop") |>
    mutate(id = str_remove(id, "en_")) |>
    mutate(age = str_trim(age), area = "England") |>
    mutate(sex = ifelse(sex == "1", "m", "f")) |>
    mutate(age = case_when(
      age %in% c(
        "105 - 109",
        "110 and over"
      ) ~ "105",
      TRUE ~ as.character(age)
    )) |>
    mutate(across(c(age, year), as.integer))
  # regroup by age
  dat <- dat |>
    group_by(across(-pop)) |>
    summarise(pop = sum(pop)) |>
    ungroup() |>
    write_rds(here("data", "npp_2018b.rds"))
}

read_npp_files("data_raw")

# read npp variant codes ----
read_npp_codes <- function(dir) {
  proj_codes_file <- list.files(
    here(
      dir
    ),
    "NPP codes.txt",
    recursive = TRUE,
    full.names = TRUE
  )
  read_lines(proj_codes_file) |>
    as_tibble() |>
    filter(str_detect(value, "^[a-z]{3}:")) |>
    separate(value, c("proj_cd", "proj_nm"), ": ") |>
    mutate(proj_cd = str_c("en_", proj_cd)) |>
    write_csv(here("data", "npp_2018b_codes.csv"))
}

read_npp_codes("data_raw")
