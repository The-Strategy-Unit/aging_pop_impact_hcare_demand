# README
# Create area code directories


# packages ----
library("dplyr")
library("here")
library("purrr")
library("readr")

# read ----
lad23 <- read_csv(here("data", "local_authority_districts_2023.csv"))
lookup_lad23_cty <- read_csv(here("data", "lookup_lad2023_cty.csv"))
lookup_lad23_icb <- read_csv(here("data", "lookup_lad2023_icb.csv"))

# create dirs ----
dirs_lad <- unique(lad23$lad23cd)
dirs_cty <- unique(lookup_lad23_cty$cty23cd)
dirs_icb <- unique(lookup_lad23_icb$icb23cd)
dir_eng <- "E92000001"

dirs <- c(dirs_lad, dirs_cty, dirs_icb, dir_eng)

# run this only once
# map(dirs, \(x) dir.create(here("data", "2022", x)))
