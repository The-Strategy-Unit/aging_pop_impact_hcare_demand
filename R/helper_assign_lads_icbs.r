# README
# Create a mapping from local authority districts to integrated care boards


# packages ----
library("dplyr")
library("here")
library("readr")

# read ----
icb23 <- read_csv(here(
  "data_raw",
  "LSOA_(2021)_to_Sub_ICB_Locations_to_Integrated_Care_Boards_Lookup_in_England.csv" # nolint: line_length_linter.
))

# assign icbs ----
icb23 |> distinct(ICB23NM) # 42 icbs
icb23 |> distinct(LAD23CD, LAD23NM) # 296 lads

# Allocate LADs exclusively to ICBs based on number of LSOAs in an LAD
# i.e. if more than 50% of the LSOAs in LAD A are associated with ICB B then
# LAD A is mapped to ICB B
# Most LADs are completely or mostly associated with a single ICB (284/12)
# East Suffolk, Waverley, Hart, and Westmorland and Furness are potential
# problems but all other LADs have more than 84% of their LSOAs associated with
# a single ICB. North Yorkshire is split 3 ways all other splits are 2 way
icb23 |>
  group_by(ICB23NM, LAD23NM) |>
  summarise(n = n()) |>
  group_by(LAD23NM) |>
  mutate(freq = n / sum(n)) |>
  filter(freq < 1) |>
  arrange(LAD23NM) |>
  print(n = 25)

lkup <- icb23 |>
  group_by(ICB23CD, ICB23NM, LAD23CD, LAD23NM) |>
  summarise(n = n()) |>
  group_by(LAD23NM) |>
  mutate(freq = n / sum(n)) |>
  filter(freq == max(freq)) |>
  ungroup() |>
  rename_with(tolower, .cols = everything()) |>
  select(lad23cd, lad23nm, icb23cd, icb23nm)

# save ----
write_csv(lkup, here("data", "lookup_lad2023_icb.csv"))
