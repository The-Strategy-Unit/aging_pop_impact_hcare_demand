# README
# Changes to local government in England took place between 2019 and 2023
# I need to reconcile local areas across years in both population and activity
# datasets
# https://en.wikipedia.org/wiki/2019%E2%80%932023_structural_changes_to_local_government_in_England # nolint: line_length_linter.
# My ground truth for local government areas is 'names and codes' from ONS Open
# Geography portal https://geoportal.statistics.gov.uk/

# ONS ground truth:
# names & codes Dec 2018 LAD = 326, CTY = 27
# names & codes Dec 2022 LAD = 309, CTY = 24
# names & codes Apr 2023 LAD = 296, CTY = 21

# Population datasets:
# snpp 2018b LAD = 326, CTY = 27

# Activity datasets:
# Changes in Cumbria, North Yorkshire, and Somerset appear mid-2022 in aae and
# apc activity datasets
# aae 2022 LAD = 313
# apc 2022 LAD = 313
# opc 2022 LAD = 309


# packages ----
library("dplyr")
library("here")
library("readr")

# activity datasets ----
# aae and apc 2022 include districts as at December 2022 but
# changes in Cumbria, North Yorkshire, and Somerset appear mid-year
# therefore a mix of districts as at December 2022 and December 2023
aae_dat <- read_rds(here("data", "aae_dat_2022.rds"))
aae_ladcd <- aae_dat |> distinct(lacd) |> pull()
apc_dat <- read_rds(here("data", "apc_dat_2022.rds"))
apc_ladcd <- apc_dat |> distinct(lacd) |> pull()
opc_dat <- read_rds(here("data", "opc_dat_2022.rds"))
opc_ladcd <- opc_dat |> distinct(lacd) |> pull()

setdiff(aae_ladcd, lad23$lad23cd)
setdiff(lad23$lad23cd, aae_ladcd)
setdiff(apc_ladcd, lad22$lad23cd)
setdiff(lad23$lad23cd, aae_ladcd)
setdiff(opc_ladcd, lad22$lad22cd)
setdiff(lad23$lad23cd, opc_ladcd)
