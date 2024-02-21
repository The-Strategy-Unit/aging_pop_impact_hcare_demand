# README
# Keep custom lists and lookups here


# packages ----
library("dplyr")
library("here")
library("readr")
library("stringr")

# mapping of population projection variants - snpp to npp, and
# lookup from projection variants to life table variants ----
lookup_proj <- tribble(
  ~proj_src, ~proj_id, ~proj_desc, ~ex_id, ~proj_map,
  "snpp", "principal_proj", "Principal projection", "ppp", "ppp",
  "snpp", "var_proj_10_year_migration", "10 year internal migration trend", "ppp", "ppp", # nolint: line_length_linter.
  "snpp", "var_proj_alt_internal_migration", "5 year internal migration trend", "ppp", "ppp", # nolint: line_length_linter.
  "snpp", "var_proj_low_intl_migration", "Low migration", "ppp", "ppl",
  "snpp", "var_proj_high_intl_migration", "High migration", "ppp", "pph",
  "npp", "ppp", "Principal projection", "ppp", NA_character_,
  "npp", "ppl", "Low migration", "ppp", NA_character_,
  "npp", "pph", "High migration", "ppp", NA_character_,
  "npp", "cpp", "Constant fertility", "ppp", NA_character_,
  "npp", "cnp", "Constant fertility no mortality improvement", "ppp", NA_character_, # nolint: line_length_linter.
  "npp", "lll", "Low population", "lle", NA_character_,
  "npp", "hhh", "High population", "hle", NA_character_,
  "npp", "hlh", "Young age structure", "lle", NA_character_,
  "npp", "lhl", "Old age structure", "hle", NA_character_,
  "npp", "lpp", "Low fertility", "ppp", NA_character_,
  "npp", "hpp", "High fertility", "ppp", NA_character_,
  "npp", "plp", "Low life expectancy", "lle", NA_character_,
  "npp", "php", "High life expectancy", "hle", NA_character_,
  "npp", "pnp", "No mortality improvement", "ppp", NA_character_,
  "npp", "ppq", "0% Future EU migration (Not National Statistics)", "ppp", NA_character_, # nolint: line_length_linter.
  "npp", "ppr", "50% Future EU migration (Not National Statistics)", "ppp", NA_character_, # nolint: line_length_linter.
  "npp", "ppz", "Zero net migration (natural change only)", "ppp", NA_character_, # nolint: line_length_linter.
  "npp", "rpp", "Replacement fertility", "ppp", NA_character_,
) |>
  mutate(proj_map = case_when(
    is.na(proj_map) ~ proj_id,
    TRUE ~ as.character(proj_map)
  ))

lookup_proj |> write_csv(here("data", "lookup_proj_vars.csv"))

# lookup for local government changes from 2018 to 2023 ----
lookup_lad18_lad23 <- tribble(
  ~"yrofchg", ~"lad18cd", ~"lad18nm", ~"new_ladcd", ~"new_ladnm",
  2019, "E06000028", "Bournemouth", "E06000058", "Bournemouth, Christchurch and Poole", # nolint: line_length_linter.
  2019, "E06000029", "Poole", "E06000058", "Bournemouth, Christchurch and Poole", # nolint: line_length_linter.
  2020, "E07000004", "Aylesbury Vale", "E06000060", "Buckinghamshire",
  2020, "E07000005", "Chiltern", "E06000060", "Buckinghamshire",
  2020, "E07000006", "South Bucks", "E06000060", "Buckinghamshire",
  2020, "E07000007", "Wycombe", "E06000060", "Buckinghamshire",
  2023, "E07000026", "Allerdale", "E06000063", "Cumberland",
  2023, "E07000027", "Barrow-in-Furness", "E06000064", "Westmorland and Furness", # nolint: line_length_linter.
  2023, "E07000028", "Carlisle", "E06000063", "Cumberland",
  2023, "E07000029", "Copeland", "E06000063", "Cumberland",
  2023, "E07000030", "Eden", "E06000064", "Westmorland and Furness",
  2023, "E07000031", "South Lakeland", "E06000064", "Westmorland and Furness",
  2019, "E07000048", "Christchurch", "E06000058", "Bournemouth, Christchurch and Poole", # nolint: line_length_linter.
  2019, "E07000049", "East Dorset", "E06000059", "Dorset",
  2019, "E07000050", "North Dorset", "E06000059", "Dorset",
  2019, "E07000051", "Purbeck", "E06000059", "Dorset",
  2019, "E07000052", "West Dorset", "E06000059", "Dorset",
  2019, "E07000053", "Weymouth and Portland", "E06000059", "Dorset",
  2021, "E07000150", "Corby", "E06000061", "North Northamptonshire",
  2021, "E07000151", "Daventry", "E06000062", "West Northamptonshire",
  2021, "E07000152", "East Northamptonshire", "E06000061", "North Northamptonshire", # nolint: line_length_linter.
  2021, "E07000153", "Kettering", "E06000061", "North Northamptonshire",
  2021, "E07000154", "Northampton", "E06000062", "West Northamptonshire",
  2021, "E07000155", "South Northamptonshire", "E06000062", "West Northamptonshire", # nolint: line_length_linter.
  2021, "E07000156", "Wellingborough", "E06000061", "North Northamptonshire",
  2023, "E07000163", "Craven", "E06000065", "North Yorkshire",
  2023, "E07000164", "Hambleton", "E06000065", "North Yorkshire",
  2023, "E07000165", "Harrogate", "E06000065", "North Yorkshire",
  2023, "E07000166", "Richmondshire", "E06000065", "North Yorkshire",
  2023, "E07000167", "Ryedale", "E06000065", "North Yorkshire",
  2023, "E07000168", "Scarborough", "E06000065", "North Yorkshire",
  2023, "E07000169", "Selby", "E06000065", "North Yorkshire",
  2023, "E07000187", "Mendip", "E06000066", "Somerset",
  2023, "E07000188", "Sedgemoor", "E06000066", "Somerset",
  2023, "E07000189", "South Somerset", "E06000066", "Somerset",
  2023, "E07000246", "Somerset West and Taunton", "E06000066", "Somerset",
  # 2019, "E07000190", "Taunton Deane", "E07000246", "Somerset West and Taunton", # nolint: line_length_linter.
  # 2019, "E07000191", "West Somerset", "E07000246", "Somerset West and Taunton", # nolint: line_length_linter.
  2019, "E07000190", "Taunton Deane", "E06000066", "Somerset",
  2019, "E07000191", "West Somerset", "E06000066", "Somerset",
  2019, "E07000201", "Forest Heath", "E07000245", "West Suffolk",
  2019, "E07000204", "St Edmundsbury", "E07000245", "West Suffolk",
  2019, "E07000205", "Suffolk Coastal", "E07000244", "East Suffolk",
  2019, "E07000206", "Waveney", "E07000244", "East Suffolk"
) |>
  mutate(yrofchg = as.integer(yrofchg))

lookup_lad18_lad23 |> write_csv(here("data", "lookup_lad2018_lad2023.csv"))
