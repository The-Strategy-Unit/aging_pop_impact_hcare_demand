# README
# Clean opc data


# packages ----
library("dplyr")
library("ggplot2")
library("here")
library("readr")
library("stringr")
library("tidyr")

# read ----
opc_dat <- read_rds(here("data", "opc_dat_2022.rds"))
lookup_lad18_lad23 <- read_csv(here("data", "lookup_lad2018_lad2023.csv"))
lookup_lad23_cty <- read_csv(here("data", "lookup_lad2023_cty.csv"))
lookup_lad23_icb <- read_csv(here("data", "lookup_lad2023_icb.csv"))
lad23 <- read_csv(here("data", "local_authority_districts_2023.csv"))

# clean ----
opc_dat <- opc_dat |>
  # remove under 17s
  filter(age >= 17) |>
  # set upper age group to 90+
  mutate(age = case_when(age >= 90 ~ 90, TRUE ~ as.double(age))) |>
  rename(area_code = lacd) |>
  # is_tele has precedence over is_proc
  mutate(
    across("has_proc", \(.x) .x * (1 - is_tele)),
    across(matches("^(i|ha)s\\_"), as.logical)
  ) |>
  # construct hsagrps
  mutate(type = ifelse(is_surg, "surg", "non-surg")) |>
  mutate(hsagrp = paste(
    sep = "_",
    type,
    case_when(
      has_proc ~ "proc",
      is_first ~ "first",
      TRUE ~ "fup"
    )
  )) |>
  group_by(area_code, sex, age, hsagrp) |>
  # sum attendances by is_tele
  summarise(
    atts = sum(n * (1 - is_tele)),
    tele_atts = sum(n * is_tele)
  ) |>
  ungroup() |>
  mutate(n = atts + tele_atts) |>
  mutate(across(c(age, atts, tele_atts, n), as.integer))

# review hsagrp by age
opc_dat |>
  group_by(hsagrp, sex, age) |>
  summarise(n = sum(n)) |>
  ggplot(aes(x = age, y = n, group = sex, color = sex)) +
  geom_point() +
  facet_wrap(vars(hsagrp), scales = "free_y")
ggsave(here("figures", "review_opc_hsagrp_byage.png"))

# review tele_atts by age
opc_dat |>
  filter(str_detect(hsagrp, "proc", negate = TRUE)) |>
  group_by(hsagrp, sex, age) |>
  summarise(tele_atts = sum(tele_atts)) |>
  ggplot(aes(x = age, y = tele_atts, group = sex, color = sex)) +
  geom_point() +
  facet_wrap(vars(hsagrp), scales = "free_y")
ggsave(here("figures", "review_opc_tele_byage.png"))

# rm tele atts split
opc_dat <- opc_dat |>
  select(-atts, -tele_atts)

# reconcile local government changes ----
# WARNING this can easily become a rabbit hole!
# opc for 2022 has 309 lads - map to 296 lads (ONS Apr 2023)
opc_lad <- opc_dat |>
  #select(-units) |>
  complete(
    area_code, hsagrp,
    nesting(sex, age),
    fill = list(n = NA),
    explicit = FALSE
  ) |>
  left_join(lookup_lad18_lad23, join_by("area_code" == "lad18cd")) |>
  mutate(
    area_code = case_when(!is.na(new_ladcd) ~ new_ladcd, TRUE ~ area_code)
  ) |>
  select(-contains("lad"), -yrofchg) |>
  group_by(across(-n)) |>
  summarise(n = sum(n, na.rm = TRUE)) |>
  ungroup() |>
  # pull area name
  left_join(lad23, join_by("area_code" == "lad23cd")) |>
  rename(area_name = lad23nm) |>
  select(area_code, area_name, everything())

# compile countys
opc_cty <- lookup_lad23_cty |>
  left_join(opc_lad, join_by("lad23cd" == "area_code")) |>
  group_by(across(starts_with("cty")), hsagrp, sex, age) |>
  summarise(n = sum(n)) |>
  ungroup() |>
  rename(area_code = cty23cd, area_name = cty23nm)

# compile icbs
opc_icb <- lookup_lad23_icb |>
  left_join(opc_lad, join_by("lad23cd" == "area_code")) |>
  group_by(across(starts_with("icb")), hsagrp, sex, age) |>
  summarise(n = sum(n)) |>
  ungroup() |>
  rename(area_code = icb23cd, area_name = icb23nm)

# compile England
opc_eng <- opc_lad |>
  group_by(hsagrp, sex, age) |>
  summarise(n = sum(n)) |>
  ungroup() |>
  mutate(
    area_code = "E92000001",
    area_name = "England",
    .before = everything()
  )

# save ----
opc_all <- bind_rows(opc_lad, opc_cty, opc_icb, opc_eng)
write_rds(opc_all, here("data", "opc_clean_2022.rds"))

opc_all |>
  group_by(grp_var = area_code) |>
  group_walk(
    \(x, y) write_rds(x, here("data", "2022", y$grp_var, "opc_clean.rds"))
  )
