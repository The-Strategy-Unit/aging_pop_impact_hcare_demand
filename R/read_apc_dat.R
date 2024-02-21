# README
# Read in apc data (data created by proc_apc_grps.sql)


# packages ----
library("dplyr")
library("ggplot2")
library("here")
library("readr")
library("stringr")
library("tidyr")
library("tidytext")

# read ----
apc_filenm <- "apc_dat_2022_20231206.csv"
apc_dat <- read_csv(
  here("data_raw", apc_filenm),
  na = c("", "NA", "NULL")
)

# exclusions ----
apc_dat  |>
  summarise(across(everything(), \(x) sum(is.na(x))))

# missing or invalid sex/age
apc_dat |>
  filter(if_any(c(sex, age), is.na))

# missing or invalid sex/age by lacd
apc_dat |>
  filter(if_any(c(sex, age), is.na)) |>
  group_by(lacd) |>
  summarise(n = sum(n)) |>
  arrange(-n)

# missing or invalid hsagrp by lacd
apc_dat |>
  filter(is.na(admigrp)) |>
  group_by(lacd) |>
  summarise(n = sum(n)) |>
  arrange(-n)

# apply exclusions
apc_dat <- apc_dat |>
  drop_na() |>
  filter(str_detect(lacd, "^(?:E10|E0[6-9])"))

# pick los method
apc_dat |>
  group_by(sex, age) |>
  summarise(
    bds_sus = sum(bds_sus),
    bds_dd = sum(bds_dd)
  ) |>
  ungroup() |>
  mutate(diff = bds_sus - bds_dd) |>
  arrange(-diff)

# keep bds_sus
apc_dat <- apc_dat |>
  select(-bds_dd) |>
  rename(bds = bds_sus)

# review ----
apc_dat |>
  group_by(yyyymm) |>
  summarise(n = sum(n)) |>
  mutate(yyyymm = factor(yyyymm)) |>
  ggplot() +
  geom_bar(aes(x = yyyymm, y = n), stat = "identity")
ggsave(here("figures", "review_apc_yyyymm.png"))

apc_dat |>
  group_by(lacd) |>
  summarise(n = sum(n)) |>
  ggplot() +
  geom_point(aes(x = reorder(lacd, n), y = n)) +
  theme(
    axis.text.x = element_text(angle = 90)
  )
ggsave(here("figures", "review_apc_lacd.png"))

apc_dat |>
  group_by(lacd, admigrp) |>
  summarise(n = sum(n)) |>
  mutate(pct_admigrp = n / sum(n)) |>
  ungroup() |>
  select(-n) |>
  mutate(
    admigrp = factor(admigrp),
    lacd = tidytext::reorder_within(lacd, pct_admigrp, within = admigrp)
  ) |>
  ggplot() +
  geom_point(aes(x = lacd, y = pct_admigrp, color = admigrp)) +
  tidytext::scale_x_reordered() +
  facet_wrap(vars(admigrp), scales = "free_x") +
  theme(
    axis.text.x = element_text(angle = 90)
  )
ggsave(here("figures", "review_apc_hsagrp_bylacd.png"))

apc_dat |>
  group_by(admigrp, sex, age) |>
  summarise(n = sum(n)) |>
  ggplot(aes(x = age, y = n, group = sex, color = sex)) +
  geom_point() +
  facet_wrap(vars(admigrp), scales = "free_y")
ggsave(here("figures", "review_apc_hsagrp_byage.png"))

# save ----
write_rds(apc_dat, here("data", "apc_dat_2022.rds"))
