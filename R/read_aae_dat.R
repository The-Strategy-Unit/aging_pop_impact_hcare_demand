# README
# Read in aae data (data created by proc_aae_grps.sql)


# packages ----
library("dplyr")
library("ggplot2")
library("here")
library("readr")
library("stringr")
library("tidyr")
library("tidytext")

# read ----
aae_filenm <- "aae_dat_2022_20231206.csv"
aae_dat <- read_csv(
  here("data_raw", aae_filenm),
  na = c("", "NA", "NULL")
)

# exclusions ----
aae_dat  |>
  summarise(across(everything(), \(x) sum(is.na(x))))

# missing or invalid sex/age
aae_dat |>
  filter(if_any(c(sex, age), is.na))

# missing or invalid sex/age by lacd
aae_dat |>
  filter(if_any(c(sex, age), is.na)) |>
  group_by(lacd) |>
  summarise(n = sum(n)) |>
  arrange(-n)

# apply exclusions
aae_dat <- aae_dat |>
  drop_na() |>
  filter(str_detect(lacd, "^(?:E10|E0[6-9])"))

# review ----
aae_dat |>
  group_by(yyyymm) |>
  summarise(n = sum(n)) |>
  mutate(yyyymm = factor(yyyymm)) |>
  ggplot() +
  geom_bar(aes(x = yyyymm, y = n), stat = "identity")
ggsave(here("figures", "review_aae_yyyymm.png"))

aae_dat |>
  group_by(lacd) |>
  summarise(n = sum(n)) |>
  ggplot() +
  geom_point(aes(x = reorder(lacd, n), y = n)) +
  theme(
    axis.text.x = element_text(angle = 90)
  )
ggsave(here("figures", "review_aae_lacd.png"))

aae_dat |>
  group_by(lacd, arrmode) |>
  summarise(n = sum(n)) |>
  ungroup() |>
  mutate(pct_arrmode = n / sum(n)) |>
  select(-n) |>
  mutate(
    arrmode = factor(arrmode),
    lacd = tidytext::reorder_within(lacd, pct_arrmode, within = arrmode)
  ) |>
  ggplot() +
  geom_point(aes(x = lacd, y = pct_arrmode, color = arrmode)) +
  tidytext::scale_x_reordered() +
  facet_wrap(vars(arrmode), scales = "free_x") +
  theme(
    axis.text.x = element_text(angle = 90)
  )
ggsave(here("figures", "review_aae_hsagrp_bylacd.png"))

aae_dat |>
  group_by(arrmode, sex, age) |>
  summarise(n = sum(n)) |>
  ggplot(aes(x = age, y = n, group = sex, color = sex)) +
  geom_point() +
  facet_wrap(vars(arrmode), scales = "free_y")
ggsave(here("figures", "review_aae_hsagrp_byage.png"))


# save ----
write_rds(aae_dat, here("data", "aae_dat_2022.rds"))
