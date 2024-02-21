# README
# Read in opc data (data created by proc-opc-grps.sql)


# packages ----
library("dplyr")
library("ggplot2")
library("here")
library("readr")
library("stringr")
library("tidyr")
library("tidytext")

# read ----
opc_filenm <- "opc_dat_2022_20231208.csv"
opc_dat <- read_csv(
  here("data_raw", opc_filenm),
  na = c("", "NA", "NULL")
)

# exclusions ----
opc_dat  |>
  summarise(across(everything(), \(x) sum(is.na(x))))

# missing or invalid sex/age
opc_dat |>
  filter(if_any(c(sex, age), is.na))

# missing or invalid sex/age by lacd
opc_dat |>
  filter(if_any(c(sex, age), is.na)) |>
  group_by(lacd) |>
  summarise(n = sum(n)) |>
  arrange(-n)

# apply exclusions
opc_dat <- opc_dat |>
  drop_na() |>
  filter(str_detect(lacd, "^(?:E10|E0[6-9])"))

# review ----
opc_dat |>
  group_by(yyyymm) |>
  summarise(n = sum(n)) |>
  mutate(yyyymm = factor(yyyymm)) |>
  ggplot() +
  geom_bar(aes(x = yyyymm, y = n), stat = "identity")
ggsave(here("figures", "review_opc_yyyymm.png"))

opc_dat |>
  group_by(lacd) |>
  summarise(n = sum(n)) |>
  ggplot() +
  geom_point(aes(x = reorder(lacd, n), y = n)) +
  theme(
    axis.text.x = element_text(angle = 90)
  )
ggsave(here("figures", "review_opc_lacd.png"))

pct_first <- opc_dat |>
  group_by(lacd, is_first) |>
  summarise(n = sum(n)) |>
  pivot_wider(names_from = "is_first", values_from = "n", names_prefix = "p") |>
  summarise(pct_first = p1 / (p0 + p1))

pct_tele <- opc_dat |>
  group_by(lacd, is_tele) |>
  summarise(n = sum(n)) |>
  pivot_wider(names_from = "is_tele", values_from = "n", names_prefix = "p") |>
  summarise(pct_tele = p1 / (p0 + p1))

pct_surg <- opc_dat |>
  group_by(lacd, is_surg) |>
  summarise(n = sum(n)) |>
  pivot_wider(names_from = "is_surg", values_from = "n", names_prefix = "p") |>
  summarise(pct_surg = p1 / (p0 + p1))

pct_proc <- opc_dat |>
  group_by(lacd, has_proc) |>
  summarise(n = sum(n)) |>
  pivot_wider(names_from = "has_proc", values_from = "n", names_prefix = "p") |>
  summarise(pct_proc = p1 / (p0 + p1))

pct_first |>
  left_join(pct_tele, join_by("lacd")) |>
  left_join(pct_surg, join_by("lacd")) |>
  left_join(pct_proc, join_by("lacd")) |>
  pivot_longer(-lacd, names_to = "grp", values_to = "pct") |>
  mutate(
    grp = factor(grp),
    lacd = tidytext::reorder_within(lacd, pct, within = grp)
  ) |>
  ggplot() +
  geom_point(aes(x = lacd, y = pct, color = grp)) +
  tidytext::scale_x_reordered() +
  facet_wrap(vars(grp), scales = "free_x") +
  theme(
    axis.text.x = element_text(angle = 90)
  )
ggsave(here("figures", "review_opc_hsagrp_bylacd.png"))

# save ----
write_rds(opc_dat, here("data", "opc_dat_2022.rds"))
