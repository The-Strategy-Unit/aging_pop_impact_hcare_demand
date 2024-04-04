# README
# Test validity of custom variant populations

# packages
library("ggplot2")
library("tidytext")

proj_lookup  <- read_csv(here("data", "lookup_proj_vars.csv"))

# A) test consistency of custom variant total population ranking v. npp rank by lad # nolint: line_length_linter.
# pick a year to test
rng_year <- sample(seq(2019, 2043), size = 1L)
test_yr <- as.character(rng_year)

# no ppp
cus_rnk <- custom |>
  filter(str_detect(area_code, "^E10", negate = TRUE)) |>
  filter(year == test_yr) |>
  group_by(area_code, area_name, id) |>
  summarise(pop = sum(pop)) |>
  arrange(pop, .by_group = TRUE) |>
  mutate(cus_rnk = row_number()) |>
  ungroup()

# rm ppp
npp_rnk <- npp_dat |>
  filter(year == test_yr, id != "ppp") |>
  group_by(id) |>
  summarise(pop = sum(pop)) |>
  arrange(pop, .by_group = TRUE) |>
  mutate(npp_rnk = row_number()) |>
  select(id, npp_rnk)

rnk_diff <- cus_rnk |>
  left_join(npp_rnk, join_by("id")) |>
  mutate(rnk_diff = cus_rnk - npp_rnk) |>
  arrange(rnk_diff)

test_that("test consistency of custom variant ranking v. npp rank by lad", {
  expect_lte(
    max(abs(rnk_diff$rnk_diff)), 3L
  )
})

# B) test difference between population totals for variants that are in both
# custom vars and original snpp set by lad
cus_vars <- custom |>
  filter(str_detect(area_code, "^E10", negate = TRUE)) |>
  filter(id %in% c("pph", "ppl")) |>
  filter(year == test_yr) |>
  group_by(area_code, area_name, id) |>
  summarise(pop = sum(pop)) |>
  ungroup()

orig_snpp <- snpp_dat |>
  filter(id %in% c(
    "var_proj_low_intl_migration",
    "var_proj_high_intl_migration"
  )) |>
  filter(year == test_yr) |>
  left_join(proj_lookup, join_by("id" == "proj_id")) |>
  group_by(id, proj_map, area_code, area_name) |>
  summarise(pop = sum(pop)) |>
  ungroup()

var_diff <- cus_vars |>
  left_join(orig_snpp, join_by("id" == "proj_map", "area_code", "area_name")) |>
  mutate(diff = (pop.x / pop.y - 1) * 100) |>
  arrange(diff)

# plot differences
var_diff |>
  mutate(
    id = factor(id),
    area_name = tidytext::reorder_within(area_name, diff, within = id)
  ) |>
  ggplot() +
  geom_point(aes(x = area_name, y = diff, color = id)) +
  tidytext::scale_x_reordered() +
  facet_wrap(vars(id), scales = "free_x") +
  theme(
    axis.text.x = element_text(angle = 90)
  )
ggsave(here("figures", "test_snpp_2018b_custom_vars.png"))

test_that("test difference between variants that are in both custom variants and
  original snpp set by lad", {
    expect_lte(
      max(abs(var_diff$diff)), 12
    )
    expect_lte(
      mean(abs(var_diff$diff)), 2
    )
  }
)
