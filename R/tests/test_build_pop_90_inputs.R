# README
# Test validity of area populations (0-90+) used as inputs to app and model


# A) test that ctys in app dataset match 2023 ONS master list
test_that("county councils in app dataset match 2023 ONS master list", {
  expect_length(
    setdiff(
      unique(app_vars_cty$area_code),
      unique(lookup_lad23_cty$cty23cd)
    ), 0
  )
})

# B) test that lads in app dataset match 2023 ONS master list
test_that("local authorities in app dataset match 2023 ONS master list", {
  expect_length(
    setdiff(
      unique(app_vars_lad$area_code),
      unique(lad23$lad23cd)
    ), 0
  )
})

# C) test that icbs in app dataset match 2023 ONS master list
test_that("integrated care boards in app dataset match 2023 ONS master list", {
  expect_length(
    setdiff(
      unique(app_vars_icb$area_code),
      unique(lookup_lad23_icb$icb23cd)
    ), 0
  )
})


# D) test that population totals in app dataset match both published snpp and npp totals # nolint: line_length_linter.
sum_test_pop <- function(app_df, ref_df) {

  rng_year <- sample(seq(2019, 2043), size = 1L)
  test_yr <- as.character(rng_year)
  test_var <- sample(ref_df$id, size = 1L)

  x <- map(list(app_df, ref_df), \(x) {
    x |>
      filter(year == {{ test_yr }}, , id == {{ test_var }}) |>
      summarise(pop = sum(pop)) |>
      pull(pop)
  })
  return(x)
}

test_snpp <- sum_test_pop(
  app_vars_lad,
  # remove countys from published snpp
  snpp_2018b |> filter(str_detect(area_code, "^E10", negate = TRUE)
  )
)

test_that("population totals in app dataset match published snpp totals", {
  expect_equal(
    test_snpp[[1]],
    test_snpp[[2]]
  )
})

test_npp <- sum_test_pop(
  app_vars_lad,
  app_vars_eng
)

test_that("population totals in app dataset match published npp totals", {
  expect_equal(
    test_npp[[1]],
    test_npp[[2]]
  )
})

# E) test that cty totals match sum of constituent lads
cty_pop <- app_vars_cty |>
  group_by(across(starts_with("area")), year) |>
  summarise(pop = sum(pop)) |>
  ungroup() |>
  arrange(area_code, year)

lad_pop <- lookup_lad23_cty |>
  left_join(app_vars_lad, join_by("lad23cd" == "area_code")) |>
  group_by(across(starts_with("cty")), year) |>
  summarise(pop = sum(pop)) |>
  ungroup() |>
  arrange(cty23cd, year)

test_that("cty totals in app dataset match sum of constituent lads", {
  expect_equal(
    var(cty_pop$pop - lad_pop$pop),
    0,
  )
})

# F) test that icb totals match sum of constituent lads
icb_pop <- app_vars_icb |>
  group_by(across(starts_with("area")), year) |>
  summarise(pop = sum(pop)) |>
  ungroup() |>
  arrange(area_code, year)

lad_pop <- lookup_lad23_icb |>
  left_join(app_vars_lad, join_by("lad23cd" == "area_code")) |>
  group_by(across(starts_with("icb")), year) |>
  summarise(pop = sum(pop)) |>
  ungroup() |>
  arrange(icb23cd, year)

test_that("icb totals in app dataset match sum of constituent lads", {
  expect_equal(
    var(icb_pop$pop - lad_pop$pop),
    0,
  )
})
