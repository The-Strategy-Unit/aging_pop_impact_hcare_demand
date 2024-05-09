# README
# Test apportionment of area 85+ and 90+ populations to syoa


# A) test that modeled 90-100+ estimates match original 90+ totals
mod_90plus <- ts_mye_new |>
  filter(age > 89, year != 2000L) |>
  group_by(area_code, year, sex) |>
  summarise(pop = sum(pop)) |>
  pull(pop)

orig_90plus <- historic_pop_obj_ls[["ts_mye"]] |>
  filter(age > 89, year != 2000L) |>
  group_by(area_code, year, sex) |>
  summarise(pop = sum(pop)) |>
  pull(pop)

test_that("modeled 90-100+ estimates match original 90+ totals", {
  expect_equal(
    var(mod_90plus - orig_90plus),
    0
  )
})

# B) test that modeled 85-90+ estimates match original 85+ totals
mod_85plus <- ts_mye_new |>
  filter(year == 2000L, age > 84) |>
  group_by(area_code, year, sex) |>
  summarise(pop = sum(pop)) |>
  pull(pop)

orig_85plus <- historic_pop_obj_ls[["lad_00"]] |>
  arrange(area_code, sex) |>
  pull(pop)

test_that("modeled 85-90+ estimates match original 85+ totals", {
  expect_equal(
    var(mod_85plus - orig_85plus),
    0
  )
})
