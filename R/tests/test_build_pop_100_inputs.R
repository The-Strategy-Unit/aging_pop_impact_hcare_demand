# README
# Test validity of modeled 0-100+ area populations used as inputs to app


snpp_lad_90 <- out_dat |>
  filter(str_detect(area_code, "^E10|^E54|^E92", negate = TRUE), age >= 90) |>
  group_by(id, year) |>
  summarise(pop = sum(pop)) |>
  ungroup()

snpp_icb_90 <- out_dat |>
  filter(str_detect(area_code, "^E54"), age >= 90) |>
  group_by(id, year) |>
  summarise(pop = sum(pop)) |>
  ungroup()

npp_90 <- npp_dat |>
  filter(year <= 2043, age >= 90) |>
  group_by(id, year) |>
  summarise(pop = sum(pop)) |>
  ungroup()

test_diff <- function(snpp_df, npp_df) {
  npp_df |>
    inner_join(snpp_df, join_by("id", "year")) |>
    mutate(diff = pop.x - pop.y)
}

diff_lad_90 <- test_diff(snpp_lad_90, npp_90)
diff_icb_90 <- test_diff(snpp_icb_90, npp_90)

test_that("modeled snpp lad 90+ populations match original npp 90+ populations
  by variant", {
    expect_lt(
      max(abs(
        diff_lad_90 |> pull(diff)
      )),
      1
    )
  }
)

test_that("modeled snpp icb 90+ populations match original npp 90+ populations
  by variant", {
    expect_lt(
      max(abs(
        diff_icb_90 |> pull(diff)
      )),
      1
    )
  }
)
