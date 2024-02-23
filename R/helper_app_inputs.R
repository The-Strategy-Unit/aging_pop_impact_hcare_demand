# README
# Lists, labels, levels, fns etc. to help with assembling input datasets for the
# app
# test areas
# Coventry E08000026
# Malvern Hills E07000235
# Tower Hamlets E09000030
# England E92000001


# packages ----
library("tibble")

# test areas
test_areas <- c(
  "E08000026",
  "E07000235",
  "E09000030",
  "E92000001"
)

# projection variants
vars_app <- c(
  "hpp",
  "lpp",
  "php",
  "plp",
  "hhh",
  "lll",
  "lhl",
  "hlh",
  "principal_proj",
  "var_proj_high_intl_migration",
  "var_proj_low_intl_migration"
)

# projection variant levels
vars_levels <- c(
  "principal_proj",
  "hpp",
  "lpp",
  "php",
  "plp",
  "var_proj_high_intl_migration",
  "var_proj_low_intl_migration",
  "hhh",
  "lll",
  "hlh",
  "lhl"
)

# hsa groups
hsagrps_app <- c(
  "aae_amb",
  "aae_walkin",
  "apc_daycase_n",
  "apc_emer_n",
  "apc_emer_bds",
  "apc_ordelec_n",
  "apc_ordelec_bds",
  "opc_non-surg_first",
  "opc_non-surg_fup",
  "opc_non-surg_proc",
  "opc_surg_first",
  "opc_surg_fup",
  "opc_surg_proc"
)

# hsa group labels
hsagrp_labs <- tribble(
  ~"hsagrp", ~"hsagrp_lab",
  "aae_amb", "Ambulance arrivals",
  "aae_walkin", "Walk-in arrivals",
  "apc_daycase_n", "Daycases",
  "apc_emer_n", "Unplanned admissions",
  "apc_emer_bds", "Unplanned bed days",
  "apc_ordelec_n", "Elective admissions",
  "apc_ordelec_bds", "Elective bed days",
  "opc_non-surg_first", "First app. (non-surgical specialties)",
  "opc_non-surg_fup", "Follow-up app. (non-surgical specialties)",
  "opc_non-surg_proc", "Procedure (non-surgical specialties)",
  "opc_surg_first", "First app. (surgical specialties)",
  "opc_surg_fup", "Follow-up app. (surgical specialties)",
  "opc_surg_proc", "Procedure (surgical specialties)"
)

# hsa group levels
hsagrp_levels <- c(
  "walkin",
  "amb",
  "emer_n",
  "emer_bds",
  "daycase_n",
  "ordelec_n",
  "ordelec_bds",
  "surg_proc",
  "non-surg_proc",
  "surg_first",
  "non-surg_first",
  "surg_fup",
  "non-surg_fup"
)

# group-up results for sexes
grp_sex_hsa <- function(df) {
  df |>
    select(-end_p) |>
    group_by(hsagrp) |>
    group_modify(~ {
      .x |>
        summarise(
          base_n = sum(base_n),
          end_n = list(rowSums(sapply(end_n, unlist)))
        )
    }) |>
    ungroup() |>
    mutate(end_p = map2(base_n, end_n, \(x, y) y / x))
}

grp_sex_demo <- function(df) {
  df |>
    select(-end_p) |>
    group_by(hsagrp) |>
    summarise(end_p_nohsa = sum(end_n) / sum(base_n))
}

# attach parameters to model results
attach_params <- function(df) {
  pmap(
    list(
      w = df,
      x = model_params$area_code,
      y = model_params$proj,
      z = model_params$end_year
    ),
    \(w, x, y, z) {
      w |>
        mutate(
          area_code = x,
          proj_var = y,
          end_year = z,
          .before = everything()
        )
    }
  )
}

# compute binning using pretty version of Freedman Diaconis rule
pretty_fd <- function(x) {

  brks <- pretty(range(x$end_p), n = nclass.FD(x$end_p), min.n = 1)
  ints <- cut(x$end_p, breaks = brks, right = FALSE)

  data <- tibble(
    x0 = head(brks, -1),
    x1 = tail(brks, -1),
    freq = summary(ints)
  )

  return(data)
}

# compute binning using Freedman Diaconis rule
freedman_diaconis <- function(x) {

  bins <- nclass.FD(x$end_p)
  ints <- cut(x$end_p, breaks = bins, right = TRUE)

  data <- tibble(
    x0 = head(brks, -1),
    x1 = tail(brks, -1),
    freq = summary(ints)
  )

  return(data)
}
