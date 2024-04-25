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

lookup_vars_id <- tribble(
  ~"proj_id", ~"vars_id",
  "principal_proj", "v1",
  "hpp", "v2",
  "lpp", "v3",
  "php", "v4",
  "plp", "v5",
  "var_proj_high_intl_migration", "v6",
  "var_proj_low_intl_migration", "v7",
  "hhh", "v8",
  "lll", "v9",
  "hlh", "v10",
  "lhl", "v11"
)

vars_id_levels <- c(paste0("v", 0:11))

# hsa groups
hsagrps_app <- c(
  "amb",
  "walkin",
  "daycase_n",
  "emer_n",
  "emer_bds",
  "ordelec_n",
  "ordelec_bds",
  "non-surg_first",
  "non-surg_fup",
  "non-surg_proc",
  "surg_first",
  "surg_fup",
  "surg_proc"
)

# hsa group labels
hsagrp_labs <- tribble(
  ~"hsagrp", ~"hsagrp_lab",
  "amb", "Ambulance arrivals",
  "walkin", "Walk-in arrivals",
  "daycase_n", "Daycases",
  "emer_n", "Unplanned admissions",
  "emer_bds", "Unplanned bed days",
  "ordelec_n", "Elective admissions",
  "ordelec_bds", "Elective bed days",
  "non-surg_first", "First app. (non-surgical specialties)",
  "non-surg_fup", "Follow-up app. (non-surgical specialties)",
  "non-surg_proc", "Procedure (non-surgical specialties)",
  "surg_first", "First app. (surgical specialties)",
  "surg_fup", "Follow-up app. (surgical specialties)",
  "surg_proc", "Procedure (surgical specialties)"
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

lookup_pod <- tribble(
  ~"hsagrp", ~"pod",
  "walkin", "aae",
  "amb", "aae",
  "emer_n", "apc",
  "emer_bds", "apc",
  "daycase_n", "apc",
  "ordelec_n", "apc",
  "ordelec_bds", "apc",
  "surg_proc", "opc",
  "non-surg_proc", "opc",
  "surg_first", "opc",
  "non-surg_first", "opc",
  "surg_fup", "opc",
  "non-surg_fup", "opc"
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

grp_sex_hsa_mode <- function(df) {
  df |>
    select(-end_p) |>
    group_by(hsagrp) |>
    summarise(end_p_md = sum(end_n) / sum(base_n))
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
  if (max(x$end_p) - min(x$end_p) < .1) {
    brks <- pretty(range(x$end_p), n = 20, min.n = 1)
    ints <- cut(x$end_p, breaks = brks, right = FALSE)
  } else {
    brks <- pretty(
      range(x$end_p),
      n = nclass.FD(x$end_p, digits = 5),
      min.n = 1
    )
    ints <- cut(x$end_p, breaks = brks, right = FALSE)
  }
  data <- tibble(
    x0 = head(brks, -1),
    x1 = tail(brks, -1),
    freq = summary(ints, maxsum = 200)
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
