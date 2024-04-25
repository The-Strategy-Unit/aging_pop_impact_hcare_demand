# README
# Register all font styles for use by ragg package
# https://github.com/yjunechoe/junebug/
# https://yjunechoe.github.io/posts/2021-06-24-setting-up-and-debugging-custom-fonts/ # nolint: line_length_linter.


# packages ----
library("dplyr")
library("purrr")
library("systemfonts")

# font_hoist ----
font_hoist <- function(family, silent = FALSE) {
  font_specs <- systemfonts::system_fonts() |>
    dplyr::filter(family == .env[["family"]]) |>
    dplyr::mutate(family = paste(.data[["family"]], .data[["style"]])) |>
    dplyr::select(plain = "path", name = "family")

  purrr::pwalk(as.list(font_specs), systemfonts::register_font)

  if (!silent) message(
    paste0(
      "Hoisted ",
      nrow(font_specs),
      " variants:\n",
      paste(font_specs$name, collapse = "\n")
    )
  )
}
