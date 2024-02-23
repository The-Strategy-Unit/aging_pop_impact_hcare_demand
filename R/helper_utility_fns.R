# README
# Utility functions

# path_closure() ----
# helper function (closure) for paths
# param: area_code, type: string, local authority code
# param: base_year, type: int, model baseline year
path_closure <- function(area_code, base_year) {
  function(filename) {
    here("data", base_year, area_code, filename)
  }
}
