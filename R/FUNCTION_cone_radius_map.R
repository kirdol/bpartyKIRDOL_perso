#' @title Cone Radius Calculation (Vectorized using map)
#' @author Lodrik Adam
#' @description This function calculates the radius of a cone for each height value in a numeric vector `x_vals`. It uses the `map_dbl` function from the `purrr` package.
#' @param x_vals A numeric vector of input height values for which the cone radius will be calculated.
#' @return A numeric vector containing the calculated cone radius corresponding to each input height value in `x_vals`.
#' @examples
#' cone_radius_map(c(-1, 4, 8, 8 + pi / 4, 10))
#' cone_radius_map(c(-5, 15))
#' cone_radius_map(seq(0, 10, by = 2))
#' @importFrom purrr map_dbl
#' @export

# Define the function cone_radius_map
cone_radius_map <- function(x_vals) {
  map_dbl(x_vals, ~ {
    x <- .x
    if (x < 0) {
      0
    } else if (x >= 0 && x < 8) {
      x / 8
    } else if (x >= 8 && x < 8 + pi / 2) {
      1 + 1.5 * sin(x - 8)
    } else if (x >= 8 + pi / 2 && x < 10) {
      2.5 - 2 * cos(x - 8)
    } else {
      0
    }
  })
}
