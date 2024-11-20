#' @title Cone Radius Calculation (Vectorized using for loop)
#' @author Lodrik Adam
#' @description This function calculates the radius of a cone for each height value in a numeric vector `x_vals`. It applies conditional logic to determine the radius based on the input value.
#' @param x_vals A numeric vector of input height values for which the cone radius will be calculated.
#' @return A numeric vector containing the calculated cone radius corresponding to each input height values in `x_vals`.
#' @examples
#' cone_radius_for(c(-1, 4, 8, 8 + pi / 4, 10))
#' cone_radius_for(c(-5, 15))
#' cone_radius_for(seq(0, 10, by = 2))
#' @export

# Define the function cone_radius_for
cone_radius_for <- function(x_vals) {
  results <- numeric(length(x_vals))
  for (i in seq_along(x_vals)) {
    x <- x_vals[i]
    results[i] <- if (x < 0) {
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
  }
  results
}
