#' @title Cone Radius Calculation (Non-Vectorized)
#' @author Lodrik Adam
#' @description This function calculates the radius of a cone based on the input height `x` according to specific conditions.
#' @param x A numeric value representing the input height to calculate the cone radius. Must be a single number.
#' @return A numeric value representing the calculated radius for the given height. Returns 0 for inputs outside the defined range.
#' @examples
#' cone_radius_non_vectorized(4)
#' cone_radius_non_vectorized(9)
#' cone_radius_non_vectorized(15)
#' @export

# Non-vectorized function for cone radius calculation
cone_radius_non_vectorized <- function(x) {
  if (x < 0) {
    return(0)
  } else if (x >= 0 && x < 8) {
    return(x / 8)
  } else if (x >= 8 && x < 8 + pi / 2) {
    return(1 + 1.5 * sin(x - 8))
  } else if (x >= 8 + pi / 2 && x < 10) {
    return(2.5 - 2 * cos(x - 8))
  } else {
    return(0)
  }
}
