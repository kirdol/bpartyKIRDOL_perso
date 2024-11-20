#' @title Cone Radius Calculation (Vectorized using sapply)
#' @author Lodrik Adam
#' @description This function calculates the radius of a cone for each height value in a numeric vector `x_vals` using `sapply`. It applies conditional logic to determine the radius for each input value.
#' @param x_vals A numeric vector of input height values for which the cone radius will be calculated.
#' @return A numeric vector containing the calculated cone radius corresponding to each input height value in `x_vals`.
#' @examples
#' cone_radius_sapply(c(-1, 4, 8, 8 + pi / 4, 10))
#' cone_radius_sapply(c(-5, 15))
#' cone_radius_sapply(seq(0, 10, by = 2))
#' @export

# Define the function cone_radius_sapply
cone_radius_sapply <- function(x_vals) {
  if (length(x_vals) == 0) {
    return(numeric(0)) # return an empty numeric vector
  }
  sapply(x_vals, function(x) {
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
