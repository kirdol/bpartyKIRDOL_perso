#' @title Cone Radius Calculation (Vectorized version using Vectorize function)
#' @author Lodrik Adam
#' @description This function calculates the radius of a cone based on a single input height value `x` using vectorized logic. The function handles specific conditions to determine the radius based on the input value. It uses the `cone_radius_non_vectorized`function.
#' @param x A numeric height value representing the input for which the cone radius will be calculated.
#' @return A numeric value representing the calculated radius for the height input. Returns 0 for inputs outside the defined range.
#' @examples
#' cone_radius_vectorized(4)
#' cone_radius_vectorized(8 + pi / 4)
#' cone_radius_vectorized(-1)
#' cone_radius_vectorized(10)
#' @export

# Define the function cone_radius_vectorized
cone_radius_vectorized <- function(x) {
  result <- Vectorize(cone_radius_non_vectorized)(x)
  return(as.numeric(result))
}
