#' @title Variable radius function for a cone
#' @author Lodrik Adam
#' @description This function computes the radius of a cone at various heights (`x_vals`) using a piecewise formula. The formula defines three distinct regions with specific calculations for the radius, ensuring smooth transitions between them.
#' @param x_vals Numeric vector. A sequence of heights at which the radius is calculated.
#' @return A numeric vector of the same length as `x_vals`, where each element represents the radius at the corresponding height.
#' @details The radius is determined as follows:
#'   \itemize{
#'     \item For \code{0 <= x_vals < 8}: Radius is calculated as \code{x_vals / 8}.
#'     \item For \code{8 <= x_vals < 8 + pi/2}: Radius is \code{1 + 1.5 * sin(x_vals - 8)}.
#'     \item For \code{x_vals >= 8 + pi/2}: Radius is \code{2.5 - 2 * cos(x_vals - 8)}.
#'   }
#' @examples
#' # Radius at height 4.5
#' cone_radius_vectorized_direct(4.5)
#' # Generate a sequence of heights
#' heights <- seq(0, 12, length.out = 100)
#' # Compute the radius at each height
#' radii <- cone_radius_vectorized_direct(heights)
#' # Inspect the results
#' head(radii)
#' @export

# Define the variable radius function
cone_radius_vectorized_direct <- function(x_vals) {
  # Create a vector to store results, initialized to 0
  results <- numeric(length(x_vals))

  # Apply each condition using logical indexing
  results[x_vals >= 0 & x_vals < 8]           <- x_vals[x_vals >= 0 & x_vals < 8] / 8
  results[x_vals >= 8 & x_vals < 8 + pi / 2]  <- 1 + 1.5 * sin(x_vals[x_vals >= 8 & x_vals < 8 + pi / 2] - 8)
  results[x_vals >= 8 + pi / 2]               <- 2.5 - 2 * cos(x_vals[x_vals >= 8 + pi / 2] - 8)

  # Return the computed results
  results
}
