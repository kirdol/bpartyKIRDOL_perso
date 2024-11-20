#' @title Function to calculate both volume and surface area of a cone with radius variations
#' @author Lodrik Adam
#' @description This function calculates the volume and surface area of a cone, where the radius varies along the height due to random perturbations. The calculation uses numerical integration.
#' @param h_i Numeric. The total height of the cone.
#' @return A list with two elements:
#'   \describe{
#'     \item{volume}{Numeric. The computed volume of the cone.}
#'     \item{surface_area}{Numeric. The computed surface area of the cone.}
#'   }
#' @importFrom stats rnorm
#' @examples
#' calculate_cone_metrics(10)
#' calculate_cone_metrics(h_i = 10)
#' calculate_cone_metrics(5)
#' calculate_cone_metrics(h_i = 5)
#' @export

calculate_cone_metrics <- function(h_i) {
  # Validate input
  if (!is.numeric(h_i) || length(h_i) != 1 || is.na(h_i) || h_i <= 1) {
    return(list(volume = NA, surface_area = NA)) # Return NA for invalid input
  }

  # Number of points for numerical integration
  N <- 1000

  # Generate x values from 0 to h_i
  x_vals <- seq(0, h_i, length.out = N)

  # Compute the base radius at each x
  r_base <- cone_radius_vectorized_direct(x_vals)

  # Generate random variations at each x
  delta_r <- rnorm(1, mean = 0, sd = 0.1)

  # Adjust the radius with variations
  r_adj <- r_base + delta_r

  # Ensure radius is non-negative
  r_adj <- pmax(r_adj, 0)

  # Calculate the differential element dx
  dx <- h_i / (N - 1)

  # Volume calculation using the disk method (numerical integration)
  volume_elements <- pi * r_adj^2 * dx
  volume <- sum(volume_elements, na.rm = TRUE) # Handle any potential NA values

  # Surface area calculation
  # Compute dr/dx using finite differences
  dr_dx <- c(0, diff(r_adj) / dx)
  # Compute the integrand for surface area at each x
  surface_area_elements <- 2 * pi * r_adj * sqrt(1 + dr_dx^2) * dx
  surface_area <- sum(surface_area_elements, na.rm = TRUE) # Handle any potential NA values

  # Return both volume and surface area
  return(list(volume = volume, surface_area = surface_area))
}
