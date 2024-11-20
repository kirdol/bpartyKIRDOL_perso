test_that("cone_radius_vectorized_direct computes radius correctly", {
  # Test input values
  x_vals <- c(0, 4, 8, 8 + pi / 4, 8 + pi / 2)

  # Expected output values
  expected_radii <- c(0, 0.5, 1, 1 + 1.5 * sin(pi / 4), 2.5)

  # Check if the function output matches expected values
  result <- cone_radius_vectorized_direct(x_vals)
  expect_equal(result, expected_radii, tolerance = 1e-6)
})
