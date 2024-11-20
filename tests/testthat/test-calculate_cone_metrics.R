test_that("calculate_cone_metrics handles invalid inputs gracefully", {
  # Test for height less than or equal to 1
  expect_equal(calculate_cone_metrics(1), list(volume = NA, surface_area = NA))
  expect_equal(calculate_cone_metrics(0.5), list(volume = NA, surface_area = NA))

  # Test for negative height
  expect_equal(calculate_cone_metrics(-10), list(volume = NA, surface_area = NA))

  # Test for non-numeric input
  expect_equal(calculate_cone_metrics("invalid_input"), list(volume = NA, surface_area = NA))

  # Test for NA input
  expect_equal(calculate_cone_metrics(NA), list(volume = NA, surface_area = NA))
})
