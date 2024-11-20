# test that all vectorized cone radius functions return the same results
test_that("All cone radius functions return the same results", {
  inputs <- c(-5, -1, 0, 4, 7.99, 8, 8 + pi / 4, 8 + pi / 2, 9.5, 10, 15)

  # Expected results from the non-vectorized function
  expected_results <- sapply(inputs, cone_radius_non_vectorized)

  # Results from each function
  result_for <- cone_radius_for(inputs)
  result_map <- cone_radius_map(inputs)
  result_sapply <- cone_radius_sapply(inputs)
  result_vectorized <- cone_radius_vectorized(inputs)

  # Compare results
  expect_equal(result_for, expected_results, label = "cone_radius_for results match")
  expect_equal(result_map, expected_results, label = "cone_radius_map results match")
  expect_equal(result_sapply, expected_results, label = "cone_radius_sapply results match")
  expect_equal(result_vectorized, expected_results, label = "cone_radius_vectorized results match")
})
