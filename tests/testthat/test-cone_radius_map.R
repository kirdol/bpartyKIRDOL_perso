# test that cone_radius_map returns the correct values for negative inputs
test_that("cone_radius_map handles negative inputs", {
  result <- cone_radius_map(c(-5, -1, -0.1))
  expect_equal(result, c(0, 0, 0))
})

# test that cone_radius_map returns the correct values for higher than the defined range
test_that("cone_radius_map handles x >= 10 correctly", {
  result <- cone_radius_map(c(10, 15))
  expect_equal(result, c(0, 0))
})

# test that cone_radius_map returns the correct values for mixed inputs
test_that("cone_radius_map handles mixed inputs", {
  result <- cone_radius_map(c(-1, 0, 4, 8, 8 + pi / 4, 10, 15))
  expected <- c(0, 0, 0.5, 1, 1 + 1.5 * sin(pi / 4), 0, 0)
  expect_equal(result, expected)
})

# test that cone_radius_map handles empty input
test_that("cone_radius_map handles empty input", {
  result <- cone_radius_map(numeric(0))
  expect_equal(result, numeric(0))
})
