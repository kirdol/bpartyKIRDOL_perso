# test that cone_radius_non_vectorized returns 0 for negative input
test_that("cone_radius_non_vectorized returns 0 for negative input", {
  expect_equal(cone_radius_non_vectorized(-5), 0)
  expect_equal(cone_radius_non_vectorized(-0.1), 0)
})

# test that cone_radius_non_vectorized returns 0 for x >= 10
test_that("cone_radius_non_vectorized returns 0 for x >= 10", {
  expect_equal(cone_radius_non_vectorized(10), 0)
  expect_equal(cone_radius_non_vectorized(15), 0)
})

# test that cone_radius_non_vectorized returns an error for vector input
test_that("cone_radius_non_vectorized fails for vector input", {
  expect_error(cone_radius_non_vectorized(c(0, 5, 3)),
               "the condition has length > 1")
})
