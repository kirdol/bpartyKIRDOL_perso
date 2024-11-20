test_that("get_average_metrics_for_day returns valid metrics", {
  skip_on_cran() # Skip this test on CRAN since it depends on API calls

  # Mock inputs
  city_name <- "Lausanne"
  api_key <- "13235fdf9df0b97304e7c6210a0a0f94"
  day_number <- 2

  # Mock API response
  result <- get_average_metrics_for_day(city_name, api_key, day_number)

  # Check result structure
  expect_named(result, c("temperature", "humidity", "pressure"))

  # Check if returned values are numeric
  expect_type(result$temperature, "double")
  expect_type(result$humidity, "double")
  expect_type(result$pressure, "double")
})
