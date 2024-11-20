#' @title Get average temperature, humidity, and pressure for a city on a specific day
#' @author Lodrik Adam
#' @description This function retrieves weather forecast data for a given city using the OpenWeatherMap API
#' and calculates the average temperature (in Celsius), humidity (as a fraction), and pressure (in hPa)
#' for a specified day in the forecast.
#' @param city_name Character. The name of the city for which to retrieve the weather forecast.
#' @param api_key Character. Your OpenWeatherMap API key.
#' @param day_number Integer. The day in the forecast to retrieve, where 1 corresponds to the first day in the forecast. Should vary between 1 and 5.
#' @return The function returns a list with the following components:
#'   \itemize{
#'     \item \code{temperature}: Numeric. The average temperature for the specified day (in Celsius).
#'     \item \code{humidity}: Numeric. The average humidity for the specified day (as a fraction, e.g., 0.5 for 50%).
#'     \item \code{pressure}: Numeric. The average pressure for the specified day (in hPa).
#'   }
#' @import httr
#' @import jsonlite
#' @details
#' The function retrieves a 5-day forecast from the OpenWeatherMap API, filters the forecast data for the
#' specified day, and calculates the average temperature, humidity, and pressure. The temperature is
#' converted from Kelvin to Celsius, and humidity is converted to a fraction for convenience.
#' @examples
#' \dontrun{
#' # Example usage:
#' city <- "London"
#' api_key <- "your_api_key_here"
#' day <- 2
#'
#' # Get the average metrics for the second day in the forecast
#' metrics <- get_average_metrics_for_day(city_name = city, api_key = api_key, day_number = day)
#' print(metrics)
#' }
#' @export

# Function to get the average temperature, humidity, and pressure on a specific day
get_average_metrics_for_day <- function(city_name, api_key, day_number) {
  # Build the API URL
  base_url <- "http://api.openweathermap.org/data/2.5/forecast"
  query_params <- list(q = city_name, appid = api_key)

  # Make the API request
  response <- GET(url = base_url, query = query_params)

  # Check if the request was successful
  if (status_code(response) != 200) {
    stop("API request failed. Please check the city name and API key.")
  }

  # Parse the JSON content
  weather_data <- content(response, as = "text", encoding = "UTF-8")
  weather_json <- fromJSON(weather_data)

  # Extract the forecast list
  forecast_list <- weather_json$list

  # Convert 'dt_txt' to POSIXct date-time objects
  forecast_times <- as.POSIXct(forecast_list$dt_txt, format="%Y-%m-%d %H:%M:%S", tz="UTC")

  # Get the dates from the forecast times
  forecast_dates <- as.Date(forecast_times)

  # Find the unique dates in the forecast
  unique_dates <- unique(forecast_dates)

  # Check if there are enough days in the forecast
  if (length(unique_dates) < day_number) {
    stop("Not enough data to retrieve the specified day's forecast.")
  }

  # Get the date for the specified day
  target_date <- unique_dates[day_number]

  # Filter the forecast data for the target day
  target_day_indices <- which(forecast_dates == target_date)
  target_day_forecast <- forecast_list[target_day_indices, ]

  # Extract temperatures in Kelvin
  temps_kelvin <- as.numeric(target_day_forecast$main$temp)

  # Extract humidity (%)
  humidity <- as.numeric(target_day_forecast$main$humidity)

  # Extract pressure (in hPa)
  pressure <- as.numeric(target_day_forecast$main$pressure)

  # Calculate the average temperature, humidity, and pressure
  average_temp_kelvin <- mean(temps_kelvin)
  average_humidity <- mean(humidity)
  average_pressure <- mean(pressure)

  # Convert Kelvin to Celsius
  average_temp_celsius <- average_temp_kelvin - 273.15

  # Return the average metrics as a list
  return(list(
    temperature = average_temp_celsius,
    humidity = average_humidity / 100,  # Convert to fraction
    pressure = average_pressure
  ))
}
