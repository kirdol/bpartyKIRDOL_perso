<!-- badges: start -->
[![R-CMD-check](https://github.com/ptds2024/bpartyKIRDOL/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ptds2024/bpartyKIRDOL/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## IMPORTANT

**NOTE:** At the time of writing, there is an issue with the Actions workflow affecting the R-CMD-check and the website. The action workflow had been functioning perfectly for several days, but since Wednesday around 18:00, it has stopped working. I am now receiving the following error message:

> The job was not started because recent account payments have failed or your spending limit needs to be increased. Please check the 'Billing & plans' section in your settings.

I have a Pro account, and upon reviewing my ‘Billing & plans’ section, there are no indications of failed payments or exceeded spending limits. Could it be possible that it is link to the ptds2024 organization account?
I apologize for the inconvenience in advance and hope it won't affect the evaluation of my project.

# bpartyKIRDOL

## Description

bpartyKIRDOL is my custom R package that provides tools for calculating the radius based on the height of a cone. Additionally, it includes a Shiny application that displays a 5-day weather forecast for three cities simultaneously and simulates the required volume of ice cream and the surface area of ice cream coating needed for a party.

## Features

**Functions to compute the radius per height of a cone:**

- `cone_radius_non_vectorized`: Calculate the radius of a cone based on its height.
- `cone_radius_map`: Calculate the radius of a cone based on its height using the `map` function.
- `cone_radius_for`: Calculate the radius of a cone based on its height using `for`loops.
- `cone_radius_sapply`: Calculate the radius of a cone based on its height using the `sapply` function.
- `cone_radius_vectorized`: Calculate the radius of a cone based on its height using the `Vectorize` function.
- `cone_radius_vectorized_direct`: Calculate the radius of a cone based on its height using vectorized operations.

**Other functions:**

- `calculate_cone_metrics`: Calculate the volume and surface area of a cone based on its height. Utilize the `cone_radius_vectorized_direct` function to calculate the radius at each height. It apply a slight random variation to the radius.
- `get_average_metrics_for_day`: Use [OpenWeather.org](https://openweathermap.org/forecast5) "5 day weather forecast" to gets the forecast for the next 5 days. It then gives the average temperature, pressure and humidity for a given day in a list.

**Shiny application:**

- `run_ice_cream_party_planner`: Run a Shiny application that simulates the required volume of ice cream and the surface area of ice cream coating needed for a party. It also displays a 5-day weather forecast for three cities simultaneously.

## Installation

Since this package is not published on CRAN or any other public repository, you need to clone the repository and install it manually.

1. Clone the Repository using GitHub Desktop or using commands in the terminal
2. Open the project in RStudio
3. Install the package by clicking on "Clean and Install" in the Build tab and then More dropdown menu in the top right corner of RStudio
4. You can then use the package as you want

**Running the tests:**

To run the tests, you can run `devtools::test()` in the console. This will run the tests and show you the results. You can also click on the Test button in the Build tab of RStudio.

## Dependencies

The package depends on the following packages:

- httr (for API calls)
- jsonlite (for JSON parsing)
- stats (for mathematical functions)
- shiny (for the Shiny application)
- ggplot2 (for plotting)
- shinythemes (for the Shiny application theme)
- here (for file paths)
- parallel (for parallel processing)
- purrr (for functional programming)

The following packages have been use for testing, the website and the R-CMD-check:

- testthat (for testing)
- usethis (for package development)
- pkgdown (for website)

## Note

The app will ask for your API key for OpenWeather.org. You can get a free API key by signing up on their website.

## License

This project is licensed under the MIT License - see the LICENSE file for more details.

## Contact

- Author: Lodrik Adam
- Email: lodrik.adam@unil.ch
- GitHub: kirdol


