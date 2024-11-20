
#' @title Shiny Application Server Logic
#' @author Lodrik Adam
#' @description Defines the server logic for the Shiny application.
#' @param input The input values from the Shiny application.
#' @param output The output values from the Shiny application.
#' @param session The Shiny session object.
#' @return Server logic for the Shiny application.
#' @importFrom shiny showModal modalDialog tagList modalButton removeModal observe
#' @keywords internal

# Define Server Logic
server <- function(input, output, session) {

  # Reactive value to store the user-provided API key
  user_api_key <- reactiveVal(NULL)

  # Function to show the API Key Modal
  show_api_key_modal <- function() {
    value <- NULL
    showModal(modalDialog(
      title = "Enter OpenWeatherMap API Key",
      textInput("api_key_input", "API Key:", value = "", placeholder = "Your API Key Here"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_api_key", "Save API Key")
      ),
      easyClose = FALSE
    ))
  }

  # Show the modal when the app launches
  observeEvent(TRUE, {
    show_api_key_modal()
  }, once = TRUE)

  # Observe the 'Save API Key' button in the modal to update the API key
  observeEvent(input$save_api_key, {
    if (nzchar(input$api_key_input)) {
      user_api_key(input$api_key_input)
      removeModal()
      showNotification("API Key saved successfully!", type = "message")
    } else {
      showNotification("Please enter a valid API Key.", type = "error")
    }
  })

  # Ensure that the API key is provided before initializing modules
  observe({
    req(user_api_key())
    # Initialize server modules with the user-provided API key
    cityModuleServer("1", user_api_key)
    cityModuleServer("2", user_api_key)
    cityModuleServer("3", user_api_key)
  })
}

#' @title City Module UI Function
#' @author Lodrik Adam
#' @description Creates the UI for the city module, allowing users to input a city name, select a date, choose parameters, and view results.
#' @param id A unique identifier for the module's namespace.
#' @return A `tabPanel` containing the UI elements for the city module.
#' @importFrom shiny NS tabPanel sidebarLayout sidebarPanel textInput uiOutput selectInput actionButton numericInput tags tableOutput plotOutput mainPanel plotOutput wellPanel h4 strong p HTML
#' @keywords internal

# Define the City Module UI
cityModuleUI <- function(id) {
  value <- NULL
  ns <- NS(id)
  tabPanel(
    title = paste("City", id),
    sidebarLayout(
      sidebarPanel(
        textInput(ns("city_name"), "Enter City Name:", value = "Lausanne"),
        uiOutput(ns("date_selector_ui")),
        selectInput(ns("parameter"), "Metric to display",
                    choices = c("Temperature", "Humidity", "Pressure"), selected = "Temperature"),
        actionButton(ns("submit_city"), "Submit City"),
        numericInput(ns("num_simulations"), "Number of Simulations:", value = 10000, min = 1),
        actionButton(ns("run_simulations"), "Run Simulations"),
        tags$hr(),
        uiOutput(ns("welcome_text")),
        uiOutput(ns("simulation_results"))
      ),
      mainPanel(
        plotOutput(ns("forecastPlot")),
        plotOutput(ns("volume_histogram")),
        plotOutput(ns("surface_area_histogram"))
      )
    )
  )
}

#' @title City Module Server Function
#' @author Lodrik Adam
#' @description Server logic for the city module, handling data retrieval from the OpenWeatherMap API, running simulations, and generating outputs.
#' @param id A unique identifier matching the module's UI `id`.
#' @param api_key_reactive A reactive expression containing the API key for accessing the OpenWeatherMap API.
#' @return Server logic for the city module.
#' @importFrom shiny moduleServer reactiveVal reactiveValues observeEvent withProgress showNotification renderUI selectInput req reactive renderTable renderPlot wellPanel h4 strong p tableOutput HTML incProgress
#' @import httr
#' @import jsonlite
#' @import ggplot2
#' @import parallel
#' @import here
#' @import stats
#' @keywords internal

# Define the City Module Server
cityModuleServer <- function(id, api_key_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values to store forecast data and dates
    forecast_data <- reactiveVal()
    forecast_dates <- reactiveVal()

    # Reactive values to store simulation results
    sim_data <- reactiveValues(
      total_volumes = NULL,
      total_surface_areas = NULL,
      num_guests_list = NULL,
      simulation_results_text = NULL
    )

    # Observe event when city is submitted
    observeEvent(input$submit_city, {
      city_name <- input$city_name
      value <- NULL

      # Fetch forecast data
      withProgress(message = 'Fetching forecast data...', value = 0, {
        tryCatch({
          base_url <- "http://api.openweathermap.org/data/2.5/forecast"
          query_params <- list(q = city_name, units = "metric", appid = api_key_reactive())
          response <- GET(url = base_url, query = query_params)
          if (status_code(response) != 200) {
            stop("API request failed for city: ", city_name, ". Please check the city name and API key.")
          }
          weather_data <- content(response, as = "text", encoding = "UTF-8")
          weather_json <- fromJSON(weather_data)
          forecast_list <- weather_json$list
          forecast_data(forecast_list)

          forecast_times <- as.POSIXct(forecast_list$dt_txt, format="%Y-%m-%d %H:%M:%S", tz="UTC")
          unique_dates_char <- as.character(unique(as.Date(forecast_times)))
          forecast_dates(unique_dates_char)
        }, error = function(e) {
          showNotification(paste("Error fetching forecast data for city", city_name, ":", e$message), type = "error")
        })
      })
    })

    # Render date selector UI
    output$date_selector_ui <- renderUI({
      req(forecast_dates())
      selectInput(ns("selected_date"), "Select a Date:", choices = forecast_dates())
    })

    # Reactive expression to compute average metrics
    avg_metrics <- reactive({
      req(forecast_data())
      req(input$selected_date)
      forecast_list <- forecast_data()
      forecast_times <- as.POSIXct(forecast_list$dt_txt, format="%Y-%m-%d %H:%M:%S", tz="UTC")
      forecast_df <- data.frame(
        date = as.Date(forecast_times),
        temp = forecast_list$main$temp,
        humidity = forecast_list$main$humidity,
        pressure = forecast_list$main$pressure
      )
      day_data <- forecast_df[forecast_df$date == as.Date(input$selected_date), ]
      if (nrow(day_data) == 0) {
        showNotification(paste("No forecast data available for", input$city_name, "on", input$selected_date), type = "error")
        return(NULL)
      }
      avg_temp <- mean(day_data$temp)
      avg_humidity <- mean(day_data$humidity) / 100
      avg_pressure <- mean(day_data$pressure)
      list(
        temperature = avg_temp,
        humidity = avg_humidity,
        pressure = avg_pressure
      )
    })

    # Render welcome text
    output$welcome_text <- renderUI({
      req(avg_metrics())
      avg_metrics_val <- avg_metrics()
      wellPanel(
        h4(strong(paste("Weather Forecast for", input$city_name, "on", input$selected_date))),
        p("The weather forecast indicates the following average conditions:"),
        tableOutput(ns("weather_table"))
      )
    })

    # Weather table
    output$weather_table <- renderTable({
      req(avg_metrics())
      avg_metrics_val <- avg_metrics()
      value <- NULL
      data.frame(
        Metric = c("Temperature", "Humidity", "Pressure"),
        Value = c(
          paste0(round(avg_metrics_val$temperature, 2), " \u00B0CC"),
          paste0(round(avg_metrics_val$humidity * 100, 2), " %"),
          paste0(round(avg_metrics_val$pressure, 2), " hPa")
        ),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")

    # Render forecast plot
    output$forecastPlot <- renderPlot({
      req(forecast_data())
      req(input$parameter)
      forecast_list <- forecast_data()
      forecast_times <- as.POSIXct(forecast_list$dt_txt, format="%Y-%m-%d %H:%M:%S", tz="UTC")
      value <- NULL
      parameter_data <- switch(input$parameter,
                               "Temperature" = forecast_list$main$temp,
                               "Humidity" = forecast_list$main$humidity,
                               "Pressure" = forecast_list$main$pressure)
      forecast_df <- data.frame(date = forecast_times, value = parameter_data)
      unit <- switch(input$parameter, "Temperature" = "\u00B0CC", "Humidity" = "%", "Pressure" = "hPa")
      ggplot(forecast_df, aes(x = date, y = value)) +
        geom_line(color = "blue") +
        labs(
          title = paste("5-Day", input$parameter, "Forecast for", input$city_name),
          x = "Date",
          y = paste(input$parameter, "(", unit, ")", sep = "")
        ) +
        theme_minimal()
    })

    # Observe event when 'Run Simulations' button is clicked
    observeEvent(input$run_simulations, {
      req(avg_metrics())
      avg_metrics_val <- avg_metrics()
      Temperature_i <- avg_metrics_val$temperature
      Humidity_i <- avg_metrics_val$humidity
      Pressure_i <- avg_metrics_val$pressure
      lambda_i <- exp(0.5 + 0.5 * Temperature_i - 3 * Humidity_i + 0.001 * Pressure_i)
      num_simulations <- input$num_simulations
      num_cores <- detectCores()
      value <- NULL
      cl <- makeCluster(num_cores)
      clusterExport(cl, varlist = c("lambda_i", "calculate_cone_metrics", "cone_radius_vectorized_direct"), envir = environment())
      withProgress(message = 'Running simulations...', value = 0, {
        sim_results <- parLapply(cl, 1:num_simulations, function(sim) {
          set.seed(123 + sim)
          num_guests <- rpois(1, lambda = lambda_i)
          if (num_guests == 0) {
            return(list(total_volume = 0, total_surface_area = 0, num_guests = 0))
          }
          cones_per_guest <- sample(c(1, 2), size = num_guests, replace = TRUE, prob = c(0.67, 0.33))
          total_cones <- sum(cones_per_guest)
          cone_volumes <- numeric(total_cones)
          cone_surface_areas <- numeric(total_cones)
          for (cone in 1:total_cones) {
            h_i <- 10 + rnorm(1, mean = 0, sd = 0.1)
            metrics <- calculate_cone_metrics(h_i)
            cone_volumes[cone] <- metrics$volume
            cone_surface_areas[cone] <- metrics$surface_area
          }
          total_volume <- sum(cone_volumes)
          total_surface_area <- sum(cone_surface_areas)
          return(list(
            total_volume = total_volume,
            total_surface_area = total_surface_area,
            num_guests = num_guests
          ))
        })
        incProgress(1)
      })
      stopCluster(cl)
      total_volumes <- sapply(sim_results, function(res) res$total_volume)
      total_surface_areas <- sapply(sim_results, function(res) res$total_surface_area)
      num_guests_list <- sapply(sim_results, function(res) res$num_guests)
      mean_num_guests <- mean(num_guests_list)
      mean_volume <- mean(total_volumes)
      sd_volume <- sd(total_volumes)
      volume_99th_percentile <- quantile(total_volumes, 0.99)
      mean_surface_area <- mean(total_surface_areas)
      sd_surface_area <- sd(total_surface_areas)
      surface_area_99th_percentile <- quantile(total_surface_areas, 0.99)

      # Update reactive values
      sim_data$total_volumes <- total_volumes
      sim_data$total_surface_areas <- total_surface_areas
      sim_data$num_guests_list <- num_guests_list

      sim_data$mean_num_guests <- mean_num_guests
      sim_data$volume_99th_percentile <- volume_99th_percentile
      sim_data$surface_area_99th_percentile <- surface_area_99th_percentile

      sim_data$simulation_results_text <- HTML(paste0(
        "<h4>Simulation Results for ", input$city_name, "</h4>",
        "<p>To satisfy the average <b>", round(mean_num_guests, 2), " guests</b> on <b>", input$selected_date, "</b> with a 99% chance, you will need:</p>",
        "<ul>",
        "<li>Approximately <b>", round(volume_99th_percentile, 2), " cubic cm</b> of ice cream</li>",
        "<li>Approximately <b>", round(surface_area_99th_percentile, 2), " square cm</b> of coating</li>",
        "</ul>"
      ))
    })

    # Render volume histogram
    output$volume_histogram <- renderPlot({
      req(sim_data$total_volumes)
      volumes_df <- data.frame(total_volumes = sim_data$total_volumes)
      ggplot(volumes_df, aes(x = total_volumes)) +
        geom_histogram(color = "black", fill = "blue", bins = 50, alpha = 0.5) +
        labs(title = "Histogram of Total Volumes per Party",
             x = "Total Volume (cubic cm)",
             y = "Frequency") +
        theme_minimal()
    })

    # Render surface area histogram
    output$surface_area_histogram <- renderPlot({
      req(sim_data$total_surface_areas)
      surface_areas_df <- data.frame(total_surface_areas = sim_data$total_surface_areas)
      ggplot(surface_areas_df, aes(x = total_surface_areas)) +
        geom_histogram(color = "black", fill = "green", bins = 50, alpha = 0.5) +
        labs(title = "Histogram of Total Surface Areas per Party",
             x = "Total Surface Area (square cm)",
             y = "Frequency") +
        theme_minimal()
    })

    output$simulation_results <- renderUI({
      req(sim_data$mean_num_guests)
      wellPanel(
        h4(strong(paste("Simulation Results for", input$city_name))),
        p(
          "To satisfy the average of ",
          strong(round(sim_data$mean_num_guests, 2), " guests"),
          " on ",
          strong(input$selected_date),
          " with a 99% chance, you will need:"
        ),
        tableOutput(ns("simulation_table"))
      )
    })

    # Add this to render the simulation results table
    output$simulation_table <- renderTable({
      data.frame(
        Item = c("Ice Cream Volume", "Coating Surface Area"),
        Amount = c(
          paste0(round(sim_data$volume_99th_percentile, 2), " cubic cm"),
          paste0(round(sim_data$surface_area_99th_percentile, 2), " square cm")
        ),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "s")
  })
}

#' @title Shiny Application UI
#' @author Lodrik Adam
#' @description Defines the user interface for the Shiny application.
#' @return A `fluidPage` containing the application's UI.
#' @importFrom shiny fluidPage titlePanel tabsetPanel
#' @import shinythemes
#' @keywords internal

# Define UI for the application
ui <- fluidPage(
  theme = shinythemes::shinytheme("cerulean"),
  titlePanel("Climate Factors and Ice Cream Simulation"),

  # Tabs for City Modules
  tabsetPanel(
    cityModuleUI("1"),
    cityModuleUI("2"),
    cityModuleUI("3")
  )
)

#' @title Launch the Ice Cream Party Planner Shiny Application
#' @author Lodrik Adam
#' @description
#' This function initializes and runs the Shiny application for analyzing climate factors
#' and simulating ice cream cone requirements based on weather forecasts. Upon launching,
#' users are prompted to enter their OpenWeatherMap API key. The application allows users
#' to input a city name, select a date, choose weather parameters to display, and view
#' forecast data alongside simulation results for ice cream volume and surface area for up
#' to 3 cities simultaneously.
#'
#' Ensure you have a valid OpenWeatherMap API key before launching the application.
#' The application utilizes parallel processing to enhance simulation performance.
#' @return
#' Lauches the Shiny application for analyzing climate factors and simulating ice cream cone requirements. The app provides the following features:
#'   \itemize{
#'     \item API Key Input: Securely enter and store your OpenWeatherMap API key.
#'     \item City Modules: Three separate modules to analyze different cities simultaneously.
#'     \item Weather Forecast: View 5-day forecasts for temperature, humidity, and pressure.
#'     \item Simulations: Run simulations to determine ice cream cone requirements based on the selected weather metrics.
#'     \item Visualizations: Interactive plots and histograms to visualize forecast data and simulation outcomes.
#'   }
#' @examples
#' \dontrun{
#' run_ice_cream_party_planner()
#' }
#' @importFrom shiny shinyApp
#' @export

run_ice_cream_party_planner <- function() {
  shinyApp(ui = ui, server = server)
}

