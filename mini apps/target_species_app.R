library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(tidyr)
library(zoo)
#library(reticulate)
library(thematic)
library(scales)
library(Metrics)
library(forecast)

# Load and preprocess your bird observation data
bird_data <- read.csv("data/grouped_month.csv")
bird_data$observation_date <- as.Date(bird_data$observation_date)
bird_data$month_year <- format(bird_data$observation_date, "%Y-%m")

# # Define the mean_absolute_percentage_error function in R

mean_absolute_percentage_error <- function(true_values, predicted_values) {
  error <- true_values - predicted_values
  print(error)
  print(true_values)
  absolute_percentage_error <- abs(error / true_values)
  print(absolute_percentage_error)
  mape <- mean(absolute_percentage_error) * 100
  return(mape)
}

# UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  titlePanel(
    "Time Series Analysis for Target Bird Species in Knox County"
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Select Bird Species", choices = unique(bird_data$common_name)),
      imageOutput("bird_image")
    ),
    mainPanel(
      plotlyOutput("time_series_plot_R"),
      plotlyOutput("time_series_plot_py"),
      textOutput("test_mape"),
      textOutput("test_mae"),
      textOutput("test_smape"),
      textOutput("test_rmse")
    )
  ) 
)

# Server
server <- function(input, output) {
  thematic::thematic_shiny() # works only for ggplot2, lattice, base plots
  observeEvent(input$species, {
    target_species <- input$species
    
    # Filter the bird observation data for the target species
    target_species_df <- bird_data[bird_data$common_name == target_species, ]
    
    # Group the data by month and average the counts
    target_species_month <- target_species_df %>%
      group_by(month_year) %>%
      summarise(observation_count = mean(observation_count))
    
    # Convert month_year to yearmon object
    target_species_month$month_year <- as.yearmon(target_species_month$month_year, format = "%Y-%m")
    
    # Convert yearmon to numeric representation for sequence generation
    target_species_month$month_year_numeric <- as.numeric(target_species_month$month_year)
    
    # Complete the target_species_month dataframe with missing months and set observation_count to 0
    target_species_month <- target_species_month %>%
      complete(month_year_numeric = seq(min(target_species_month$month_year_numeric), 
                                        max(target_species_month$month_year_numeric), by = 1/12)) %>%
      mutate(month_year = as.yearmon(month_year_numeric),
             observation_count = replace(observation_count, is.na(observation_count), 0)) %>%
      select(-month_year_numeric)
    
    # Time series data for training and testing
    train <- target_species_month[target_species_month$month_year <= "2021-01-01", "observation_count"]
    test <- target_species_month[target_species_month$month_year > "2021-01-01", "observation_count"]
    
    # Pass train and test data to Python
    py$train <- train
    py$test <- test

    # Run SARIMAX model
    py_run_string("
from statsmodels.tsa.statespace.sarimax import SARIMAX
import numpy as np

model_py = SARIMAX(train, order=(1, 0, 2), seasonal_order=(0, 1, 0, 12), trend='c')
model_fit = model_py.fit(disp=False, maxiter = 200)
predictions = model_fit.predict(start=len(train), end=len(train)+len(test)-1)
predictions_np = np.array(predictions)
predictions_list = predictions_np.tolist()
")

    # Retrieve the predicted values from Python
    predictions_list <- py$predictions_list

    # Convert predictions to an R vector
    predictions <- as.vector(predictions_list) %>%
      round(0)
    # Filter the target_species_month data for train and test sets
    train_dates <- target_species_month$month_year[target_species_month$month_year <= as.yearmon("2021-01")]
    test_dates <- target_species_month$month_year[target_species_month$month_year > as.yearmon("2021-01")]
    
    # Filter the observation counts for train and test sets
    train_counts <- target_species_month$observation_count[target_species_month$month_year <= as.yearmon("2021-01")] 
    test_counts <- target_species_month$observation_count[target_species_month$month_year > as.yearmon("2021-01")]
    
    # Round counts for plotly tooltip
    train_counts <- train_counts %>%
      round(0)
    test_counts <- test_counts %>%
      round(0)
    
    # convert to date objects for plotly graph
    date_test <- as.Date(paste(test_dates, "01"), format = "%b %Y %d")          
    date_train <- as.Date(paste(train_dates, "01"), format = "%b %Y %d")  
    
    #### FOR PYTHON SARIMA
    # Create the time series plot using ggplot2
    
    time_series_plot_py <- ggplot() +
      geom_line(data = data.frame(x = date_train, y = train_counts),
                aes(x = x, y = y, color = "Actual (Train)"),
                linetype = "solid",
                linewidth = 1,
                na.rm = TRUE) +
      geom_line(data = data.frame(x = date_test, y = test_counts),
                aes(x = x, y = y, color = "Actual (Test)"),
                linetype = "solid",
                linewidth = 1,
                alpha = .8,
                na.rm = TRUE) +
      geom_line(data = data.frame(x = date_test, y = predictions),
                aes(x = x, y = y, color = "Predicted"),
                linetype = "dotdash" ,
                linewidth = 1,
                alpha = .8,
                na.rm = TRUE) +
      labs(x = "Date",
           y = "Observation Count",
           colour = "Legend",
           title = paste("Python Time Series Analysis for", target_species)) +
      scale_color_manual(values = c("Actual (Train)" = "#C7C7C7",
                                    "Actual (Test)" = "#97D8ED",
                                    "Predicted" = "#4B8FA6")) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")    
    
    # make it a ggplotly for interactivity
    ggplotly(time_series_plot_py)
    
    
    ##########TRYING WITH R INSTEAD     ##########
    
    # Time series data for training and testing
    train <- target_species_month[target_species_month$month_year <= "2021-01-01", "observation_count"]
    test <- target_species_month[target_species_month$month_year > "2021-01-01", "observation_count"]
    
    
    # Convert the train and test data to time series objects
    train_ts <- ts(train, frequency = 12)
    test_ts <- ts(test, frequency = 12)
    
    # Fit the SARIMA model
    model <- auto.arima(train_ts,
                        d = 0,
                        D = 1,
                        max.p = 5,
                        max.q = 5,
                        max.P = 2,
                        max.Q = 2,
                        max.order = 5,
                        max.d = 2,
                        max.D = 2,
                        start.p = 1,
                        start.q = 0,
                        start.P = 0,
                        start.Q = 0)
    
    # Make predictions
    predictions <- forecast(model, h = length(test_ts))
    
    # Extract the point forecasts from the predictions
    predictions_values <- as.vector(predictions$mean)
    
    # Convert predictions to a list
    predictions_list <- as.list(predictions_values)
    
    #### END OF R SARIMA #### 
    
    # Filter the target_species_month data for train and test sets
    train_dates <- target_species_month$month_year[target_species_month$month_year <= as.yearmon("2021-01")]
    test_dates <- target_species_month$month_year[target_species_month$month_year > as.yearmon("2021-01")]
    
    # Filter the observation counts for train and test sets
    train_counts <- target_species_month$observation_count[target_species_month$month_year <= as.yearmon("2021-01")] 
    test_counts <- target_species_month$observation_count[target_species_month$month_year > as.yearmon("2021-01")]

    
    # Calculate and output metrics
   # test_mae <- rae(test_counts, predictions_list)
   #test_mape <- mean_absolute_percentage_error(test_counts, predictions_list)
   # test_smape <- smape(test_counts, predictions_list)
    #test_sse <- sse(test_counts, predictions)
   # test_rmse <- rmse(test_counts, predictions_list)

    
  #  output$test_mae <- renderText(paste0("Test MAE:", round(test_mae, 2)))
    #output$test_mape <- renderText(paste0("Test MAPE:", round(test_mape, 2), "%"))
   # output$test_smape <- renderText(paste0("Test SMAPE:", round(test_smape, 2), "%"))
   # output$test_rmse <- renderText(paste0("Test RMSE:", round(test_rmse, 2)))

    # Round counts for plotly tooltip
    train_counts <- train_counts %>%
      round(0)
    test_counts <- test_counts %>%
      round(0)
    
    # convert to date objects for plotly graph
    date_test <- as.Date(paste(test_dates, "01"), format = "%b %Y %d")          
    date_train <- as.Date(paste(train_dates, "01"), format = "%b %Y %d")  
    
    ######    FOR R ARIMA PLOT    ###### 
    # convert R list to values
    predictions_values <- unlist(predictions_list)
    
    
    # Create the time series plot using ggplot2

    time_series_plot_R <- ggplot() +
      geom_line(data = data.frame(x = date_train, y = train_counts),
                aes(x = x, y = y, color = "Actual (Train)"),
                linetype = "solid",
                linewidth = 1,
                na.rm = TRUE) +
      geom_line(data = data.frame(x = date_test, y = test_counts),
                aes(x = x, y = y, color = "Actual (Test)"),
                linetype = "solid",
                linewidth = 1,
                alpha = .8,
                na.rm = TRUE) +
      geom_line(data = data.frame(x = date_test, y = predictions_values),
                aes(x = x, y = y, color = "Predicted"),
                linetype = "dotdash" ,
                linewidth = 1,
                alpha = .8,
                na.rm = TRUE) +
      labs(x = "Date",
           y = "Observation Count",
           colour = "Legend",
           title = paste("Time Series Analysis for", target_species)) +
      scale_color_manual(values = c("Actual (Train)" = "#C7C7C7",
                                    "Actual (Test)" = "#97D8ED",
                                    "Predicted" = "#4B8FA6")) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")    
    
    # make it a ggplotly for interactivity
      ggplotly(time_series_plot_R)
    

    
    # Render the plotly plot
    output$time_series_plot_R <- renderPlotly(time_series_plot_R)
    output$time_series_plot_py <- renderPlotly(time_series_plot_py)
    
        
    # Render the bird image from a local file
    output$bird_image <- renderImage({
      target_species <- input$species
      file_name <- gsub(" ", "_", target_species)
      file_path <- file.path("bird_images/", paste0(file_name, ".jpeg"))
      
      list(src = file_path,
           width = "100%",
           height = "auto")
   
    
    }, deleteFile = F)
  
    
})
}



# Create Shiny app
shinyApp(ui, server)
