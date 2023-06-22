library(shiny)
library(dplyr)
library(leaflet)
library(htmltools)
library(DescTools)
library(utils)


# Load and preprocess your bird observation data
bird_data <- read.csv("data/ebird_7_reduced.csv")
bird_data$observation_date <- as.Date(bird_data$observation_date)
bird_data$month <- format(bird_data$observation_date, "%m")
# duplicate to preserve bird_data for the when where bar chart
map_bird_data <- bird_data

server <- function(input, output, session) {
  
  
  ################################################################
  ########################  DIVERSITY MAP  #######################
  ################################################################
  
  filtered_data <- reactive({
    bird_data[bird_data$month == input$selected_month, ]
  })
  
  observe({
    filtered <- filtered_data()
    
    if (!is.null(filtered)) {
      # Perform clustering
      clusters <- kmeans(filtered[, c("latitude", "longitude")], centers = 30)
      
      # Add cluster labels to the filtered dataframe
      filtered$cluster <- clusters$cluster
      
      # Aggregate data by cluster and common name
      agg_data <- filtered %>%
        add_count(cluster, locality) %>%
        group_by(cluster) %>%
        mutate(Majority = locality[n == max(n)][1]) %>%
        select(-n) %>%
        summarise(
          distinct_common_names = n_distinct(common_name),
          latitude = mean(latitude),
          longitude = mean(longitude),
          locality = Majority
        ) %>%
        ungroup()
      
      # Prepare the text for tooltips
      mytext <- paste(
        "Location: ", agg_data$locality, "<br/>",
        "Number of Species: ", agg_data$distinct_common_names, "<br/>",
        sep = ""
      ) %>%
        lapply(htmltools::HTML)
      
      # Define a color palette for the bubbles
      color_palette <- colorNumeric(
        palette = "Blues",
        domain = agg_data$distinct_common_names
      )
      
      # Create the map
      output$diversity_map <- renderLeaflet({
        leaflet(agg_data) %>%
          addTiles() %>%
          setView(
            lng = mean(filtered$longitude),
            lat = mean(filtered$latitude),
            zoom = 10
          ) %>%
          addCircleMarkers(
            lng = ~longitude,
            lat = ~latitude,
            weight = 1,
            color = "black",
            stroke = TRUE,
            fillColor = ~color_palette(distinct_common_names),
            radius = ~distinct_common_names / 10,
            fillOpacity = 0.7,
            label = mytext
          )
      })
    }
  })
  
  ################################################################
  #############################  TSA  ############################
  ################################################################
  
  # Load and preprocess your bird observation data
  tsa_bird_data <- read.csv("data/grouped_month.csv")
  tsa_bird_data$observation_date <- as.Date(tsa_bird_data$observation_date)
  tsa_bird_data$month_year <- format(tsa_bird_data$observation_date, "%Y-%m")
  
    observeEvent(input$species, {
      
      target_species <- input$species
      
      
      ##################    INITIAL FILTERING AND DATA MANIPULATION    ##################    
      target_species_df <- tsa_bird_data %>%
        filter(common_name == target_species)
      
      # Filter the bird observation data for the target species
      #target_species_df <- bird_data[bird_data$common_name == target_species, ]
      
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
      
      
      
      ##################    R AUTO ARIMA    ##################    
      
      # Time series data for training and testing
      train <- target_species_month[target_species_month$month_year < "2021-01-01", "observation_count"]
      test <- target_species_month[target_species_month$month_year >= "2021-01-01", "observation_count"]
      
      
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
    #  predictions_list <- as.list(predictions_values)
      
      # convert R list to values
   #   predictions_values <- unlist(predictions_list)
      
      ##################    SARIMA EVALUATION    ##################    
      
      # Filter the target_species_month data for train and test sets
      train_dates <- target_species_month$month_year[target_species_month$month_year < as.yearmon("2021-01")]
      test_dates <- target_species_month$month_year[target_species_month$month_year >= as.yearmon("2021-01")]
      
      # Filter the observation counts for train and test sets
      train_counts <- target_species_month$observation_count[target_species_month$month_year < as.yearmon("2021-01")] 
      test_counts <- target_species_month$observation_count[target_species_month$month_year >= as.yearmon("2021-01")]
      
      
      # Calculate and output metrics
      test_mae <- rae(test_counts, predictions_values)
      test_mape <- mape(test_counts, predictions_values) * 100
      test_smape <- smape(test_counts, predictions_values)
      test_sse <- sse(test_counts, predictions_values)
      test_rmse <- rmse(test_counts, predictions_values)
      test_mase <- mase(test_counts, predictions_values)
      
      
      output$test_mae <- renderText(paste0("Test MAE:", round(test_mae, 2)))
      output$test_mape <- renderText(paste0("Test MAPE:", round(test_mape, 2), "%"))
      output$test_smape <- renderText(paste0("Test SMAPE:", round(test_smape, 2), "%"))
      output$test_rmse <- renderText(paste0("Test RMSE:", round(test_rmse, 2)))
      output$test_mase <- renderText(paste0("Test MASE:", round(test_mase, 2)))
      
      
      ##################    PLOT THE ARIMA    ##################    
      
      # Round counts for plotly tooltip
      train_counts <- train_counts %>%
        round(0)
      test_counts <- test_counts %>%
        round(0)
      predictions_values <- predictions_values %>%
        round(0)
      
      # convert to date objects for plotly graph
      
      date_test <- as.Date(paste(test_dates, "01"), format = "%b %Y %d")          
      date_train <- as.Date(paste(train_dates, "01"), format = "%b %Y %d")  
      
      
      
      time_series_plot <- plot_ly() %>%
        add_lines(data = data.frame(x = date_train, y = train_counts),
                  x = ~x, y = ~y, color = I("#C7C7C7"),
                  line = list(dash = "solid"),                
                  name = "Actual (Train)") %>%
        add_lines(data = data.frame(x = date_test, y = test_counts),
                  x = ~x, y = ~y, color = I("#97D8ED"),
                  line = list(dash = "solid"),                
                  alpha = 0.8,
                  name = "Actual (Test)") %>%
        add_lines(data = data.frame(x = date_test, y = predictions_values),
                  x = ~x, y = ~y, color = I("#4B8FA6"),
                  line = list(dash = "dashdot"),                
                  alpha = 0.8,
                  name = "Predicted") %>%
        layout(xaxis = list(title = "Date"),
               yaxis = list(title = "Observation Count"),
               title = paste("Time Series Analysis for", target_species),
               showlegend = TRUE) %>%
        rangeslider()
      
      
      # Render the plotly plot
      output$time_series_plot_R <- renderPlotly(time_series_plot)
      
      ##################    BIRD PHOTOS    ##################    
      
      # Render the bird image from a local file
      output$bird_image <- renderImage({
        target_species <- input$species
        file_name <- gsub(" ", "_", target_species)
        file_path <- file.path("bird_images/", paste0(file_name, ".jpeg"))
        
        list(src = file_path,
             width = "100%",
             height = "auto")
        
        
      }, deleteFile = F)
      
      
      ################################################################
      ##################    WHEN AND WHERE MAP      ##################    
      ################################################################
      
      #############     MAP DATA      ############# 
      
      filtered_map_data <- reactive({
        
        
        map_bird_data %>%
          filter(common_name == input$species_name) %>%
          filter(month %in% input$species_month) %>%
          group_by(latitude, longitude, locality) %>%
          summarise(sum_counts = sum(observation_count, na.rm = TRUE)) # sometimes birds are marked only as present which gives a count of NA
        
        
      })
      

      
      #############     MAP OUTPUT      ############# 
      observe({
        filtered_map_data <- filtered_map_data()
        
        if (nrow(filtered_map_data) == 0) {
          output$map <- renderLeaflet({
            leaflet() %>%
              addTiles() %>%
              setView(lng = -83.9207, lat = 35.9606, zoom = 10)  # Set the center and zoom level for Knoxville, TN
          })
        } else {
          output$map <- renderLeaflet({
            # Calculate the minimum and maximum observation counts
            min_count <- min(filtered_map_data$sum_counts, na.rm = TRUE)
            max_count <- max(filtered_map_data$sum_counts, na.rm = TRUE)
            
            # Define a color palette
            pal <- colorNumeric(
              palette = "Blues",
              domain = filtered_map_data$sum_counts
            )        
            
            # Define the scaling factor for bubble sizes
            scaling_factor <- 10 / max_count
            
            # Prepare the text for tooltips:
            mytext <- paste(
              "Species: ", input$species_name, "<br/>",
              "Location: ", filtered_map_data$locality,"<br/>", 
              "Number of Observations: ", filtered_map_data$sum_counts, "<br/>", 
              sep="") %>%
              lapply(htmltools::HTML)
            
            leaflet(filtered_map_data) %>%
              addTiles() %>%
              addCircleMarkers(
                lng = ~longitude,
                lat = ~latitude,
                weight = 1,
                fillColor = ~pal(sum_counts),
                color = "black", 
                stroke = TRUE,
                radius = ~sum_counts * scaling_factor,  # Apply the scaling factor
                fillOpacity = .7,
                label = mytext)
            # ) %>%
            # addLegend(
            #   position = "bottomright",
            #   colors = color_palette(5),  # Adjust the number of colors as desired
            #   labels = seq(ceiling(min_count), floor(max_count), length.out = 5),  # Adjust the number of labels as desired
            #   opacity = 0.6,
            #   title = "Observation Count"
            # )
          })
        }
      })
      
      #############     BAR CHART DATA      ############# 
      filtered_bar_data <- reactive({
        map_bird_data %>%
          filter(common_name == input$species_name) 
      })
      #############     BAR CHART OUTPUT      ############# 
      output$bar_chart <- renderPlotly({
        
        filtered_bar_data <- filtered_bar_data()
        
        # group by month and add up observations
        monthly_counts <- filtered_bar_data %>%
          group_by(month) %>%
          summarise(monthly_sum = round(sum(observation_count, na.rm = TRUE))) %>%
          mutate(Month = factor(month, levels = unique(month)))
        
        # Create the bar chart using Plotly
        plot_ly(monthly_counts, 
                x = ~Month, 
                y = ~monthly_sum, 
                type = "bar", 
                marker = list(color = "purple")) %>%
          layout(xaxis = list(title = "Month"), 
                 yaxis = list(title = "Observations"), 
                 title = "Total Number of Observations")
      })
      
      
    })
  
}

