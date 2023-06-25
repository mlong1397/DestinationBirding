library(shiny)
library(dplyr)
library(leaflet)
library(htmltools)
library(DescTools)
library(utils)
library(cluster)
library(dbscan)


# Load and preprocess your bird observation data
#bird_data <- read.csv("data/ebird_7_reduced.csv")
bird_data <- read.csv("data/knox_birds_reduced.csv")

bird_data$observation_date <- as.Date(bird_data$observation_date)
bird_data$month <- format(bird_data$observation_date, "%m")

# remove unrealistic species (do not naturally occur in Knoxville)
bird_data <- bird_data %>%
  filter(!common_name %in% c("Mandarin Duck", "Black-headed Parrot", "Indian Peafowl", 
                             "Helmeted Guineafowl", "Nanday Parakeet", "Ring-necked Pheasant", "Red Junglefowl"))


# duplicate to preserve bird_data for the when where bar chart
map_bird_data <- bird_data %>%
  mutate(observation_count = replace_na(observation_count, 1))
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
  #tsa_bird_data <- read.csv("data/grouped_month.csv")
  tsa_bird_data <- read.csv("data/daily_counts.csv")
  tsa_bird_data$observation_date <- as.Date(tsa_bird_data$observation_date)
  tsa_bird_data$month_year <- format(tsa_bird_data$observation_date, "%Y-%m")
  
    observeEvent(input$species, {
      
      target_species <- input$species
      
      
      ##################    INITIAL FILTERING AND DATA MANIPULATION    ##################    
      
      # Filter the bird observation data for the target species
      target_species_df <- tsa_bird_data %>%
        filter(common_name == target_species)
      
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
      
      # Extend the target_species_month dataframe with future months and set observation_count to NA
      target_species_month <- target_species_month %>%
        complete(month_year = seq(min(target_species_month$month_year), 
                                  as.yearmon("2023-12"), by = 1/12),
                 fill = list(observation_count = NA))
      
      
      ##################    R AUTO ARIMA    ##################    
      
      # Time series data for training and testing
      train <- target_species_month[target_species_month$month_year < "2022-01-01", "observation_count"]
      test <- target_species_month[target_species_month$month_year >= "2022-01-01", "observation_count"]
      
      
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
                          start.Q = 0,
                          ic = "aic",
                          )
      
      # Make predictions
      predictions <- forecast(model, h = length(test_ts))
      
      # Extract the point forecasts from the predictions
      predictions_values <- as.vector(predictions$mean)
      
      # Set negative predictions to 0
      predictions_values <- pmax(predictions_values, 0)
      
      # View model summary
      output$modelSummary <- renderPrint(print(summary(model)))

      # Filter the target_species_month data for train and test sets
      train_dates <- target_species_month$month_year[target_species_month$month_year < as.yearmon("2022-01")]
      test_dates <- target_species_month$month_year[target_species_month$month_year >= as.yearmon("2022-01")]

      # Filter the observation counts for train and test sets
      train_counts <- target_species_month$observation_count[target_species_month$month_year < as.yearmon("2022-01")]
      test_counts <- target_species_month$observation_count[target_species_month$month_year >= as.yearmon("2022-01")]
      
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
      
      
      # Create the plot
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
               showlegend = TRUE,
               margin = list(t = 80)) %>%
        rangeslider()
      
      
      # Render the plotly plot
      output$time_series_plot_R <- renderPlotly(time_series_plot)
    
      
      
      ################################################################
      ##################    WHEN AND WHERE MAP      ##################    
      ################################################################

      #############     MAP DATA      #############
      
      filtered_map_data <- reactive({
        map_bird_data %>%
          filter(common_name == input$species_name) %>%
          filter(month %in% input$species_month)
      })
      
      #############     CLUSTERING     #############
      
      observeEvent(input$perform_clustering, {
        perform_clustering <- input$perform_clustering
        filtered_data <- filtered_map_data()
        
        if (perform_clustering) {
          if (nrow(filtered_data) == 0) {
            # No valid data for clustering
            cat("No valid data for clustering.\n")
          } else {
            
            # Define the range of eps and minPts values to explore
            eps_range <- seq(0.0001, 0.01, by = 0.0005)
            minPts_range <- seq(2, 30, by = 1)
            
            # Variables to store the optimal parameters and number of clusters
            optimal_eps <- 0
            optimal_minPts <- 0
            optimal_num_clusters <- Inf  # Initialize with a value outside the desired range

            # Flag variable to track if the notification has been shown
            notification_shown <- FALSE
            
            
            # Iterate through different parameter combinations
            for (eps in eps_range) {
              for (minPts in minPts_range) {
                # Perform DBSCAN clustering
                dbscan_result <- dbscan(filtered_data[, c("longitude", "latitude")], eps = eps, minPts = minPts)
                
                # Get the number of clusters
                num_clusters <- max(dbscan_result$cluster)
                
                # Print the number of clusters
                cat("Eps:", eps, "MinPts:", minPts, "Num Clusters:", num_clusters, "\n")

                # Update the optimal parameters if a better number of clusters is found within the desired range
                if (num_clusters >= 3 && num_clusters <= 50) {
                  if (optimal_num_clusters == 0 || (num_clusters < optimal_num_clusters && num_clusters >= 3)) {
                    optimal_eps <- eps
                    optimal_minPts <- minPts
                    optimal_num_clusters <- num_clusters
                 
                  }
                }
              }  
            }  
            
            print(optimal_eps) 
            print(optimal_minPts)
            print(optimal_num_clusters)
            
            # Check if there are too few points to cluster appropriately
            if (optimal_num_clusters == 0 || is.infinite(optimal_num_clusters)) {
              notification_shown <- TRUE
              showNotification(
                tags$p(
                  style = "font-weight:bold; color:red;",
                  "Too few points to cluster appropriately."
                )
              )
            }
            
            # Perform DBSCAN clustering
            dbscan_result <- dbscan(filtered_data[, c("longitude", "latitude")], eps = optimal_eps, minPts = optimal_minPts)
            
            # Add cluster ID to the original data
            filtered_data$cluster_id <- dbscan_result$cluster
            
            # Aggregate data by cluster and common name, excluding cluster_id = 0 which is noise
            clustered_data <- filtered_data %>%
              filter(cluster_id != 0) %>%
              add_count(cluster_id, locality) %>%
              group_by(cluster_id) %>%
              mutate(Majority = locality[n == max(n)][1]) %>%
              select(-n) %>%
              summarise(
                total_counts = sum(observation_count),
                latitude = mean(latitude),
                longitude = mean(longitude),
                locality = Majority
              ) %>%
              ungroup()
            
            # Render the clustered map
            output$map <- renderLeaflet({
              if (nrow(clustered_data) == 0) {
                # No data to display on the map
                leaflet() %>%
                  addTiles() %>%
                  setView(lng = -83.9207, lat = 35.9606, zoom = 10)  # Set the center and zoom level for Knoxville, TN
              } else {
                # Calculate the maximum observation counts
                max_count <- max(clustered_data$total_counts, na.rm = TRUE)
                
                # Define a color palette
                pal <- colorNumeric(
                  palette = "Blues",
                  domain = clustered_data$total_counts
                )
                
                # Define the scaling factor for bubble sizes
                scaling_factor <- 10 / max_count
                
                # Prepare the text for tooltips
                mytext <- paste(
                  "Species: ", input$species_name, "<br/>",
                  "Location: ", clustered_data$locality, "<br/>",
                  "Number of Observations: ", clustered_data$total_counts, "<br/>",
                  sep = ""
                ) %>%
                  lapply(htmltools::HTML)
                
                leaflet(clustered_data) %>%
                  addTiles() %>%
                  addCircleMarkers(
                    lng = ~longitude,
                    lat = ~latitude,
                    weight = 1,
                    fillColor = ~pal(total_counts),
                    color = "black",
                    stroke = TRUE,
                    radius = ~total_counts * scaling_factor,
                    fillOpacity = 0.7,
                    label = mytext
                  )
              }
            })
          }
        } else {
          # Clustering is unchecked, render the non-clustered map
          
          # Filter and aggregate data
          filtered_data <- filtered_map_data() %>%
            group_by(latitude, longitude, locality) %>%
            summarise(total_counts = sum(observation_count, na.rm = TRUE))
          
          # Render the non-clustered map
          output$map <- renderLeaflet({
            if (nrow(filtered_data) == 0) {
              # No data to display on the map
              leaflet() %>%
                addTiles() %>%
                setView(lng = -83.9207, lat = 35.9606, zoom = 10)  # Set the center and zoom level for Knoxville, TN
            } else {
              # Calculate the maximum observation counts
              max_count <- max(filtered_data$total_counts, na.rm = TRUE)
              
              # Define a color palette
              pal <- colorNumeric(
                palette = "Blues",
                domain = filtered_data$total_counts
              )
              
              # Define the scaling factor for bubble sizes
              scaling_factor <- 10 / max_count
              
              # Prepare the text for tooltips
              mytext <- paste(
                "Species: ", input$species_name, "<br/>",
                "Location: ", filtered_data$locality, "<br/>",
                "Number of Observations: ", filtered_data$total_counts, "<br/>",
                sep = ""
              ) %>%
                lapply(htmltools::HTML)
              
              leaflet(filtered_data) %>%
                addTiles() %>%
                addCircleMarkers(
                  lng = ~longitude,
                  lat = ~latitude,
                  weight = 1,
                  fillColor = ~pal(total_counts),
                  color = "black",
                  stroke = TRUE,
                  radius = ~total_counts * scaling_factor,
                  fillOpacity = 0.7,
                  label = mytext
                )
            }
          })
        }
      })
      
      # Update the non-clustered map when the month selection changes
      observeEvent(input$species_month, {
        if (!input$perform_clustering) {
          output$map <- renderLeaflet({
            filtered_data <- filtered_map_data() %>%
              group_by(latitude, longitude, locality) %>%
              summarise(total_counts = sum(observation_count, na.rm = TRUE))
            
            if (nrow(filtered_data) == 0) {
              # No data to display on the map
              leaflet() %>%
                addTiles() %>%
                setView(lng = -83.9207, lat = 35.9606, zoom = 10)  # Set the center and zoom level for Knoxville, TN
            } else {
              # Calculate the maximum observation counts
              max_count <- max(filtered_data$total_counts, na.rm = TRUE)
              
              # Define a color palette
              pal <- colorNumeric(
                palette = "Blues",
                domain = filtered_data$total_counts
              )
              
              # Define the scaling factor for bubble sizes
              scaling_factor <- 10 / max_count
              
              # Prepare the text for tooltips
              mytext <- paste(
                "Species: ", input$species_name, "<br/>",
                "Location: ", filtered_data$locality, "<br/>",
                "Number of Observations: ", filtered_data$total_counts, "<br/>",
                sep = ""
              ) %>%
                lapply(htmltools::HTML)
              
              leaflet(filtered_data) %>%
                addTiles() %>%
                addCircleMarkers(
                  lng = ~longitude,
                  lat = ~latitude,
                  weight = 1,
                  fillColor = ~pal(total_counts),
                  color = "black",
                  stroke = TRUE,
                  radius = ~total_counts * scaling_factor,
                  fillOpacity = 0.7,
                  label = mytext
                )
            }
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
                marker = list(color = "#3c8dbc")) %>%
          layout(xaxis = list(title = "Month"), 
                 yaxis = list(title = "Observations"), 
                 title = "Total Number of Observations",
                 margin = list(t = 80))
      })
      
      ################################################################
      ##################        BIRD PHOTOS         ##################    
      ################################################################

      
      ##################    TSA BIRD PHOTOS    ##################
      
      observeEvent(input$tabs, {
        if (input$tabs == "target_tsa") {
          target_species <- input$species
          
          file_name <- target_species
          file_path <- file.path("www/bird_images", paste0(file_name, ".jpeg"))
          print(file_path)
          
          output$bird_image <- renderImage({
            list(src = file_path,
                 width = "100%",
                 height = "auto")
          }, deleteFile = FALSE)
        } 
        # so that the default loads without initially choosing
          else if (input$tabs == "target_map") {
          target_species <- input$species_name

          file_name <- gsub("_", " ", target_species)
          file_path <- file.path("www/bird_images", paste0(file_name, ".jpeg"))
          print(file_path)
          output$bird_image_ww <- renderImage({
            list(src = file_path,
                 width = "100%",
                 height = "auto")
          }, deleteFile = FALSE)
        }
      })
      
      ##################    WHEN WHERE BIRD PHOTOS    ##################
      
      observeEvent(input$species_name, {
        if (input$tabs == "target_map") {
          target_species <- input$species_name
          
         file_name <- target_species
          file_path <- file.path("www/bird_images", paste0(file_name, ".jpeg"))
          print(file_path)
          output$bird_image_ww <- renderImage({
            list(src = file_path,
                 width = "100%",
                 height = "auto")
          }, deleteFile = FALSE)
        }
      })
      
      ##################    SELECTED BIRD PHOTO    ##################
      
      output$sel_bird_image <- renderUI({
        if (input$tabs == "target_tsa") {
          imageOutput("bird_image")
        } else if (input$tabs == "target_map") {
          imageOutput("bird_image_ww")
        } else {
          NULL
        }
      })
      
      
    })
  
}

