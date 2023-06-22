library(shiny)
library(dplyr)
library(sf)
library(leaflet)
library(htmltools)
library(DescTools)

# Load and preprocess your bird observation data
bird_data <- read.csv("data/ebird_7_reduced.csv")
bird_data$observation_date <- as.Date(bird_data$observation_date)
#bird_data$month_year <- format(bird_data$observation_date, "%Y-%m")
bird_data$month <- format(bird_data$observation_date, "%m")

# UI
ui <- fluidPage(
  # SELECT A THEME
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  titlePanel("Monthly Bird Species Diveristy Map"),
  sidebarLayout(
    sidebarPanel(
      selectInput("month", "Select Month", choices = unique(bird_data$month))
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  
  # Filter the data based on selected month
  observeEvent(input$month, {
    target_month <- input$month
    
    # # filter the data by month
    # filtered_data <- bird_data %>%
    #   filter(month(observation_date) == target_month)
    
    # filter the data by month
    filtered_data <- bird_data %>%
      filter(month == target_month)
    
    # Perform clustering
    clusters <- kmeans(filtered_data[, c("latitude", "longitude")], centers = 40)
    
    # Add cluster labels to the filtered dataframe
    filtered_data$cluster <- clusters$cluster
    
    
    # Aggregate data by cluster and common name
    agg_data <- filtered_data %>%
      
      # add a column n with counts of the locality names at each cluster (most frequent name)
      add_count(cluster, locality) %>%
      
      group_by(cluster) %>%
      
      # only keep the most frequent locality name per cluster
      mutate(Majority = locality[n == max(n)][1]) %>%
      
      # do not keep temp var
      select(-n) %>%
      
      # summarise by distinct names, lat, lon, and locality

      summarise(distinct_common_names = n_distinct(common_name), # number of distinct species per  cluster
                latitude = mean(latitude),
                longitude = mean(longitude),
                locality = Majority) %>%
      ungroup()
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "Location: ", agg_data$locality,"<br/>", 
      "Number of Species: ", agg_data$distinct_common_names, "<br/>", 
      sep="") %>%
      lapply(htmltools::HTML)
    
    # Define a color palette for the bubbles
    color_palette <- colorNumeric(
      palette = "Blues", 
      domain = agg_data$distinct_common_names
    )
    
    # Create the map
    output$map <- renderLeaflet({
      leaflet(agg_data) %>%
        addTiles() %>%
        setView(lng = mean(filtered_data$longitude), lat = mean(filtered_data$latitude), zoom = 10) %>%
        addCircleMarkers(
          lng = ~longitude, 
          lat = ~latitude,
          weight = 1, 
          color = ~color_palette(distinct_common_names),  # Use the color palette
          radius = ~distinct_common_names / 10,  # Adjust the size of the circles based on count
          fillOpacity = 0.7,
          label = mytext
        )
    })
  })
}

# Run the app
shinyApp(ui, server)
