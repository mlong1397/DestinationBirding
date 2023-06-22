library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(shinyWidgets)

# Load and preprocess your bird observation data
bird_data <- read.csv("data/ebird_7_reduced.csv")
bird_data$observation_date <- as.Date(bird_data$observation_date)
bird_data$month <- format(bird_data$observation_date, "%m")
# duplicate to preserve bird_data for the bar chart
map_bird_data <- bird_data
# UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("species_name", "Select Bird Species", choices = unique(bird_data$common_name)),
      pickerInput("species_month", "Select Month", choices = unique(bird_data$month), options = list(`actions-box` = TRUE),multiple = T)
      ),
    mainPanel(
      leafletOutput("map"),
      plotlyOutput("bar_chart")
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    
    
    map_bird_data %>%
      filter(common_name == input$species_name) %>%
      filter(month %in% input$species_month) %>%
      #mutate(rounded_lat = round(latitude,3)) %>%
      #mutate(rounded_lon = round(longitude,3)) %>%
      group_by(latitude, longitude, locality) %>%
      summarise(sum_counts = sum(observation_count))
    
   
  })
  

  #### BIRD SPECIES MAP ####
  observe({
    filtered_map_data <- filtered_data()
    
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
  
  ## BAR CHART DATA
  filtered_bar_data <- reactive({
    bird_data %>%
      filter(common_name == input$species_name) #%>%
     # filter(month %in% input$species_month)
  })
  
  #### BAR CHART ####
  output$bar_chart <- renderPlotly({
    
   filtered_bar_data <- filtered_bar_data()
    
   # filtered_bar_data$observation_count <- as.numeric(filtered_bar_data$observation_count)
    
    # Convert observation_date to Date format
    #filtered_bar_data$observation_date <- as.Date(filtered_bar_data$observation_date)
    
    
   monthly_counts <- filtered_bar_data %>%
     group_by(month) %>%
     summarise(monthly_sum = round(sum(observation_count))) %>%
     mutate(Month = factor(month, levels = unique(month)))  # Convert Month to factor with ordered levels
   
    # Convert Month to factor with ordered levels
   # monthly_counts$Month <- factor(monthly_counts$Month, levels = month.abb)
    
    plot_ly(monthly_counts, 
            x = ~Month, 
            y = ~monthly_sum, 
            type = "bar", 
            marker = list(color = "purple")) %>%
      layout(xaxis = list(title = "Month"), 
             yaxis = list(title = "Average Observation Count"), 
             title = "Average Number of Observations by Month")
    
  })
}


# Create Shiny app
shinyApp(ui, server)