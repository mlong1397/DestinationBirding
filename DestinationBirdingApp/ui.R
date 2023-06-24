##################################
# Destination Birding            #
# by Marilyn Long                #
# ui.R file                      #
##################################

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(tidyr)
library(zoo)
library(thematic)
library(scales)
library(Metrics)
library(forecast)
library(htmltools)
library(leaflet)
library(shinydashboard)
library(shinycssloaders)
#library(DT)
library(collapsibleTree)
library(shinyWidgets)
library(dbscan)

# data for species diveristy map
bird_data <- read.csv("data/ebird_7_reduced.csv")
bird_data$observation_date <- as.Date(bird_data$observation_date)
bird_data$month <- format(bird_data$observation_date, "%m")

# duplicate to preserve bird_data for the when where bar chart and map
map_bird_data <- bird_data %>%
  mutate(observation_count = replace_na(observation_count, 1))

# data for TSA
tsa_bird_data <- read.csv("data/grouped_month.csv")
tsa_bird_data$observation_date <- as.Date(tsa_bird_data$observation_date)
tsa_bird_data$month_year <- format(tsa_bird_data$observation_date, "%Y-%m")

###########
# LOAD UI #
###########

shinyUI(fluidPage(
  
  ######################     OUTPUT SIZING     ###################### 
  
  tags$head(
    tags$style(
      HTML("
      .sidebar-logo {
        display: block;
        margin: 20px auto;
        width: 150px;
        height: auto;
      }
      #diversity_map {
      width: 100%;
      height: auto;
      }
      #time_series_plot_R {
      width: 100%;
      height: auto;
      }
      #map {
      width: 100%;
      height: auto;
      }
      .sidebar-footer {
          position: absolute;
          bottom: 0;
          left: 0;
          width: 100%;
          text-align: center;
          padding: 10px;
      }
      .sidebar-information {
        font-size: 14px;
        font-weight: bold;
        text-align: center;
        padding-top: 10px;
        position: absolute;
        bottom: 80px;
        width: 100%;
      }
        .sidebar-links {
          position: absolute;
          bottom: 30px;
          left: 0;
          width: 100%;
          padding: 10px;
          text-align: center;
        }
    ")
    )
  ),
  # load page layout
  dashboardPage(
    
    skin = "blue",
    
    ######################     TITLE     ###################### 
    
    dashboardHeader(title="Destination Birding", titleWidth = 300),
    
    ######################     SIDEBAR     ###################### 
    
    dashboardSidebar(width = 300,
                     sidebarMenu(
                       
                       tags$img(src = "bird-logo-w.png", class = "sidebar-logo")
                       ,
                       menuItem("Home", tabName = "home", icon = icon("home")),
                       menuItem("Species Diversity Map", tabName = "diversity_map", icon = icon("map")),
                       menuItem("Time Series Analysis", tabName = "target_tsa", icon = icon("chart-line")),
                       menuItem("When and Where to Bird", tabName = "target_map", icon = icon("map-location")),
                       menuItem("Releases", tabName = "releases", icon = icon("tasks")),
                       HTML("<div class='sidebar-information'>Information and Contact</div>"),
                       HTML(paste0(
                         "<div class='sidebar-links'>",
                         "<table style='margin-left:auto; margin-right:auto;'>",
                         "<tr>",
                         "<td style='padding: 5px;'><a href='https://ebird.org/home' target='_blank'><img src='ebird-png.webp' style='width: 30px; height: auto;'></a></td>",
                         "<td style='padding: 5px;'><a href='https://merlin.allaboutbirds.org/' target='_blank'><img src='merlin.png' style='width: 24px; height: auto;'></a></td>",
                         "<td style='padding: 5px;'><a href='https://tnstateparks.com/parks/seven-islands' target='_blank'><img src='tnsp.png' style='width: 24px; height: auto;'></a></td>",
                         "<td style='padding: 5px;'><a href='https://www.ijams.org/' target='_blank'><img src='ijams.png' style='width: 24px; height: auto;'></a></td>",
                         "<td style='padding: 5px;'><a href='https://www.linkedin.com/in/marilyn-long/' target='_blank'><img src='linkedin.png' style='width: 24px; height: auto;'></a></td>",
                         "</tr>",
                         "</table>",
                         "</div>",
                         "<br>"
                       )),
                       HTML("<div class='sidebar-footer'><small>&copy; Marilyn Long - 2023</small></div>")
                     )
    ), # end dashboardSidebar


    
    ##########################################################
    ######################     BODY     ###################### 
    ##########################################################
    dashboardBody(
      
      tabItems(
        
        #########   HOME   ######### 
        tabItem(tabName = "home",
                
                # home section
                includeMarkdown("www/home.md")
                
        ),
        
        #################################################
        #########     SPECIES DIVERSITY MAP     ######### 
        #################################################
        tabItem(
          tabName = "diversity_map",
          fluidRow(
            column(
              width = 12,
              h4("Instructions"),
              p("Use the dropdown menu to select a month. The map will display the number of 
                bird species seen for the selected month. The data is clustered into 30 locations 
                around Knoxville. If you hover over a point, you will see the name of the 
                locality (ex: Seven Islands State Birding Park) and the number of different 
                species seen."),
              h4("Why use this?"),
              p("Let's say you're visiting Knoxville in June, want to go birding 
                and want to maximize the potential to see the most species. With this app, now you can!"),
              h5("Please note that not all locations may be publicly accessible.")
            )
          ),
          fluidRow(
            title = "Month Selection",
            column(3, selectInput("selected_month", "Select Month", 
                                  choices = unique(bird_data$month)),
                   style="z-index:1002;")
          ),
          fluidRow(
            column(12, leafletOutput("diversity_map") %>% withSpinner(color = "blue"))
          )
        ),
        
        #################################################
        #########     TARGET SPECIES TSA        ######### 
        #################################################
        tabItem(
          tabName = "target_tsa",
          fluidRow(
            column(
              width = 12,
              h4("Instructions"),
              p("Select a bird species from the dropdown menu. The graph displays a Time Series Analysis 
      for the selected bird, using past observations to predict future observations. The graph 
      includes three lines:"),
              p(
                "• Actual (Train): Observed data used for training",
                br(),
                "• Actual (Test): Observed data for the test period",
                br(),
                "• Predicted: Projection into 2023 based on the model"
              ),
              p("Compare the Actual (Test) line with the Predicted line to assess the model's performance 
      and the reliability of the 2023 predictions. If they closely align, it indicates a well-performing 
      model for the selected species, increasing confidence in the projections. However, if there's 
      a significant deviation, exercise caution when interpreting the 2023 predictions.")
            )
          ),
          fluidRow(
            title = "Species Selection",
            column(3, selectInput("species", "Select Bird Species", 
                                  choices = unique(tsa_bird_data$common_name)),
                   style="z-index:1002;")
          ),
          fluidRow(
            column(12, plotlyOutput("time_series_plot_R") %>% withSpinner(color = "blue"))
          )
        ),
        #################################################
        #########         WHEN WHERE MAP        ######### 
        #################################################
        
        tabItem(
          tabName = "target_map",
          fluidRow(
            column(
              width = 12,
              h4("Instructions"),
              p("To explore bird species distribution, select a species from the dropdown 
                menu and choose at least one month. The map will showcase the areas where 
                the selected species is observed during the specified month(s). 
                Additionally, the accompanying graph displays the frequency of species 
                sightings throughout the year. Please note that if a month is absent 
                from the graph, it indicates a lack of observations for that specific 
                month, so if you choose that month the map will be empty."),
              
              p("Clustering is a technique employed to identify and group 
                similar data points based on their shared characteristics. In this 
                application, you may choose whether or not you want to view glustered, or 
                grouped, points. 
                However, please keep in mind that clustering requires a sufficient number of 
                data points to provide meaningful results. If there are too few data points 
                available for the selected species and month combination, you will get a 
                warning message. Please consider selecting different species or 
                months with more available data for clustering analysis."),
              
              p("If there are a large number of data points, the clustering process 
                may take a minute or two to complete. Please be patient during this 
                process and allow the application some time to generate the clusters 
                on the map.")
            )
          ),

          fluidRow(
            column(
              width = 4,
              title = "Species Selection",
              selectInput("species_name", "Select Bird Species", 
                          choices = unique(bird_data$common_name))
            ),
            column(
              width = 4,
              title = "Month Selection",
              pickerInput("species_month", "Select Month", 
                          choices = unique(bird_data$month), 
                          options = list(`actions-box` = TRUE), multiple = TRUE)
            ),
            column(
              width = 4,
              title = "Clustering",
              checkboxInput("perform_clustering", "Perform Clustering", value = FALSE)
            ),

          fluidRow(
            column(12, leafletOutput("map") %>% withSpinner(color = "blue"),
                   style = "margin-top: 20px")
          ),
          fluidRow(
            column(12, plotlyOutput("bar_chart") %>% withSpinner(color = "blue"),
                   style = "margin-top: 20px")
          )
          )
        ),
        
        #################################################
        #########           RELEASES            ######### 
        #################################################
        tabItem(tabName = "releases", includeMarkdown("www/releases.Rmd"))
      )
      
    ) # end dashboardBody
    
  )# end dashboardPage
  ))  
