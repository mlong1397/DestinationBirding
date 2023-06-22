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
library(DT)
library(collapsibleTree)
library(shinyWidgets)

# data for species diveristy map
bird_data <- read.csv("data/ebird_7_reduced.csv")
bird_data$observation_date <- as.Date(bird_data$observation_date)
bird_data$month <- format(bird_data$observation_date, "%m")

# duplicate to preserve bird_data for the when where bar chart and map
map_bird_data <- bird_data

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
                       HTML(paste0(
                         "<br><br><br><br><br><br><br><br><br>",
                         "<table style='margin-left:auto; margin-right:auto;'>",
                         "<tr>",
                         "<td style='padding: 5px;'><a href='https://ebird.org/home' target='_blank'><img src='ebird-png.webp'style='width: 30px; height: auto;'></a></td>",
                         "<td style='padding: 5px;'><a href='https://merlin.allaboutbirds.org/' target='_blank'><img src='merlin.png'style='width: 24px; height: auto;'></a></td>",
                         "<td style='padding: 5px;'><a href='https://tnstateparks.com/parks/seven-islands' target='_blank'><img src='tnsp.png'style='width: 24px; height: auto;'></a></td>",
                         "<td style='padding: 5px;'><a href='https://www.ijams.org/' target='_blank'><img src='ijams.png'style='width: 24px; height: auto;'></a></td>",
                         "<td style='padding: 5px;'><a href='hhttps://www.linkedin.com/in/marilyn-long/ target='_blank'><img src='linkedin.png'style='width: 24px; height: auto;'></a></td>",
                         "</tr>",
                         "</table>",
                         "<br>"),
                         HTML(paste0(
                           "<script>",
                           "var today = new Date();",
                           "var yyyy = today.getFullYear();",
                           "</script>",
                           "<p style = 'text-align: center;'><small>&copy; Marilyn Long</a> - <script>document.write(yyyy);</script></small></p>")
                         ))
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
                bird species seen for the selected month. The data is clustered into 40 locations 
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
              p("Use the dropdown menu to select a bird species. The graph will show a 
                Time Series Analysis for the selected bird. A time series analysis uses past 
                observations to predict future observations.")
            )
          ),
          fluidRow(
            title = "Species Selection",
            column(3, selectInput("species", "Select Bird Species", 
                                  choices = unique(tsa_bird_data$common_name)),
                   style="z-index:1002;"),
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
              p("Use the dropdown menu to select a bird species and at least one month. 
                The map will show where the species is present during the selected month(s). 
                The graph shows how frequently the species is seen throughout the year. Please note that
                if a month is not present in the graph, it means that no observations of that species was made for that month.
                The same goes for the map.")
            )
          ),
          fluidRow(
            title = "Species Selection",
            column(5, selectInput("species_name", "Select Bird Species", 
                                  choices = unique(bird_data$common_name)),
                   style="z-index:1003;")
            ),
          fluidRow(
            title = "Month Selection",
            column(5, pickerInput("species_month", "Select Month", 
                                  choices = unique(bird_data$month), 
                                  options = list(`actions-box` = TRUE),multiple = T),
                   style="z-index:1002;")
          ),
          fluidRow(
            column(12, leafletOutput("map") %>% withSpinner(color = "blue"),
                   style = "margin-top: 20px")
          ),
          fluidRow(
            column(12, plotlyOutput("bar_chart") %>% withSpinner(color = "blue"),
                   style = "margin-top: 20px")
          )
        ),
        
        ### EXTRA ###
        tabItem(tabName = "releases", includeMarkdown("www/releases.md"))
        
      )
      
    ) # end dashboardBody
    
  )# end dashboardPage
  ))  
