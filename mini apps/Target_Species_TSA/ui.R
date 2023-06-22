

library(shiny)

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
      textOutput("test_mape"),
      textOutput("test_mae"),
      textOutput("test_smape"),
      textOutput("test_rmse")
    )
  ) 
)
