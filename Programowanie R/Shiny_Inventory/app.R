library(shiny)
library(DT)
library(bslib)
library(shinydashboard)

# Source modules
source("modules/how_to_module.R")
source("modules/data_table_module.R")
source("modules/analysis_module.R")
source("modules/charts_module.R")
source("modules/charts2_module.R")



# Load the dataset
data <- read.csv("server_info_biased_8078_rows.csv")

# Define the UI
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "cerulean"),
  
  # Include Bootstrap Icons
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons/font/bootstrap-icons.css")
  ),
  tags$br(),
  
  titlePanel(
    HTML('<i class="bi bi-server"></i> Infrastructure Inventory App') # Add the Bootstrap icon
  ),
  tags$br(),
  
  navset_card_pill(
    nav_panel("Intro", howToUI("how_to")),
    nav_panel("Data Table", dataTableUI("data_table")),
    nav_panel("Analysis", analysisUI("analysis")),
    nav_panel("Fixed Charts", chartsUI("charts")),
    nav_panel("Interactive Charts", charts2UI("charts2"))
  )
)


# Define the server
server <- function(input, output, session) {
  # Call the modules
  howToServer("how_to")
  dataTableServer("data_table", data)
  analysisServer("analysis", data)
  chartsServer("charts", data)
  charts2Server("charts2", data)
}

# Run the application
shinyApp(ui = ui, server = server)
