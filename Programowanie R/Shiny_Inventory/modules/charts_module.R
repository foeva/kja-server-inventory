library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

chartsUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Inventory Charts"),
    tags$br(),
    tags$p("A visual snapshot of crucial data points related to infrastructure, providing a clear reference for server updates and changes."),
    tags$br(),
    fluidRow(
      column(6, plotOutput(ns("rhel_bar_chart"), height = "400px")),  # Chart 1: RHEL Version Distribution
      column(6, plotOutput(ns("zone_env_chart"), height = "400px"))  # Chart 2: Servers by Zone Type and Environment
    ),
    tags$br(),
    fluidRow(
      column(6, plotOutput(ns("operational_status_pie"), height = "400px")), # Chart 3: Operational Status Pie Chart
      column(6, plotOutput(ns("cost_heatmap"), height = "400px"))            # Chart 4: Running Cost Heatmap
    )
  )
}

chartsServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Chart 1: RHEL Version Distribution
    output$rhel_bar_chart <- renderPlot({
      rhel_counts <- data %>%
        group_by(rhel_version) %>%
        summarise(Count = n())
      
      ggplot(rhel_counts, aes(x = reorder(rhel_version, -Count), y = Count, fill = rhel_version)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = Count), vjust = 1.5, color = "black", size = 5) +
        labs(
          title = "RHEL Version Distribution",
          x = "RHEL Version",
          y = "Server Count"
        ) +
        theme_minimal(base_size = 15) +
        theme(legend.position = "none", plot.title = element_text(face = "bold"))
    })
    
    # Chart 2: Servers by Zone Type and Environment
    output$zone_env_chart <- renderPlot({
      zone_env_data <- data %>%
        group_by(network_zone_type, network_environment) %>%
        summarise(Count = n()) %>%
        ungroup()
      
      ggplot(zone_env_data, aes(x = network_zone_type, y = Count, fill = network_environment)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(aes(label = Count), position = position_stack(vjust = 0.5), color = "black", size = 4) +
        labs(
          title = "Servers by Zone Type and Environment",
          x = "Network Zone Type",
          y = "Server Count",
          fill = "Environment"
        ) +
        theme_minimal(base_size = 15) +
        theme(plot.title = element_text(face = "bold"))
    })
    
    # Chart 3: Operational Status Pie Chart
    output$operational_status_pie <- renderPlot({
      status_data <- data %>%
        group_by(operational_status) %>%
        summarise(Count = n()) %>%
        ungroup() %>%
        mutate(Percentage = Count / sum(Count) * 100)  # Calculate percentage
      
      ggplot(status_data, aes(x = "", y = Count, fill = operational_status)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        geom_text(aes(
          label = paste0(round(Percentage, 1), "%\n(", Count, ")")
        ), 
        position = position_stack(vjust = 0.5),  # Place labels in the middle of slices
        color = "black", size = 5) +  # Black text, adjust size if needed
        labs(
          title = "Operational Status Distribution",
          fill = "Status"
        ) +
        theme_minimal(base_size = 15) +
        theme(
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          plot.title = element_text(face = "bold")
        )
    })
    
    # Chart 4: Running Cost Heatmap
    output$cost_heatmap <- renderPlot({
      heatmap_data <- data %>%
        group_by(data_centre_location, network_environment) %>%
        summarise(Total_Cost = sum(running_cost_in_usd, na.rm = TRUE)) %>%
        pivot_wider(names_from = network_environment, values_from = Total_Cost, values_fill = 0)
      
      long_heatmap_data <- heatmap_data %>%
        pivot_longer(
          cols = -data_centre_location,
          names_to = "Network_Environment",
          values_to = "Total_Cost"
        )
      
      ggplot(long_heatmap_data, aes(x = Network_Environment, y = data_centre_location, fill = Total_Cost)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "white", high = "red", na.value = "gray90") +
        labs(
          title = "Running Cost Heatmap",
          x = "Network Environment",
          y = "Data Center",
          fill = "Total Cost"
        ) +
        theme_minimal(base_size = 15) +
        theme(
          plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    })
  })
}
