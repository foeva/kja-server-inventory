library(shiny)
library(ggplot2)
library(dplyr)

charts2UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Interactive Server Charts"),
    tags$br(),
    tags$p("Sometimes, we need to quickly slice and analyze data on the go. This is exactly what the Interactive Charts tab offers. It consists of three key elements:"),
    tags$ul(
      tags$li(tags$b("Side Panel:"), " - A selection of filters to refine the data."), 
      tags$li(tags$b("Server Count by Infrastructure Component Chart"), " - Visualizes the distribution of servers across infrastructure components."), 
      tags$li(tags$b("Total Cost by Infrastructure Component Chart:")," -  Highlights the cost associated with each infrastructure component."),
    ),
    tags$p("The data dynamically responds to the selected filters, providing users with an instant, tailored overview of the information they are most interested in. Clear Filter selection available."),
    tags$br(),
    sidebarLayout(
      sidebarPanel(
        width = 2 ,
        h4("Filters"),
        checkboxGroupInput(ns("country_filter"), "Select Country:",
                           choices = NULL, selected = NULL),
        checkboxGroupInput(ns("zone_filter"), "Select Network Zone Type:",
                           choices = NULL, selected = NULL),
        checkboxGroupInput(ns("owner_filter"), "Select IT Service Owner:",
                           choices = NULL, selected = NULL),
        checkboxGroupInput(ns("location_filter"), "Select Data Center Location:",
                           choices = NULL, selected = NULL),
        checkboxGroupInput(ns("service_filter"), "Select IT Service:",
                           choices = NULL, selected = NULL),
        checkboxGroupInput(ns("environment_filter"), "Select Network Environment:",
                           choices = NULL, selected = NULL),
        actionButton(
          ns("clear_filters"), 
          HTML('<i class="bi bi-trash"></i> Clear Filters'),  # Add Bootstrap icon
          class = "btn btn-secondary"
        )
      ),
      mainPanel(
        plotOutput(ns("server_count_chart")),
        plotOutput(ns("server_cost_chart"))
      )
    )
  )
}

charts2Server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize filter choices dynamically from the dataset
    observe({
      updateCheckboxGroupInput(session, "country_filter",
                               choices = unique(data$country),
                               selected = unique(data$country))
      updateCheckboxGroupInput(session, "zone_filter",
                               choices = unique(data$network_zone_type),
                               selected = unique(data$network_zone_type))
      updateCheckboxGroupInput(session, "owner_filter",
                               choices = unique(data$it_service_owner),
                               selected = unique(data$it_service_owner))
      updateCheckboxGroupInput(session, "location_filter",
                               choices = unique(data$data_centre_location),
                               selected = unique(data$data_centre_location))
      updateCheckboxGroupInput(session, "service_filter",
                               choices = unique(data$it_service),
                               selected = unique(data$it_service))
      updateCheckboxGroupInput(session, "environment_filter",
                               choices = unique(data$network_environment),
                               selected = unique(data$network_environment))
    })
    
    # Clear all filters when the button is clicked
    observeEvent(input$clear_filters, {
      updateCheckboxGroupInput(session, "country_filter",
                               selected = unique(data$country))
      updateCheckboxGroupInput(session, "zone_filter",
                               selected = unique(data$network_zone_type))
      updateCheckboxGroupInput(session, "owner_filter",
                               selected = unique(data$it_service_owner))
      updateCheckboxGroupInput(session, "location_filter",
                               selected = unique(data$data_centre_location))
      updateCheckboxGroupInput(session, "service_filter",
                               selected = unique(data$it_service))
      updateCheckboxGroupInput(session, "environment_filter",
                               selected = unique(data$network_environment))
    })
    
    # Reactive dataset filtered by user input
    filtered_data <- reactive({
      data %>%
        filter(
          country %in% input$country_filter,
          network_zone_type %in% input$zone_filter,
          it_service_owner %in% input$owner_filter,
          data_centre_location %in% input$location_filter,
          it_service %in% input$service_filter,
          network_environment %in% input$environment_filter
        )
    })
    
    # Render the server count chart using ggplot2
    output$server_count_chart <- renderPlot({
      server_counts <- filtered_data() %>%
        count(infrastructure_component) %>%
        arrange(desc(n))
      
      ggplot(server_counts, aes(x = n, y = reorder(infrastructure_component, n))) +
        geom_bar(stat = "identity", fill = "steelblue") +
        geom_text(aes(label = n), hjust = -0.2, color = "black", size = 5) +
        labs(
          title = "Server Count by Infrastructure Component",
          x = "Server Count",
          y = "Infrastructure Component"
        ) +
        theme_minimal(base_size = 15) +
        theme(plot.title = element_text(face = "bold"))
    })
    
    # Render the cost chart using ggplot2
    output$server_cost_chart <- renderPlot({
      server_costs <- filtered_data() %>%
        group_by(infrastructure_component) %>%
        summarise(Total_Cost = sum(running_cost_in_usd, na.rm = TRUE)) %>%
        arrange(desc(Total_Cost))
      
      ggplot(server_costs, aes(x = Total_Cost, y = reorder(infrastructure_component, Total_Cost))) +
        geom_bar(stat = "identity", fill = "orange") +
        geom_text(aes(label = scales::comma(Total_Cost, accuracy = 1)), hjust = -0.2, color = "black", size = 5) +
        labs(
          title = "Total Cost by Infrastructure Component",
          x = "Total Cost (USD)",
          y = "Infrastructure Component"
        ) +
        theme_minimal(base_size = 15) +
        theme(plot.title = element_text(face = "bold"))
    })
  })
}
