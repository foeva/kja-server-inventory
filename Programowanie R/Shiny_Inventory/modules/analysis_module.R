library(shiny)
library(dplyr)
library(DT)

analysisUI <- function(id) {
  ns <- NS(id)
  tags$div(
    tags$h2("Inventory Analysis"),
    tags$p("Detailed insights into the server inventory, including key metrics and summaries."),
    tags$br(),
    tags$p("A quick snapshot of key static and calculated data points. These values will automatically update whenever the associated CSV file is refreshed or modified."),
    
    # Grid layout for 12 analytics
 
    fluidRow(
      column(4, tags$div(class = "card", tags$h4("RHEL Version with Max Cost"), verbatimTextOutput(ns("rhel_max_cost")))),
      column(4, tags$div(class = "card", tags$h4("Total Running Cost (USD)"), verbatimTextOutput(ns("total_cost")))),
      column(4, tags$div(class = "card", tags$h4("Average Running Cost (USD)"), verbatimTextOutput(ns("avg_cost"))))
    ),
    tags$br(),
    
    fluidRow(
      column(4, tags$div(class = "card", tags$h4("Max Running Cost"), verbatimTextOutput(ns("max_cost")))),
      column(4, tags$div(class = "card", tags$h4("Min Running Cost"), verbatimTextOutput(ns("min_cost")))),
      column(4, tags$div(class = "card", tags$h4("Total Servers"), verbatimTextOutput(ns("total_servers")))),
    ),
    tags$br(),
    fluidRow(
      column(4, tags$div(class = "card", tags$h4("Servers by RHEL Version"), DTOutput(ns("servers_by_rhel")))),
      column(4, tags$div(class = "card", tags$h4("Top 5 Data Centers"), DTOutput(ns("top_data_centers")))),
      column(4, tags$div(class = "card", tags$h4("Servers by Zone Type"), DTOutput(ns("servers_by_zone"))))
    ),
    tags$br(),
    
    fluidRow(
      column(4, tags$div(class = "card", tags$h4("Most Common IT Service"), verbatimTextOutput(ns("common_service")))),
      column(4, tags$div(class = "card", tags$h4("Unique Data Centers"), verbatimTextOutput(ns("unique_dcs")))),
      column(4, tags$div(class = "card", tags$h4("Servers by Environment"), DTOutput(ns("servers_by_env"))))
      
    )
  )
}

analysisServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Total Servers
    output$total_servers <- renderText({
      nrow(data)
    })
    
    # 2. Total Running Cost
    output$total_cost <- renderText({
      formatC(sum(data$running_cost_in_usd, na.rm = TRUE), format = "f", big.mark = ",")
    })
    
    # 3. Average Running Cost
    output$avg_cost <- renderText({
      formatC(mean(data$running_cost_in_usd, na.rm = TRUE), format = "f", big.mark = ",")
    })
    
    # 4. Servers by RHEL Version
    output$servers_by_rhel <- renderDT({
      data %>%
        group_by(rhel_version) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count)) %>%
        datatable(options = list(pageLength = 5, dom = 't', autoWidth = TRUE))
    })
    
    # 5. Top 5 Data Centers
    output$top_data_centers <- renderDT({
      data %>%
        group_by(data_centre_location) %>%
        summarise(Server_Count = n()) %>%
        arrange(desc(Server_Count)) %>%
        head(5) %>%
        datatable(options = list(pageLength = 5, dom = 't', autoWidth = TRUE))
    })
    
    # 6. Servers by Zone Type
    output$servers_by_zone <- renderDT({
      data %>%
        group_by(network_zone_type) %>%
        summarise(Count = n()) %>%
        datatable(options = list(pageLength = 5, dom = 't', autoWidth = TRUE))
    })
    
    # 7. Max Running Cost
    output$max_cost <- renderText({
      formatC(max(data$running_cost_in_usd, na.rm = TRUE), format = "f", big.mark = ",")
    })
    
    # 8. Min Running Cost
    output$min_cost <- renderText({
      formatC(min(data$running_cost_in_usd, na.rm = TRUE), format = "f", big.mark = ",")
    })
    
    # 9. Servers by Environment
    output$servers_by_env <- renderDT({
      data %>%
        group_by(network_environment) %>%
        summarise(Server_Count = n()) %>%
        datatable(options = list(pageLength = 5, dom = 't', autoWidth = TRUE))
    })
    
    # 10. Most Common IT Service
    output$common_service <- renderText({
      data %>%
        group_by(it_service) %>%
        summarise(Count = n()) %>%
        arrange(desc(Count)) %>%
        slice(1) %>%
        pull(it_service)
    })
    
    # 11. Unique Data Centers
    output$unique_dcs <- renderText({
      n_distinct(data$data_centre_location)
    })
    
    # 12. RHEL Version with Max Cost
    output$rhel_max_cost <- renderText({
      data %>%
        group_by(rhel_version) %>%
        summarise(Total_Cost = sum(running_cost_in_usd, na.rm = TRUE)) %>%
        arrange(desc(Total_Cost)) %>%
        slice(1) %>%
        pull(rhel_version)
    })
  })
}
