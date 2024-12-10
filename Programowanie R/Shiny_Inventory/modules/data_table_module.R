library(shiny)
library(DT)

dataTableUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Data Table"),
    tags$br(),
    tags$p("The Data Table serves as a quick and efficient tool for filtering and exploring data. In my experience, I often find myself deciphering which infrastructure component a server belongs to or vice versa. This table provides a straightforward solution, offering a search box for each data point, making it easier to locate specific information."),
    tags$p("For this specific table, you can search by the following attributes:"),
    tags$ul(
      tags$li(tags$b("Country"), " - UK, US, HK, PL, CN"), 
      tags$li(tags$b("Hostname")), 
      tags$li(tags$b("Network Zone Type")," - DMZ, LAN, WAN"),
      tags$li(tags$b("IP Address")),
      tags$li(tags$b("Data Centre Location")," -  London DC, New York DC, Hong Kong DC, Warsaw DC, Beijing DC"),
      tags$li(tags$b("IT Service")," -  Email, Database, Web Hosting, File Storage, Monitoring"),
      tags$li(tags$b("IT Service Owner")," -  Elton John, David Bowie, Adele, Amy Winehouse, John Lennon"),
      tags$li(tags$b("RHEL Version")," -  RHEL 7, RHEL 8, RHEL 9"),
      tags$li(tags$b("Network Environment")," -  Prod, Dev, Test, UAT"),
      tags$li(tags$b("Cost")," -  For data cleaning and reshaping"),
      tags$li(tags$b("Infrastructure Component")," -  Mule PCE, Mule GW, Kong GW, rProxy, Hazelcast, MongoDB"),
      tags$li(tags$b("Operational Status")," -  Active, Planned, Decommissioning"),
    ),
    tags$p("This table not only simplifies data lookup but also enhances efficiency by enabling targeted searches across key data points. User can download .csv file with the selected data."),
    tags$br(),
    dataTableOutput(ns("data_table")),
    downloadButton(ns("download_data"), "Download CSV", class = "btn btn-primary")  # Download button at the bottom
  )
}

dataTableServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$data_table <- renderDataTable({
      datatable(
        data,
        options = list(
          dom = 'lrtip',  # Keeps the length menu, search box, and pagination
          pageLength = 30,  # Default number of rows per page
          autoWidth = TRUE,
          searchHighlight = TRUE,  # Highlights search matches
          columnDefs = list(
            list(orderable = FALSE, targets = "_all", className = "dt-center")
          )
        ),
        filter = "top"  # Adds individual column search boxes
      )
    })
    
    # Download handler for the CSV file
    output$download_data <- downloadHandler(
      filename = function() {
        paste("data_table", Sys.Date(), ".csv", sep = "_")  # File name with current date
      },
      content = function(file) {
        write.csv(data, file, row.names = FALSE)  # Save the data as a CSV file
      }
    )
  })
}

