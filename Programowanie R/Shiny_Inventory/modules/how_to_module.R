howToUI <- function(id) {
  ns <- NS(id)
  tags$div(
    tags$h2("Welcome to the Infrastructure Inventory App"),
    tags$br(),
    tags$p("This project was developed as part of my Data Science Degree, specifically for the Programming in R course. I saw this as an opportunity to tackle a real-life challenge I face at work: managing an infrastructure inventory."),
    tags$p("In many companies, infrastructure inventories are often maintained using tools like UCMDB, which, while functional, tend to be outdated, difficult to use, and lack metadata tailored to the needs of engineering teams managing specific portions of the infrastructure. So, I decided to build my own infrastructure inventory database that’s fit for purpose."),
    tags$h3("About the App"),
    tags$p("The Infrastructure Inventory App is written in R and leverages the following powerful packages:"),
    tags$ul(
      tags$li(tags$b("shiny"), " - For building interactive web apps"), 
      tags$li(tags$b("DT"), " - For rendering interactive data tables"), 
      tags$li(tags$b("bslib")," - For responsive and customizable themes"),
      tags$li(tags$b("shinydashboard")," - For a modular and user-friendly UI"),
      tags$li(tags$b("dplyr")," -  For data manipulation"),
      tags$li(tags$b("ggplot2")," -  For creating static visualizations"),
      tags$li(tags$b("idyr")," -  For data cleaning and reshaping")
    ),
    tags$h3("App Features"),
    tags$p("The application is divided into several functional modules:"),
    tags$ul(
      tags$li(tags$b("Data Table")," - Interactive tables for viewing and managing data"), 
      tags$li(tags$b("Analysis")," - Tools for exploring and analyzing the dataset"), 
      tags$li(tags$b("Charts")," - Static data visualizations for insights at a glance"),
      tags$li(tags$b("Interactive Charts")," - Dynamic charts for deeper exploration"),
    ),
    tags$h3("Learning Journey"),
    tags$p("Working on this project allowed me to apply my learning to a real-world scenario, which has always been the most effective approach for me. It has also provided me with a prototype and reference point for developing a similar app in Python, which I believe is better suited for maintaining and scaling web apps with more complex functionality."),
    tags$h3("Explore the Code"),
    tags$p(
      "The source code for this app is available on my ",
      tags$a(href = "https://github.com/your-username/your-repository", 
             "GitHub", target = "_blank"),
      " — feel free to check it out!"
    )
  )
}

howToServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Logic for the "How to" panel (if any)
  })
}
