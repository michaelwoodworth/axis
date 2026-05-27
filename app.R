source("R/import_vitek.R")
source("R/import_openspecimen.R")
source("R/link_results.R")
source("R/mdro_categories.R")
source("R/summarize_inventory.R")

if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("The shiny package is required. Install it with install.packages('shiny').", call. = FALSE)
}

ui <- shiny::fluidPage(
  shiny::titlePanel("AXIS"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput("vitek_file", "Synthetic VITEK2 CSV", accept = ".csv"),
      shiny::fileInput("openspecimen_file", "Synthetic OpenSpecimen CSV", accept = ".csv"),
      shiny::helpText("Use synthetic CSV files only. Example files are in tests/fixtures.")
    ),
    shiny::mainPanel(
      shiny::h3("Inventory Summary"),
      shiny::tableOutput("summary"),
      shiny::h3("Linked Results"),
      shiny::tableOutput("links")
    )
  )
)

server <- function(input, output, session) {
  vitek <- shiny::reactive({
    path <- input$vitek_file$datapath
    if (is.null(path)) {
      path <- file.path("tests", "fixtures", "synthetic_vitek2.csv")
    }
    import_vitek(path)
  })

  specimens <- shiny::reactive({
    path <- input$openspecimen_file$datapath
    if (is.null(path)) {
      path <- file.path("tests", "fixtures", "synthetic_openspecimen.csv")
    }
    import_openspecimen(path)
  })

  linked <- shiny::reactive({
    link_results(vitek(), specimens())
  })

  output$summary <- shiny::renderTable({
    summarize_inventory(linked())
  })

  output$links <- shiny::renderTable({
    linked()
  })
}

shiny::shinyApp(ui = ui, server = server)
