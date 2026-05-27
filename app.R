# ─────────────────────────────────────────────────────────────────────────────
# AXIS · app.R — entry point
# Requires: shiny, bslib (≥ 0.7), readxl, fuzzyjoin, duckdb, DBI,
#           dplyr, purrr, lubridate, readr, shinybusy, uuid, stringdist
# ─────────────────────────────────────────────────────────────────────────────

library(shiny)
library(bslib)

# ── Package checks (install if missing) ──────────────────────────────────────
.need <- c("readxl", "fuzzyjoin", "duckdb", "DBI", "dplyr", "purrr",
           "lubridate", "readr", "shinybusy", "uuid", "stringdist",
           "tibble", "tools", "DT", "echarts4r", "leaflet", "tidyr")
.missing <- .need[!sapply(.need, requireNamespace, quietly = TRUE)]
if (length(.missing) > 0) {
  message("Installing missing packages: ", paste(.missing, collapse = ", "))
  install.packages(.missing, quiet = TRUE)
}
invisible(lapply(.need, library, character.only = TRUE))

# ── Data directory configuration ─────────────────────────────────────────────
# AXIS_OS_DIR  — path to the OpenSpecimen CSV exports.
# Defaults to ../01.data/01.openspecimen_exports/ relative to this app.
# Override by setting the environment variable AXIS_OS_DIR before launch,
# or via options(axis.os_data_dir = "/your/path") in .Rprofile.
AXIS_OS_DIR <- Sys.getenv(
  "AXIS_OS_DIR",
  unset = normalizePath(
    file.path("..", "01.data", "01.openspecimen_exports"),
    mustWork = FALSE
  )
)
if (!dir.exists(AXIS_OS_DIR)) {
  message("Note: AXIS_OS_DIR not found at ", AXIS_OS_DIR,
          "\nSet env var AXIS_OS_DIR or options(axis.os_data_dir=...) to fix.")
}
# Publish as a global option so module servers can find it regardless of
# which environment Shiny uses to source app.R.
options(axis.os_data_dir = AXIS_OS_DIR)

# Ensure the DuckDB data directory exists
dir.create("data", showWarnings = FALSE)

# Make inst/assets/ accessible to the browser as /assets/
shiny::addResourcePath("assets", "inst/assets")

# ── Source data layer ─────────────────────────────────────────────────────────
source("R/store.R")              # open_db(), write_links(), etc.
source("R/data_parse_labid.R")   # parse_lab_ids(), extract_mdro_target(), rules table
source("R/data_parse_vitek.R")   # parse_vitek_files(), parse_one_vitek(), etc.
source("R/data_parse_os.R")      # scan_os_projects(), parse_os_specimens*(), specimens_empty()
source("R/data_dedup.R")         # dedup_vitek(), dedup_summary()
source("R/data_match.R")         # auto_match(), bucket_results(), etc.
source("R/data_clean.R")         # build_cleaned()
source("R/mdro_categories.R")    # axis_interpret_mdro_categories()

# ── Source UI / modules ───────────────────────────────────────────────────────
source("R/theme.R")              # AXIS_THEME, MDRO_COLORS
source("R/ui_helpers.R")         # mdro_badge(), filter_pill(), kpi_tile()
source("R/mod_overview.R")       # overviewUI()  / overviewServer()
source("R/mod_inventory.R")      # inventoryUI() / inventoryServer()
source("R/mod_ingestion.R")      # ingestionUI() / ingestionServer()
source("R/mod_linking.R")        # linkingUI()   / linkingServer()
source("R/mod_specimens.R")      # specimensUI() / specimensServer()
source("R/mod_settings.R")       # settingsUI()  / settingsServer()

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- bslib::page_navbar(
  title = tags$span(
    tags$span(
      "A",
      style = paste(
        "display:inline-grid; place-items:center;",
        "width:26px; height:26px; border-radius:5px;",
        "background:#1f3a5f; color:#fff; font-weight:700;",
        "font-family:'IBM Plex Serif',Georgia,serif;",
        "font-size:14px; margin-right:8px; vertical-align:middle;"
      )
    ),
    tags$span("AXIS", style = "font-weight:600; font-size:15px; letter-spacing:0.2px;"),
    tags$span(
      "v0.4 · internal",
      style = "font-size:11px; color:#6b7280; margin-left:6px; vertical-align:middle;"
    )
  ),
  theme = AXIS_THEME,
  bg    = "#ffffff",     # navbar bg distinct from page bg
  id    = "main_nav",
  # fillable=TRUE so page_navbar grows to viewport height;
  # each panel controls its own internal scroll
  fillable = TRUE,

  # ── Nav panels ──────────────────────────────────────────────────────────────
  bslib::nav_panel("Ingestion",  ingestionUI("ingestion"), fillable = TRUE),
  bslib::nav_panel("Linking",    linkingUI("linking"),     fillable = FALSE),
  bslib::nav_panel("Inventory",  inventoryUI("inventory"), fillable = FALSE),
  # bslib::nav_panel("Overview",   overviewUI("overview"),  fillable = FALSE),
  # bslib::nav_panel("Specimens",  specimensUI("specimens"), fillable = FALSE),

  bslib::nav_spacer(),

  # OpenSpecimen connection pill (cosmetic for now)
  bslib::nav_item(
    tags$span(
      tags$span(
        style = paste(
          "display:inline-block; width:8px; height:8px;",
          "border-radius:50%; background:#15803d; margin-right:5px;"
        )
      ),
      "OpenSpecimen · connected",
      style = paste(
        "font-size:12px; color:#3b4252;",
        "background:#e7f3eb; border:1px solid #bbdfc6;",
        "border-radius:20px; padding:3px 10px;"
      )
    )
  ),

  bslib::nav_panel("Settings",   settingsUI("settings")),

  # ── Additional head tags ─────────────────────────────────────────────────────
  header = tags$head(
    tags$link(rel = "stylesheet", href = "assets/axis.css"),
    # Custom message handlers for Linking module tab badges and class toggling
    tags$script(HTML("
      Shiny.addCustomMessageHandler('axis_update_badge', function(msg) {
        var el = document.getElementById(msg.id);
        if (el) el.textContent = msg.text;
      });
      Shiny.addCustomMessageHandler('axis_set_class', function(msg) {
        var el = document.getElementById(msg.id);
        if (el) el.className = msg.cls;
      });
    ")),
    # IBM Plex fonts (fallback — bslib already loads via font_google() in theme)
    tags$link(
      rel  = "stylesheet",
      href = paste0(
        "https://fonts.googleapis.com/css2?",
        "family=IBM+Plex+Sans:wght@400;500;600;700&",
        "family=IBM+Plex+Serif:wght@500;600&",
        "family=IBM+Plex+Mono:wght@400;500&display=swap"
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Shared reactive state — flows from Ingestion → Linking → Inventory
  app_state <- reactiveValues(
    vitek_raw          = NULL,
    vitek_ast          = NULL,
    vitek_unique       = NULL,
    specimens          = NULL,
    match_candidates   = NULL,
    match_buckets      = NULL,
    links_confirmed    = NULL,
    cleaned_links      = NULL,
    cleaned_ast        = NULL,
    cleaned_overrides  = NULL,
    edit_log           = NULL,
    db_conn            = NULL   # DuckDB connection, opened by ingestionServer
  )

  # ── Module servers ──────────────────────────────────────────────────────────
  overviewServer("overview",   app_state = app_state)
  inventoryServer("inventory", app_state = app_state)
  ingestionServer("ingestion", app_state = app_state)
  linkingServer("linking",     app_state = app_state)
  specimensServer("specimens", app_state = app_state)
  settingsServer("settings",   app_state = app_state)
}

# ── Run ───────────────────────────────────────────────────────────────────────
shiny::shinyApp(ui = ui, server = server)
