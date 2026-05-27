# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/mod_settings.R  — Settings tab (stub)
# Settings to expose (HANDOFF.md §9):
#   - Dedup rule: "latest run wins" | "first run wins" | "manual flag if differ"
#   - Auto-match thresholds (auto ≥ 80, review 50–79 by default)
#   - OpenSpecimen connection (URL, API key)
#   - DuckDB path override
#   - Auth / user identity (shinymanager or proxy)
# ─────────────────────────────────────────────────────────────────────────────

settingsUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fillable(
    padding = 24,
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Dedup rules"),
        bslib::card_body(
          shiny::radioButtons(
            ns("dedup_rule"),
            label   = "When duplicate accession IDs appear across runs:",
            choices = c(
              "Latest run wins (default)" = "latest",
              "First run wins"            = "first",
              "Flag for manual review"    = "manual"
            ),
            selected = "latest"
          )
        )
      ),
      bslib::card(
        bslib::card_header("Match thresholds"),
        bslib::card_body(
          shiny::sliderInput(
            ns("thresh_auto"),
            "Auto-match threshold (score ≥ x → confirmed)",
            min = 50, max = 100, value = 80, step = 5
          ),
          shiny::sliderInput(
            ns("thresh_review"),
            "Needs-review lower bound (score ≥ x → review bucket)",
            min = 20, max = 79, value = 50, step = 5
          )
        )
      )
    ),
    placeholder_card("Settings (OpenSpecimen connection + auth)")
  )
}

settingsServer <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    # Expose reactive thresholds to other modules via app_state (TBD)
    # app_state$thresh_auto   <- reactive(input$thresh_auto)
    # app_state$thresh_review <- reactive(input$thresh_review)
    # app_state$dedup_rule    <- reactive(input$dedup_rule)
  })
}
