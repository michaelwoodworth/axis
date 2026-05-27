# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/mod_specimens.R  — Specimens tab (stub)
# Implementation note: this tab surfaces the specimens tibble from
# app_state$specimens with search/filter capabilities.
# ─────────────────────────────────────────────────────────────────────────────

specimensUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fillable(
    padding = 24,
    placeholder_card("Specimens")
  )
}

specimensServer <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    # placeholder — implementation TBD
    # Reads: app_state$specimens (from OpenSpecimen import)
    # Renders DT with search, project filter, status filter
  })
}
