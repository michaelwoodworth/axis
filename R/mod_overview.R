# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/mod_overview.R  — Overview tab (stub)
# ─────────────────────────────────────────────────────────────────────────────

overviewUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fillable(
    padding = 24,
    bslib::layout_columns(
      col_widths = 12,
      placeholder_card("Overview")
    )
  )
}

overviewServer <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    # placeholder — implementation: Prompt D (inventory/summary rollup)
  })
}
