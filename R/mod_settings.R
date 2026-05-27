# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/mod_settings.R  — Settings tab
#
# Settings exposed (HANDOFF.md §10 / §11):
#   - Dedup rule: "latest run wins" | "first run wins" | "manual flag if differ"
#   - Auto-match thresholds (auto ≥ 80, review 50–79 by default)
#   - Future: OS connection (URL, API key), DuckDB path, auth identity
#
# All three live settings are persisted onto app_state so that other modules
# (notably ingestionServer) can read the latest user-configured values.
#
# Storage convention — app_state holds *plain values* (not reactives):
#   app_state$thresh_auto   numeric    (default 80)
#   app_state$thresh_review numeric    (default 50)
#   app_state$dedup_rule    character  (default "latest")
# Reader-side: `app_state$thresh_auto %||% 80` — works whether or not the
# Settings tab has been opened yet.
# ─────────────────────────────────────────────────────────────────────────────

# Defaults applied on module init
.SETTINGS_DEFAULTS <- list(
  thresh_auto   = 80,
  thresh_review = 50,
  dedup_rule    = "latest"
)

# ── UI ────────────────────────────────────────────────────────────────────────

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
            selected = .SETTINGS_DEFAULTS$dedup_rule
          ),
          shiny::tags$div(
            style = "font-size:11.5px; color:#6b7280; margin-top:8px; line-height:1.5;",
            "Applied on the next Vitek2 upload. Existing already-deduplicated
             tables are not re-collapsed retroactively."
          )
        )
      ),
      bslib::card(
        bslib::card_header("Match thresholds"),
        bslib::card_body(
          shiny::sliderInput(
            ns("thresh_auto"),
            "Auto-match threshold (score ≥ x → confirmed)",
            min = 50, max = 100,
            value = .SETTINGS_DEFAULTS$thresh_auto, step = 5
          ),
          shiny::sliderInput(
            ns("thresh_review"),
            "Needs-review lower bound (score ≥ x → review bucket)",
            min = 20, max = 79,
            value = .SETTINGS_DEFAULTS$thresh_review, step = 5
          ),
          shiny::tags$div(
            style = "font-size:11.5px; color:#6b7280; margin-top:6px; line-height:1.5;",
            "Click ", shiny::tags$em("Re-run match"),
            " on the Ingestion tab to apply new thresholds to the current batch."
          ),
          shiny::tags$div(
            style = "margin-top:12px;",
            shiny::uiOutput(ns("threshold_warn"))
          )
        )
      )
    ),
    placeholder_card("Settings (OpenSpecimen connection + auth)")
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

settingsServer <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {

    # ── Seed app_state defaults the first time the module runs ─────────
    shiny::isolate({
      if (is.null(app_state$thresh_auto))
        app_state$thresh_auto   <- .SETTINGS_DEFAULTS$thresh_auto
      if (is.null(app_state$thresh_review))
        app_state$thresh_review <- .SETTINGS_DEFAULTS$thresh_review
      if (is.null(app_state$dedup_rule))
        app_state$dedup_rule    <- .SETTINGS_DEFAULTS$dedup_rule
    })

    # ── Push input changes onto app_state ──────────────────────────────
    shiny::observeEvent(input$thresh_auto, {
      app_state$thresh_auto <- input$thresh_auto
      # Keep review < auto, snap if user pushed past it
      if (!is.null(input$thresh_review) && input$thresh_review >= input$thresh_auto) {
        new_review <- max(20, input$thresh_auto - 5)
        shiny::updateSliderInput(session, "thresh_review", value = new_review)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$thresh_review, {
      app_state$thresh_review <- input$thresh_review
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$dedup_rule, {
      app_state$dedup_rule <- input$dedup_rule
    }, ignoreInit = TRUE)

    # ── Sanity warning when the gap is unusual ─────────────────────────
    output$threshold_warn <- shiny::renderUI({
      a <- input$thresh_auto   %||% .SETTINGS_DEFAULTS$thresh_auto
      r <- input$thresh_review %||% .SETTINGS_DEFAULTS$thresh_review
      if (r >= a) {
        shiny::tags$div(
          style = "font-size:11.5px; color:#b91c1c;",
          "Review threshold must be below the auto threshold."
        )
      } else if ((a - r) <= 5) {
        shiny::tags$div(
          style = "font-size:11.5px; color:#b45309;",
          "Narrow gap between thresholds — the review bucket will be small."
        )
      } else {
        NULL
      }
    })

  })
}
