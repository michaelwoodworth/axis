# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/ui_helpers.R
# Reusable UI primitives used across modules.
# ─────────────────────────────────────────────────────────────────────────────

#' MDRO badge chip
#'
#' @param label  Character. MDRO category name.
#' @param colors Named character vector mapping MDRO names to hex colours.
#'               Defaults to MDRO_COLORS from theme.R.
mdro_badge <- function(label, colors = MDRO_COLORS) {
  col <- if (!is.null(colors[label]) && !is.na(colors[label])) colors[label] else "#6b7280"
  shiny::tags$span(
    label,
    style = paste0(
      "display:inline-block; padding:2px 8px; border-radius:20px;",
      "font-size:11.5px; font-weight:600; color:#fff;",
      "background:", col, "; line-height:1.6;"
    )
  )
}

#' Filter pill button
#'
#' @param inputId   Shiny input ID.
#' @param label     Display label.
#' @param value     Current value shown inside the pill.
#' @param active    Logical — adds a blue border when TRUE.
filter_pill <- function(inputId, label, value = "All", active = FALSE) {
  border_col <- if (active) "#1f3a5f" else "#d6d3cc"
  bg_col     <- if (active) "#eef2f8" else "#ffffff"
  shiny::actionButton(
    inputId = inputId,
    label   = shiny::tagList(
      shiny::tags$span(label, style = "font-size:11px; color:#6b7280; font-weight:600;"),
      shiny::tags$span(value, style = "font-size:12.5px; font-weight:500; margin-left:4px;")
    ),
    style = paste0(
      "border:1px solid ", border_col, "; background:", bg_col, ";",
      "border-radius:20px; padding:4px 14px; height:auto;",
      "box-shadow:none; font-family:inherit;"
    )
  )
}

#' KPI tile (text-only — sparkline version comes with echarts4r in Inventory)
#'
#' @param label   Metric name.
#' @param value   Formatted string value (e.g. "2,194").
#' @param delta   Optional delta label (e.g. "+12 this week").
#' @param color   Hex colour for the value text.
kpi_tile <- function(label, value, delta = NULL, color = "#1a1d24") {
  shiny::div(
    class = "axis-kpi-tile",
    style = paste0(
      "display:flex; flex-direction:column; gap:4px;",
      "padding:16px 20px; background:#ffffff;",
      "border:1px solid #e8e6e0; border-radius:10px;"
    ),
    shiny::tags$div(
      label,
      style = "font-size:11.5px; color:#6b7280; font-weight:600;
               text-transform:uppercase; letter-spacing:0.5px;"
    ),
    shiny::tags$div(
      value,
      style = paste0(
        "font-size:26px; font-weight:700; color:", color, ";",
        "font-family:'IBM Plex Mono',monospace; line-height:1.1;"
      )
    ),
    if (!is.null(delta)) {
      shiny::tags$div(
        delta,
        style = "font-size:11.5px; color:#6b7280;"
      )
    }
  )
}

#' Placeholder card used by stubs
#'
#' @param module_name  Human-readable module name for the message.
placeholder_card <- function(module_name) {
  bslib::card(
    style = "min-height:300px;",
    bslib::card_body(
      class = "d-flex align-items-center justify-content-center",
      shiny::div(
        style = "text-align:center; color:#9ca3af;",
        shiny::tags$div(
          style = "font-size:32px; margin-bottom:12px;",
          "\U0001F6A7"  # 🚧
        ),
        shiny::tags$div(
          style = "font-size:14px; font-weight:600;",
          paste0(module_name, " module — coming soon")
        ),
        shiny::tags$div(
          style = "font-size:12px; margin-top:6px;",
          "Scaffold in place · implementation pending"
        )
      )
    )
  )
}
