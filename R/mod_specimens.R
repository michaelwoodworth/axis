# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/mod_specimens.R — OpenSpecimen specimen browser
#
# Read-only DT browser over app_state$specimens (the canonical OS export
# tibble from parse_os_specimens_multi). Adds a derived "Linked?" column by
# checking each os_identifier against app_state$links_confirmed.
#
# Filters:
#   - cp_short_title  (select)
#   - custom_mdro     (select)
#   - activity_status (select)
#   - linked          (Yes / No / All)
#   - free-text search (built into DT)
# ─────────────────────────────────────────────────────────────────────────────

.SP <- list(
  primary     = "#1f3a5f",
  primarySoft = "#eef2f8",
  ok          = "#15803d",
  okSoft      = "#dcfce7",
  warn        = "#b45309",
  warnSoft    = "#fef3c7",
  bg          = "#f7f7f5",
  card        = "#ffffff",
  border      = "#e8e6e0",
  ink         = "#1a1d24",
  ink2        = "#3b4252",
  muted       = "#6b7280",
  faint       = "#9ca3af",
  mono        = "'IBM Plex Mono', 'Courier New', monospace"
)

# ── UI ────────────────────────────────────────────────────────────────────────

specimensUI <- function(id) {
  ns <- shiny::NS(id)

  inline_css <- sprintf("
    .sp-outer       { display:flex; flex-direction:column; gap:14px;
                      padding:20px 28px; }
    .sp-header      { display:flex; align-items:flex-end; gap:14px; }
    .sp-title       { font-size:22px; font-weight:600; color:%s;
                      font-family:'IBM Plex Serif',Georgia,serif; }
    .sp-subtitle    { font-size:12.5px; color:%s; margin-top:4px; }
    .sp-filter-bar  { display:flex; gap:10px; flex-wrap:wrap;
                      align-items:flex-end; padding:14px 16px;
                      background:%s; border:1px solid %s; border-radius:10px; }
    .sp-filter-bar .form-group { margin-bottom:0 !important; }
    .sp-filter-bar .shiny-input-container { width:auto !important;
                                            min-width:160px; margin-bottom:0; }
    .sp-counts      { margin-left:auto; font-size:12px; color:%s; }
    .sp-table-card  { background:%s; border:1px solid %s; border-radius:10px;
                      padding:14px 16px; box-shadow:0 1px 3px rgba(0,0,0,.03); }
    .sp-linked-yes  { display:inline-block; padding:2px 8px; border-radius:10px;
                      font-size:11px; font-weight:600; background:%s; color:%s; }
    .sp-linked-no   { display:inline-block; padding:2px 8px; border-radius:10px;
                      font-size:11px; font-weight:600; background:%s; color:%s; }
  ",
  .SP$ink, .SP$muted,                     # title / subtitle
  .SP$card, .SP$border,                   # filter bar bg/border
  .SP$muted,                              # counts color
  .SP$card, .SP$border,                   # table card
  .SP$okSoft, .SP$ok,                     # linked yes
  "#f3f4f6", .SP$muted                    # linked no
  )

  shiny::tagList(
    shiny::tags$style(shiny::HTML(inline_css)),

    shiny::div(
      class = "sp-outer",

      # ── Header ──────────────────────────────────────────────────────────
      shiny::div(
        class = "sp-header",
        shiny::div(
          style = "flex:1;",
          shiny::div(class = "sp-title", "Specimens"),
          shiny::div(class = "sp-subtitle",
            "Raw OpenSpecimen records · read-only view of the combined export
             across all selected projects. Search, filter, and check link
             status — actual edits happen in Linking.")
        )
      ),

      # ── Filter bar ──────────────────────────────────────────────────────
      shiny::div(
        class = "sp-filter-bar",
        shiny::selectInput(
          ns("f_cp"), "Collection protocol",
          choices = c("All"), selected = "All", width = "220px"
        ),
        shiny::selectInput(
          ns("f_mdro"), "MDRO category",
          choices = c("All"), selected = "All", width = "160px"
        ),
        shiny::selectInput(
          ns("f_status"), "Activity status",
          choices = c("All"), selected = "All", width = "150px"
        ),
        shiny::selectInput(
          ns("f_linked"), "Linked?",
          choices = c("All", "Linked only" = "yes", "Unlinked only" = "no"),
          selected = "All", width = "150px"
        ),
        shiny::div(
          class = "sp-counts",
          shiny::textOutput(ns("counts"), inline = TRUE)
        )
      ),

      # ── Table card ──────────────────────────────────────────────────────
      shiny::div(
        class = "sp-table-card",
        DT::DTOutput(ns("specimens_tbl"))
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

specimensServer <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ── Set of linked os_identifiers ─────────────────────────────────────
    linked_ids <- shiny::reactive({
      lc <- app_state$links_confirmed
      if (is.null(lc) || nrow(lc) == 0) return(character(0))
      unique(na.omit(lc$os_identifier))
    })

    # ── Enriched specimens (with Linked? column) ─────────────────────────
    enriched <- shiny::reactive({
      sp <- app_state$specimens
      if (is.null(sp) || nrow(sp) == 0) return(specimens_empty())

      sp |>
        dplyr::mutate(
          linked = ifelse(os_identifier %in% linked_ids(), "yes", "no")
        )
    })

    # ── Refresh filter choices when specimens change ─────────────────────
    shiny::observe({
      sp <- enriched()
      if (nrow(sp) == 0) return()

      cps      <- sort(unique(stats::na.omit(sp$cp_short_title)))
      mdros    <- sort(unique(stats::na.omit(sp$custom_mdro)))
      statuses <- sort(unique(stats::na.omit(sp$activity_status)))

      shiny::updateSelectInput(session, "f_cp",
        choices = c("All", cps),
        selected = if (isTRUE(input$f_cp %in% c("All", cps))) input$f_cp else "All"
      )
      shiny::updateSelectInput(session, "f_mdro",
        choices = c("All", mdros),
        selected = if (isTRUE(input$f_mdro %in% c("All", mdros))) input$f_mdro else "All"
      )
      shiny::updateSelectInput(session, "f_status",
        choices = c("All", statuses),
        selected = if (isTRUE(input$f_status %in% c("All", statuses))) input$f_status else "All"
      )
    })

    # ── Filtered tibble ───────────────────────────────────────────────────
    filtered <- shiny::reactive({
      d <- enriched()
      if (nrow(d) == 0) return(d)

      if (!is.null(input$f_cp) && input$f_cp != "All")
        d <- dplyr::filter(d, cp_short_title == input$f_cp)
      if (!is.null(input$f_mdro) && input$f_mdro != "All")
        d <- dplyr::filter(d, custom_mdro == input$f_mdro)
      if (!is.null(input$f_status) && input$f_status != "All")
        d <- dplyr::filter(d, activity_status == input$f_status)
      if (!is.null(input$f_linked) && input$f_linked != "All")
        d <- dplyr::filter(d, linked == input$f_linked)

      d
    })

    # ── Counts strip ──────────────────────────────────────────────────────
    output$counts <- shiny::renderText({
      d_all <- enriched()
      d_f   <- filtered()
      if (nrow(d_all) == 0) return("No specimens loaded — select OpenSpecimen projects in Ingestion.")
      n_linked <- sum(d_f$linked == "yes", na.rm = TRUE)
      sprintf("Showing %s of %s · %s linked · %s unlinked",
              format(nrow(d_f),     big.mark = ","),
              format(nrow(d_all),   big.mark = ","),
              format(n_linked,      big.mark = ","),
              format(nrow(d_f) - n_linked, big.mark = ","))
    })

    # ── DT ────────────────────────────────────────────────────────────────
    output$specimens_tbl <- DT::renderDT({
      d <- filtered()

      if (nrow(d) == 0) {
        return(DT::datatable(
          data.frame(Status = "No specimens to display under the current filters."),
          rownames = FALSE,
          options  = list(dom = "t", paging = FALSE, ordering = FALSE,
                          searching = FALSE)
        ))
      }

      # Defensive column readers — specimens schema includes class/type/lineage
      # which collide with base R function names, so use [[ rather than bare refs.
      .col <- function(df, name) {
        if (name %in% names(df)) df[[name]] else rep(NA_character_, nrow(df))
      }

      view <- tibble::tibble(
        `Linked?`     = ifelse(d$linked == "yes",
                               '<span class="sp-linked-yes">linked</span>',
                               '<span class="sp-linked-no">—</span>'),
        Project       = .col(d, "project_id"),
        Identifier    = .col(d, "os_identifier"),
        Label         = .col(d, "specimen_label"),
        `CP title`    = .col(d, "cp_short_title"),
        Class         = .col(d, "class"),
        Type          = .col(d, "type"),
        Lineage       = .col(d, "lineage"),
        Participant   = .col(d, "participant_id"),
        Organism      = .col(d, "custom_organism"),
        MDRO          = .col(d, "custom_mdro"),
        `Coll. date`  = if ("custom_collection_date" %in% names(d))
                          format(d$custom_collection_date, "%Y-%m-%d")
                        else rep(NA_character_, nrow(d)),
        `Avail. qty`  = .col(d, "available_qty"),
        Status        = .col(d, "activity_status")
      )

      DT::datatable(
        view,
        rownames  = FALSE,
        escape    = FALSE,       # allow the linked badge HTML
        class     = "compact stripe hover",
        filter    = "none",
        options   = list(
          dom        = "ftip",
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100, 200),
          scrollX    = TRUE,
          scrollY    = "55vh",
          scrollCollapse = TRUE,
          order      = list(list(2L, "asc")),   # Identifier ascending
          columnDefs = list(
            list(targets = 0L, orderable = FALSE, width = "80px")
          )
        )
      )
    })

  })
}
