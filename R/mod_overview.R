# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/mod_overview.R  — Overview / rollup dashboard
#
# Summary view of the current cleaned dataset. Reads from app_state:
#   - links_confirmed    (DuckDB) → match counts
#   - cleaned_overrides  (DuckDB) → edit counts
#   - edit_log           (DuckDB) → activity feed
#   - vitek_unique       → match-rate denominator
#   - specimens          → join target for build_cleaned()
#
# Layout:
#   ┌─ Header (batch label + last-updated stamp) ─────────────────────────┐
#   │  KPI tile row (4 tiles)                                              │
#   │  Per-study match breakdown table                                     │
#   │  Recent activity feed (latest edit_log entries)                      │
#   │  Quick-link buttons → Ingestion / Linking / Inventory                │
#   └──────────────────────────────────────────────────────────────────────┘
# ─────────────────────────────────────────────────────────────────────────────

# ── Design tokens (mirrors .AX from mod_ingestion) ───────────────────────────
.OV <- list(
  primary     = "#1f3a5f",
  primarySoft = "#eef2f8",
  accent      = "#d4a017",
  ok          = "#15803d",
  okSoft      = "#dcfce7",
  warn        = "#b45309",
  warnSoft    = "#fef3c7",
  err         = "#b91c1c",
  errSoft     = "#fee2e2",
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

overviewUI <- function(id) {
  ns <- shiny::NS(id)

  inline_css <- sprintf("
    .ov-outer        { display:flex; flex-direction:column; gap:18px;
                       padding:22px 28px; }
    .ov-header       { display:flex; align-items:flex-end; gap:14px; }
    .ov-eyebrow      { font-size:11.5px; color:%s; font-weight:500;
                       letter-spacing:0.6px; text-transform:uppercase; }
    .ov-title        { font-size:22px; font-weight:600; color:%s;
                       font-family:'IBM Plex Serif',Georgia,serif; margin-top:3px; }
    .ov-subtitle     { font-size:12.5px; color:%s; margin-top:4px; }
    .ov-kpi-row      { display:grid; grid-template-columns:repeat(4,1fr);
                       gap:14px; }
    .ov-kpi-tile     { background:%s; border:1px solid %s; border-radius:10px;
                       padding:16px 18px; display:flex; flex-direction:column;
                       gap:4px; box-shadow:0 1px 3px rgba(0,0,0,.03); }
    .ov-kpi-label    { font-size:11px; font-weight:600; color:%s;
                       text-transform:uppercase; letter-spacing:.5px; }
    .ov-kpi-value    { font-size:28px; font-weight:700; color:%s;
                       font-family:%s; line-height:1.1; }
    .ov-kpi-sub      { font-size:11.5px; color:%s; }
    .ov-card         { background:%s; border:1px solid %s; border-radius:10px;
                       padding:16px 18px; box-shadow:0 1px 3px rgba(0,0,0,.03); }
    .ov-card-title   { font-size:12.5px; font-weight:700; color:%s;
                       text-transform:uppercase; letter-spacing:.6px;
                       margin-bottom:10px; }
    .ov-card-sub     { font-size:11.5px; color:%s; margin-bottom:12px; }
    .ov-empty        { color:%s; font-size:12.5px; font-style:italic;
                       text-align:center; padding:24px 0; }
    .ov-row          { display:grid; grid-template-columns:1.4fr 1fr; gap:14px; }
    .ov-feed         { max-height:280px; overflow-y:auto; }
    .ov-feed-item    { padding:9px 2px; border-bottom:1px solid %s;
                       font-size:12px; color:%s; line-height:1.5; }
    .ov-feed-item:last-child { border-bottom:none; }
    .ov-feed-time    { color:%s; font-family:%s; font-size:10.5px; }
    .ov-feed-field   { font-weight:600; color:%s; }
    .ov-quick-row    { display:flex; gap:10px; flex-wrap:wrap; }
    /* extra specificity so we beat Bootstrap's .btn.btn-default */
    .ov-quick-row .ov-quick-btn,
    button.ov-quick-btn.action-button {
                       flex:1; min-width:160px; padding:14px 16px;
                       border:1px solid %s !important; border-radius:10px;
                       background:%s !important; color:inherit !important;
                       text-align:left; cursor:pointer; box-shadow:none;
                       transition:border-color .15s, background .15s; }
    .ov-quick-row .ov-quick-btn:hover,
    button.ov-quick-btn.action-button:hover {
                       border-color:%s !important; background:%s !important; }
    .ov-quick-lbl    { font-size:11px; font-weight:600; color:%s;
                       text-transform:uppercase; letter-spacing:.5px; }
    .ov-quick-text   { font-size:13.5px; font-weight:600; color:%s;
                       margin-top:3px; }
    .ov-quick-sub    { font-size:11px; color:%s; margin-top:2px; }
  ",
  .OV$muted, .OV$ink, .OV$muted,                       # eyebrow / title / subtitle
  .OV$card,  .OV$border,                               # kpi-tile
  .OV$muted, .OV$primary, .OV$mono, .OV$muted,         # kpi label/value/value-font/sub
  .OV$card,  .OV$border,                               # card
  .OV$muted, .OV$muted, .OV$faint,                     # card title/sub/empty
  .OV$border, .OV$ink2,                                # feed item border / color
  .OV$muted, .OV$mono, .OV$primary,                    # feed time / mono / field
  .OV$border, .OV$card,                                # quick-btn border/bg
  .OV$primary, .OV$primarySoft,                        # quick-btn hover
  .OV$muted, .OV$ink, .OV$muted                        # quick lbl/text/sub
  )

  shiny::tagList(
    shiny::tags$style(shiny::HTML(inline_css)),

    # One-time JS message handler for navbar quick-links.
    # bslib::page_navbar renders each nav_panel as <a class="nav-link"
    # data-value="<title>">; clicking it activates the panel.
    shiny::tags$script(shiny::HTML("
      if (!window.AXIS_NAV_HANDLER_REGISTERED) {
        Shiny.addCustomMessageHandler('axis_nav_to', function(name) {
          var sel = 'a.nav-link[data-value=\"' + name + '\"]';
          var el  = document.querySelector(sel);
          if (el) el.click();
        });
        window.AXIS_NAV_HANDLER_REGISTERED = true;
      }
    ")),

    shiny::div(
      class = "ov-outer",

      # ── Header ─────────────────────────────────────────────────────────
      shiny::div(
        class = "ov-header",
        shiny::div(
          style = "flex:1;",
          shiny::div(class = "ov-eyebrow", shiny::textOutput(ns("eyebrow"), inline = TRUE)),
          shiny::div(class = "ov-title",   "Overview"),
          shiny::div(class = "ov-subtitle",
            "Cleaned dataset rollup · confirmed links, edits, and recent activity.")
        ),
        shiny::div(
          style = sprintf("font-size:11.5px; color:%s; text-align:right;", .OV$muted),
          shiny::textOutput(ns("last_updated"), inline = TRUE)
        )
      ),

      # ── KPI row ────────────────────────────────────────────────────────
      shiny::div(
        class = "ov-kpi-row",
        shiny::div(class = "ov-kpi-tile",
          shiny::div(class = "ov-kpi-label", "Confirmed links"),
          shiny::div(class = "ov-kpi-value", shiny::textOutput(ns("kpi_links"), inline = TRUE)),
          shiny::div(class = "ov-kpi-sub",   shiny::textOutput(ns("kpi_links_sub"), inline = TRUE))
        ),
        shiny::div(class = "ov-kpi-tile",
          shiny::div(class = "ov-kpi-label", "Match rate"),
          shiny::div(class = "ov-kpi-value", shiny::textOutput(ns("kpi_match"), inline = TRUE)),
          shiny::div(class = "ov-kpi-sub",   shiny::textOutput(ns("kpi_match_sub"), inline = TRUE))
        ),
        shiny::div(class = "ov-kpi-tile",
          shiny::div(class = "ov-kpi-label", "Edited records"),
          shiny::div(class = "ov-kpi-value", shiny::textOutput(ns("kpi_edits"), inline = TRUE)),
          shiny::div(class = "ov-kpi-sub",   shiny::textOutput(ns("kpi_edits_sub"), inline = TRUE))
        ),
        shiny::div(class = "ov-kpi-tile",
          shiny::div(class = "ov-kpi-label", "MDRO disagreements"),
          shiny::div(class = "ov-kpi-value", shiny::textOutput(ns("kpi_mdro"), inline = TRUE)),
          shiny::div(class = "ov-kpi-sub",   shiny::textOutput(ns("kpi_mdro_sub"), inline = TRUE))
        )
      ),

      # ── Per-study breakdown + activity feed ───────────────────────────
      shiny::div(
        class = "ov-row",
        shiny::div(
          class = "ov-card",
          shiny::div(class = "ov-card-title", "Per-study breakdown"),
          shiny::div(class = "ov-card-sub",
                     "Confirmed links by project, with auto/manual mix and edit counts."),
          DT::DTOutput(ns("study_tbl"), height = "auto")
        ),
        shiny::div(
          class = "ov-card",
          shiny::div(class = "ov-card-title", "Recent activity"),
          shiny::div(class = "ov-card-sub",
                     "Latest field-level edits captured in the audit trail."),
          shiny::uiOutput(ns("activity_feed"))
        )
      ),

      # ── Quick links ────────────────────────────────────────────────────
      shiny::div(
        class = "ov-card",
        shiny::div(class = "ov-card-title", "Quick actions"),
        shiny::div(
          class = "ov-quick-row",
          shiny::actionButton(
            ns("go_ingestion"), label = NULL,
            class = "ov-quick-btn",
            shiny::tagList(
              shiny::div(class = "ov-quick-lbl",  "Step 1"),
              shiny::div(class = "ov-quick-text", "Ingest Vitek2 exports"),
              shiny::div(class = "ov-quick-sub",  "Upload → dedup → auto-match")
            )
          ),
          shiny::actionButton(
            ns("go_linking"), label = NULL,
            class = "ov-quick-btn",
            shiny::tagList(
              shiny::div(class = "ov-quick-lbl",  "Step 2"),
              shiny::div(class = "ov-quick-text", "Review & reconcile links"),
              shiny::div(class = "ov-quick-sub",  "Confirm matches · edit fields")
            )
          ),
          shiny::actionButton(
            ns("go_inventory"), label = NULL,
            class = "ov-quick-btn",
            shiny::tagList(
              shiny::div(class = "ov-quick-lbl",  "Step 3"),
              shiny::div(class = "ov-quick-text", "Explore inventory"),
              shiny::div(class = "ov-quick-sub",  "KPIs · accrual · sites map")
            )
          ),
          shiny::actionButton(
            ns("go_specimens"), label = NULL,
            class = "ov-quick-btn",
            shiny::tagList(
              shiny::div(class = "ov-quick-lbl",  "Reference"),
              shiny::div(class = "ov-quick-text", "Browse OS specimens"),
              shiny::div(class = "ov-quick-sub",  "Raw OpenSpecimen records")
            )
          )
        )
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

overviewServer <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # ── Cleaned dataset (mirrors mod_inventory) ──────────────────────────
    cleaned_data <- shiny::reactive({
      lc <- app_state$links_confirmed
      ov <- app_state$cleaned_overrides
      vu <- app_state$vitek_unique
      sp <- app_state$specimens

      if (is.null(lc) || nrow(lc) == 0 ||
          is.null(vu) || nrow(vu) == 0 ||
          is.null(sp) || nrow(sp) == 0)
        return(cleaned_empty())

      tryCatch(
        build_cleaned(lc, ov, vu, sp),
        error = function(e) {
          warning("overview build_cleaned() error: ", e$message)
          cleaned_empty()
        }
      )
    })

    # ── Header outputs ───────────────────────────────────────────────────
    output$eyebrow <- shiny::renderText({
      lc <- app_state$links_confirmed
      if (is.null(lc) || nrow(lc) == 0) return("No batches committed yet")
      bids <- unique(na.omit(lc$batch_id))
      paste0(length(bids), " batch", if (length(bids) == 1L) "" else "es",
             " committed · latest: ", utils::tail(sort(bids), 1L))
    })

    output$last_updated <- shiny::renderText({
      lc <- app_state$links_confirmed
      if (is.null(lc) || nrow(lc) == 0) return("Updated —")
      ts <- suppressWarnings(max(lc$created_at, na.rm = TRUE))
      if (!is.finite(as.numeric(ts))) return("Updated —")
      paste0("Updated ", format(ts, "%b %d, %Y %H:%M"))
    })

    # ── KPI: Confirmed links ─────────────────────────────────────────────
    output$kpi_links <- shiny::renderText({
      lc <- app_state$links_confirmed
      n  <- if (is.null(lc)) 0L else nrow(lc)
      format(n, big.mark = ",")
    })
    output$kpi_links_sub <- shiny::renderText({
      lc <- app_state$links_confirmed
      if (is.null(lc) || nrow(lc) == 0) return("no links yet")
      n_auto   <- sum(lc$match_method == "auto",   na.rm = TRUE)
      n_manual <- sum(lc$match_method == "manual", na.rm = TRUE)
      sprintf("%s auto · %s manual",
              format(n_auto, big.mark = ","),
              format(n_manual, big.mark = ","))
    })

    # ── KPI: Match rate (confirmed / deduped vitek rows) ─────────────────
    output$kpi_match <- shiny::renderText({
      lc <- app_state$links_confirmed
      vu <- app_state$vitek_unique
      if (is.null(lc) || is.null(vu) || nrow(vu) == 0) return("—")
      rate <- nrow(lc) / nrow(vu)
      paste0(round(rate * 100), "%")
    })
    output$kpi_match_sub <- shiny::renderText({
      vu <- app_state$vitek_unique
      if (is.null(vu) || nrow(vu) == 0) return("upload Vitek to compute")
      sprintf("of %s deduped Vitek rows", format(nrow(vu), big.mark = ","))
    })

    # ── KPI: Edited records ──────────────────────────────────────────────
    output$kpi_edits <- shiny::renderText({
      ov <- app_state$cleaned_overrides
      if (is.null(ov) || nrow(ov) == 0) return("0")
      n_edited <- dplyr::n_distinct(ov$link_id)
      format(n_edited, big.mark = ",")
    })
    output$kpi_edits_sub <- shiny::renderText({
      ov <- app_state$cleaned_overrides
      lc <- app_state$links_confirmed
      if (is.null(ov) || nrow(ov) == 0) return("no overrides yet")
      n_edits <- nrow(ov)
      n_links <- if (!is.null(lc)) nrow(lc) else 0L
      pct <- if (n_links > 0L) round(dplyr::n_distinct(ov$link_id) / n_links * 100) else 0L
      sprintf("%s field edits · %d%% of links", format(n_edits, big.mark = ","), pct)
    })

    # ── KPI: MDRO disagreements ──────────────────────────────────────────
    output$kpi_mdro <- shiny::renderText({
      d <- cleaned_data()
      if (nrow(d) == 0 || !"mdro_disagree" %in% names(d)) return("—")
      n <- sum(d$mdro_disagree, na.rm = TRUE)
      format(n, big.mark = ",")
    })
    output$kpi_mdro_sub <- shiny::renderText({
      d <- cleaned_data()
      if (nrow(d) == 0) return("nothing to compare")
      n <- sum(d$mdro_disagree, na.rm = TRUE)
      if (n == 0L) "all agree across Vitek ↔ OS"
      else sprintf("Vitek2 ↔ OpenSpecimen mismatch on %s row%s",
                   format(n, big.mark = ","), if (n == 1L) "" else "s")
    })

    # ── Per-study breakdown table ────────────────────────────────────────
    study_tbl_data <- shiny::reactive({
      d  <- cleaned_data()
      ov <- app_state$cleaned_overrides

      if (nrow(d) == 0) {
        return(tibble::tibble(
          Project       = character(),
          `Study label` = character(),
          Links         = integer(),
          Auto          = integer(),
          Manual        = integer(),
          Edited        = integer(),
          `MDRO mismatch` = integer()
        ))
      }

      d |>
        dplyr::group_by(project_id, cp_short_title) |>
        dplyr::summarise(
          Links          = dplyr::n(),
          Auto           = sum(match_method == "auto",   na.rm = TRUE),
          Manual         = sum(match_method == "manual", na.rm = TRUE),
          Edited         = sum(has_edit,       na.rm = TRUE),
          `MDRO mismatch`= sum(mdro_disagree,  na.rm = TRUE),
          .groups        = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(Links)) |>
        dplyr::rename(
          Project       = project_id,
          `Study label` = cp_short_title
        )
    })

    output$study_tbl <- DT::renderDT({
      df <- study_tbl_data()
      if (nrow(df) == 0) {
        return(DT::datatable(
          data.frame(Status = "No confirmed links yet — start with Ingestion."),
          rownames = FALSE, options = list(dom = "t", paging = FALSE,
                                            ordering = FALSE, searching = FALSE)
        ))
      }
      DT::datatable(
        df,
        rownames = FALSE,
        class    = "compact stripe hover",
        options  = list(
          dom        = "t",
          paging     = FALSE,
          searching  = FALSE,
          info       = FALSE,
          ordering   = TRUE,
          scrollY    = "240px",
          scrollCollapse = TRUE,
          columnDefs = list(
            list(className = "dt-right",
                 targets   = c("Links", "Auto", "Manual",
                               "Edited", "MDRO mismatch"))
          )
        )
      )
    })

    # ── Recent activity feed (edit_log, last 15) ─────────────────────────
    output$activity_feed <- shiny::renderUI({
      el <- app_state$edit_log
      if (is.null(el) || nrow(el) == 0) {
        # Try to read on-demand from DB if connection is available
        if (!is.null(app_state$db_conn)) {
          el <- tryCatch(read_table(app_state$db_conn, "edit_log"),
                         error = function(e) NULL)
        }
      }
      if (is.null(el) || nrow(el) == 0) {
        return(shiny::div(class = "ov-empty",
          "No edits recorded yet. Edits appear here when you save reconciled
           fields in the Linking tab."))
      }

      recent <- el |>
        dplyr::arrange(dplyr::desc(when_ts)) |>
        dplyr::slice_head(n = 15L)

      shiny::div(
        class = "ov-feed",
        purrr::pmap(recent, function(link_id, field, from_value, to_value,
                                      who, when_ts, ...) {
          shiny::div(
            class = "ov-feed-item",
            shiny::tags$div(
              shiny::tags$span(class = "ov-feed-field",
                               .ov_pretty_field(field %||% "")),
              " · ",
              shiny::tags$span(
                style = sprintf("color:%s;", .OV$muted),
                .ov_trunc(from_value %||% "—", 24L)
              ),
              shiny::tags$span(
                style = sprintf("color:%s; margin:0 4px;", .OV$primary),
                "→"
              ),
              shiny::tags$span(
                style = sprintf("color:%s; font-weight:500;", .OV$ink),
                .ov_trunc(to_value %||% "—", 24L)
              )
            ),
            shiny::tags$div(
              class = "ov-feed-time",
              sprintf("%s · %s · link %s",
                      if (!is.null(when_ts) && !is.na(when_ts))
                        format(when_ts, "%Y-%m-%d %H:%M") else "—",
                      who %||% "—",
                      substr(link_id %||% "—", 1L, 8L))
            )
          )
        })
      )
    })

    # ── Quick-link nav buttons ───────────────────────────────────────────
    shiny::observeEvent(input$go_ingestion, {
      session$sendCustomMessage("axis_nav_to", "Ingestion")
    })
    shiny::observeEvent(input$go_linking, {
      session$sendCustomMessage("axis_nav_to", "Linking")
    })
    shiny::observeEvent(input$go_inventory, {
      session$sendCustomMessage("axis_nav_to", "Inventory")
    })
    shiny::observeEvent(input$go_specimens, {
      session$sendCustomMessage("axis_nav_to", "Specimens")
    })

  })
}

# ── Helpers ───────────────────────────────────────────────────────────────────

.ov_pretty_field <- function(x) {
  map <- c(
    lab_id          = "Lab ID",
    organism        = "Organism",
    specimen_type   = "Specimen type",
    mdro_category   = "MDRO category",
    testing_date    = "Testing date",
    participant_id  = "Participant ID",
    cp_title        = "CP title",
    ast_notes       = "AST notes"
  )
  out <- map[x]
  ifelse(is.na(out), x, unname(out))
}

.ov_trunc <- function(x, n) {
  s <- as.character(x)
  if (is.na(s) || s == "") return("—")
  if (nchar(s) > n) paste0(substr(s, 1L, n - 1L), "…") else s
}
