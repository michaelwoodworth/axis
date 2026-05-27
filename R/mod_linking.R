# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/mod_linking.R  — Linking / reconciliation module
# Prompt C of HANDOFF.md
#
# Layout:
#   Top tabs: All links / Below 80% / Edited fields / Disputed  (count badges)
#   Filter chip bar + Edit mode toggle (bslib::input_switch)
#   Left:  DT linked-records table
#   Right: Detail rail
#          - Identity card  (Vitek ↔ OS, confidence bar)
#          - Edit-mode banner  (writes only to AXIS_clean_b<batch>)
#          - Field reconciler  (Vitek2 | OpenSpecimen | Cleaned + status dot)
#          - Save + Revert buttons
#          - Audit timeline
#
# CRITICAL: never mutate app_state$vitek_raw or app_state$specimens.
# All edits persist only to cleaned_overrides / edit_log via write_overrides().
# ─────────────────────────────────────────────────────────────────────────────

# ── Design tokens ─────────────────────────────────────────────────────────────

.LK <- list(
  primary     = "#1f3a5f",
  primarySoft = "#eef2f8",
  ok          = "#15803d",
  okSoft      = "#dcfce7",
  warn        = "#b45309",
  warnSoft    = "#fef3c7",
  err         = "#b91c1c",
  errSoft     = "#fee2e2",
  blue        = "#0369a1",
  blueSoft    = "#e0f2fe",
  amber       = "#b45309",
  amberSoft   = "#fef3c7",
  muted       = "#6b7280",
  bg          = "#f7f7f5",
  card        = "#ffffff",
  border      = "#e8e6e0",
  mono        = "'IBM Plex Mono', 'Courier New', monospace"
)

# ── Field reconciler spec ─────────────────────────────────────────────────────
# Each entry: id (used as input id + cleaned_overrides.field),
#             label, src_v (vitek_unique col), src_o (specimens col),
#             textarea, mono.
# src_v = NULL means no Vitek source (derived / free-text).
# src_o = NULL means no OS source.

.RECONCILE_FIELDS <- list(
  list(id = "lab_id",        label = "Vitek Lab ID",        src_v = "lab_id",
       src_o = "specimen_label",           textarea = FALSE, mono = TRUE),
  list(id = "organism",      label = "Organism",             src_v = "organism_name",
       src_o = "custom_organism",          textarea = FALSE, mono = FALSE),
  list(id = "specimen_type", label = "Specimen type",        src_v = "specimen_type",
       src_o = "type",                     textarea = FALSE, mono = FALSE),
  list(id = "mdro_category", label = "MDRO category",        src_v = "parsed_target",
       src_o = "custom_mdro",             textarea = FALSE, mono = FALSE),
  list(id = "testing_date",  label = "Testing date",         src_v = "testing_date",
       src_o = "custom_collection_date",  textarea = FALSE, mono = TRUE),
  list(id = "participant_id",label = "Participant ID",        src_v = "parsed_subject",
       src_o = "participant_id",          textarea = FALSE, mono = TRUE),
  list(id = "cp_title",      label = "Collection protocol",  src_v = "cp_hint",
       src_o = "cp_short_title",          textarea = FALSE, mono = FALSE),
  list(id = "parent_specimen_type", label = "Parent specimen type", src_v = NULL,
       src_o = "custom_parent_specimen_type", textarea = FALSE, mono = FALSE),
  list(id = "ast_notes",     label = "Antibiogram notes",    src_v = NULL,
       src_o = NULL,                       textarea = TRUE,  mono = FALSE)
)

# ── Null-coalescing ───────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && a[1] != "") a else b

# ── UI ────────────────────────────────────────────────────────────────────────

linkingUI <- function(id) {
  ns <- shiny::NS(id)

  inline_css <- "
    /* ─── Linking module ─── */
    .lk-outer          { display:flex; flex-direction:column; height:100%; gap:0; }
    .lk-toolbar        { display:flex; align-items:center; gap:10px; flex-wrap:wrap;
                         padding:14px 20px 10px; background:#fff;
                         border-bottom:1px solid #e8e6e0; flex-shrink:0; }
    .lk-tab-bar        { display:flex; gap:2px; margin-right:auto; }
    .lk-tab            { display:flex; align-items:center; gap:6px;
                         padding:6px 14px; border-radius:6px; border:none;
                         background:transparent; cursor:pointer;
                         font-size:13px; font-weight:500; color:#4b5563;
                         transition:background .15s, color .15s; }
    .lk-tab:hover      { background:#f3f4f6; }
    .lk-tab.active     { background:#eef2f8; color:#1f3a5f; font-weight:600; }
    .lk-tab .badge     { font-size:10px; padding:1px 6px; border-radius:10px;
                         background:#e5e7eb; color:#374151; font-weight:600; }
    .lk-tab.active .badge { background:#1f3a5f; color:#fff; }
    .lk-chips          { display:flex; gap:6px; flex-wrap:wrap; align-items:center; }
    .lk-chip           { display:inline-grid; grid-template-columns:auto minmax(72px, auto);
                         align-items:center; gap:4px;
                         padding:3px 7px 3px 10px; border-radius:16px; font-size:12px;
                         border:1px solid #d1d5db; background:#fff; cursor:pointer;
                         font-weight:500; color:#374151; white-space:nowrap; }
    .lk-chip.active    { border-color:#1f3a5f; background:#eef2f8; color:#1f3a5f; }
    .lk-chip .form-group,
    .lk-chip .shiny-input-container { margin:0 !important; }
    .lk-chip select     { border:none; background:transparent; color:#1f3a5f;
                         font-size:12px; font-weight:600; padding:0 16px 0 0;
                         min-height:20px; height:20px; box-shadow:none; }
    .lk-body           { display:flex; flex:1; overflow:hidden; min-height:0; }
    .lk-table-pane     { flex:1; overflow:auto; padding:16px; }
    .lk-rail           { width:360px; min-width:320px; max-width:400px;
                         border-left:1px solid #e8e6e0; overflow-y:auto;
                         background:#f7f7f5; flex-shrink:0; }
    .lk-rail-empty     { display:flex; flex-direction:column; align-items:center;
                         justify-content:center; height:100%; gap:10px;
                         color:#9ca3af; font-size:13px; text-align:center;
                         padding:32px; }
    .lk-rail-content   { padding:16px; display:flex; flex-direction:column; gap:12px; }
    .lk-id-card        { background:#fff; border:1px solid #e8e6e0; border-radius:8px;
                         padding:14px 16px; }
    .lk-id-card h6     { margin:0 0 10px; font-size:12px; font-weight:600;
                         text-transform:uppercase; letter-spacing:.6px; color:#6b7280; }
    .lk-id-row         { display:flex; align-items:center; gap:8px; margin-bottom:6px; }
    .lk-id-chip        { display:inline-flex; align-items:center; gap:5px;
                         padding:3px 9px; border-radius:5px; font-size:12px;
                         font-weight:500; white-space:nowrap; }
    .lk-id-chip.vitek  { background:#eef2f8; color:#1f3a5f; }
    .lk-id-chip.os     { background:#dcfce7; color:#15803d; }
    .lk-conf-bar-wrap  { margin-top:8px; }
    .lk-conf-bar-track { height:6px; border-radius:3px; background:#e5e7eb; overflow:hidden; }
    .lk-conf-bar-fill  { height:100%; border-radius:3px; }
    .lk-conf-label     { font-size:11px; color:#6b7280; margin-top:3px; }
    .lk-edit-banner    { background:#fef3c7; border:1px solid #fde68a;
                         border-radius:7px; padding:9px 13px;
                         font-size:12px; color:#92400e; line-height:1.5; }
    .lk-edit-banner strong { color:#78350f; }
    .lk-section-label  { font-size:11px; font-weight:700; text-transform:uppercase;
                         letter-spacing:.7px; color:#6b7280; margin-bottom:4px; }
    .lk-recon-grid     { display:grid; gap:8px; }
    .lk-recon-row      { background:#fff; border:1px solid #e8e6e0; border-radius:7px;
                         padding:10px 12px; }
    .lk-recon-label    { font-size:11px; font-weight:600; color:#374151;
                         margin-bottom:6px; display:flex; align-items:center; gap:6px; }
    .lk-status-dot     { display:inline-block; width:8px; height:8px;
                         border-radius:50%; flex-shrink:0; }
    .lk-recon-values   { display:grid; grid-template-columns:1fr 1fr 1fr;
                         gap:6px; align-items:start; font-size:12px; }
    .lk-recon-col-hdr  { font-size:10px; font-weight:600; text-transform:uppercase;
                         letter-spacing:.5px; color:#9ca3af; margin-bottom:2px; }
    .lk-recon-val      { color:#374151; word-break:break-word; }
    .lk-recon-val.mono { font-family: 'IBM Plex Mono', monospace; font-size:11px; }
    .lk-recon-val.dim  { color:#9ca3af; font-style:italic; }
    .lk-action-row     { display:flex; gap:8px; }
    .lk-btn-save       { flex:1; padding:8px; border-radius:7px; border:none;
                         background:#1f3a5f; color:#fff; font-weight:600;
                         font-size:13px; cursor:pointer; }
    .lk-btn-save:hover { background:#162d4a; }
    .lk-btn-save:disabled { opacity:.5; cursor:not-allowed; }
    .lk-btn-revert     { padding:8px 16px; border-radius:7px;
                         border:1px solid #d1d5db; background:#fff;
                         color:#374151; font-weight:500; font-size:13px; cursor:pointer; }
    .lk-btn-revert:hover { background:#f9fafb; }
    .lk-audit          { background:#fff; border:1px solid #e8e6e0; border-radius:8px;
                         padding:12px 14px; }
    .lk-audit h6       { margin:0 0 10px; font-size:11px; font-weight:700;
                         text-transform:uppercase; letter-spacing:.6px; color:#6b7280; }
    .lk-audit-item     { display:flex; gap:8px; padding:5px 0;
                         border-bottom:1px solid #f3f4f6; font-size:12px;
                         color:#374151; }
    .lk-audit-item:last-child { border-bottom:none; }
    .lk-audit-ts       { color:#9ca3af; white-space:nowrap; font-size:11px;
                         flex-shrink:0; padding-top:1px; }
    .lk-audit-empty    { color:#9ca3af; font-size:12px; font-style:italic; }
    /* confidence-bar in DT */
    .conf-bar-cell     { position:relative; }
    .conf-bar-bg       { position:absolute; top:4px; bottom:4px; left:0;
                         border-radius:3px; opacity:.25; }
    .conf-bar-txt      { position:relative; font-weight:600; font-size:12px; }
  "

  shiny::tagList(
    shiny::tags$style(inline_css),

    shiny::div(
      class = "lk-outer",

      # ── Toolbar: tab bar + filter chips + edit-mode toggle ──────────────────
      shiny::div(
        class = "lk-toolbar",

        # Tab bar with count badges
        shiny::div(
          class = "lk-tab-bar",
          shiny::actionButton(ns("tab_all"),     shiny::HTML('All links <span class="badge" id="lk-badge-all">—</span>'),
                              class = "lk-tab active"),
          shiny::actionButton(ns("tab_review"),  shiny::HTML('Below 80% <span class="badge" id="lk-badge-review">—</span>'),
                              class = "lk-tab"),
          shiny::actionButton(ns("tab_edited"),  shiny::HTML('Edited fields <span class="badge" id="lk-badge-edited">—</span>'),
                              class = "lk-tab"),
          shiny::actionButton(ns("tab_disputed"),shiny::HTML('Disputed <span class="badge" id="lk-badge-disputed">—</span>'),
                              class = "lk-tab")
        ),

        # Filter chips
        shiny::div(
          class = "lk-chips",
          shiny::uiOutput(ns("filter_chips"))
        ),

        # Edit mode toggle
        bslib::input_switch(ns("edit_mode"), "Edit mode", value = FALSE)
      ),

      # ── Body: DT left + detail rail right ───────────────────────────────────
      shiny::div(
        class = "lk-body",

        # Table pane
        shiny::div(
          class = "lk-table-pane",
          DT::dataTableOutput(ns("links_tbl"), width = "100%")
        ),

        # Detail rail
        shiny::div(
          class = "lk-rail",
          shiny::uiOutput(ns("detail_rail"))
        )
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

linkingServer <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Local reactive state ──────────────────────────────────────────────────
    rv <- shiny::reactiveValues(
      active_tab    = "all",     # "all" | "review" | "edited" | "disputed"
      selected_id   = NULL,      # link_id of the selected row
      pending_edits = list(),    # named list: field_id → cleaned value
      save_status   = NULL       # "saved" | "error" | NULL
    )

    # ── Tab button observers ──────────────────────────────────────────────────
    shiny::observeEvent(input$tab_all,      { rv$active_tab <- "all";      rv$selected_id <- NULL })
    shiny::observeEvent(input$tab_review,   { rv$active_tab <- "review";   rv$selected_id <- NULL })
    shiny::observeEvent(input$tab_edited,   { rv$active_tab <- "edited";   rv$selected_id <- NULL })
    shiny::observeEvent(input$tab_disputed, { rv$active_tab <- "disputed"; rv$selected_id <- NULL })

    # Update tab button CSS classes via JS
    shiny::observe({
      active <- rv$active_tab
      tabs   <- c("tab_all", "tab_review", "tab_edited", "tab_disputed")
      keys   <- c("all",     "review",     "edited",     "disputed")
      for (i in seq_along(tabs)) {
        cls <- if (keys[i] == active) "lk-tab active" else "lk-tab"
        session$sendCustomMessage("axis_set_class",
          list(id = ns(tabs[i]), cls = cls))
      }
    })

    # ── Derived data ──────────────────────────────────────────────────────────

    # Full joined display table (links × vitek × specimens × overrides counts)
    links_data <- shiny::reactive({
      lc  <- app_state$links_confirmed
      vu  <- app_state$vitek_unique
      sp  <- app_state$specimens
      ov  <- app_state$cleaned_overrides

      if (is.null(lc) || nrow(lc) == 0) {
        return(.augment_link_filters(staged_links_display(app_state), app_state))
      }
      lc <- dedup_confirmed_links(lc)

      # Count overrides per link_id
      ov_counts <- if (!is.null(ov) && nrow(ov) > 0) {
        ov |>
          dplyr::group_by(link_id) |>
          dplyr::summarise(n_edits = dplyr::n(), .groups = "drop")
      } else {
        tibble::tibble(link_id = character(), n_edits = integer())
      }

      # Field disagreement flags from the exact committed candidate. Confidence
      # measures linkage strength; disputed means an important reconciled field
      # differs for that chosen Vitek/OpenSpecimen pair.
      mc <- app_state$match_candidates
      disagree_flags <- .link_disagreement_flags(mc)

      lc |>
        dplyr::left_join(ov_counts, by = "link_id") |>
        dplyr::left_join(
          disagree_flags,
          by = c("lab_id", "isolate_number", "os_identifier")
        ) |>
        dplyr::mutate(
          n_edits     = tidyr::replace_na(n_edits, 0L),
          conf_pct    = round(confidence * 100),
          disputed    = tidyr::replace_na(disputed, FALSE)
        ) |>
        .augment_link_filters(app_state)
    })

    output$filter_chips <- shiny::renderUI({
      d <- links_data()
      choices_for <- function(col) {
        vals <- if (nrow(d) > 0 && col %in% names(d)) sort(unique(stats::na.omit(d[[col]]))) else character()
        c("All", vals)
      }
      shiny::tagList(
        .lk_select_chip(ns("f_study"), "Study", choices_for("study_filter")),
        .lk_select_chip(ns("f_mdro"), "MDRO", choices_for("mdro_filter")),
        .lk_select_chip(ns("f_method"), "Method", choices_for("method_filter")),
        .lk_select_chip(ns("f_state"), "State", choices_for("state_filter"))
      )
    })

    # Tab-filtered view
    filtered_data <- shiny::reactive({
      d <- links_data()
      if (nrow(d) == 0) return(d)

      d <- switch(rv$active_tab,
        all      = d,
        review   = dplyr::filter(d, conf_pct < 80),
        edited   = dplyr::filter(d, n_edits  > 0),
        disputed = dplyr::filter(d, disputed)
      )

      if (!is.null(input$f_study_select) && input$f_study_select != "All")
        d <- dplyr::filter(d, study_filter == input$f_study_select)
      if (!is.null(input$f_mdro_select) && input$f_mdro_select != "All")
        d <- dplyr::filter(d, mdro_filter == input$f_mdro_select)
      if (!is.null(input$f_method_select) && input$f_method_select != "All")
        d <- dplyr::filter(d, method_filter == input$f_method_select)
      if (!is.null(input$f_state_select) && input$f_state_select != "All")
        d <- dplyr::filter(d, state_filter == input$f_state_select)

      d
    })

    # ── Tab count badges ──────────────────────────────────────────────────────
    shiny::observe({
      d <- links_data()
      n_all      <- nrow(d)
      n_review   <- sum(d$conf_pct < 80,  na.rm = TRUE)
      n_edited   <- sum(d$n_edits  > 0,   na.rm = TRUE)
      n_disputed <- sum(d$disputed,         na.rm = TRUE)

      session$sendCustomMessage("axis_update_badge",
        list(id = "lk-badge-all",      text = as.character(n_all)))
      session$sendCustomMessage("axis_update_badge",
        list(id = "lk-badge-review",   text = as.character(n_review)))
      session$sendCustomMessage("axis_update_badge",
        list(id = "lk-badge-edited",   text = as.character(n_edited)))
      session$sendCustomMessage("axis_update_badge",
        list(id = "lk-badge-disputed", text = as.character(n_disputed)))
    })

    # ── DT table ──────────────────────────────────────────────────────────────

    # JS confidence-bar renderer
    .conf_renderer <- DT::JS("
      function(data, type, row) {
        if (type !== 'display') return data;
        var pct = Math.round(data * 100);
        var col = pct >= 80 ? '#15803d' : pct >= 50 ? '#b45309' : '#b91c1c';
        return '<div class=\"conf-bar-cell\" style=\"position:relative;\">' +
          '<div class=\"conf-bar-bg\" style=\"width:' + pct + '%;background:' + col + ';' +
          'position:absolute;top:4px;bottom:4px;left:0;border-radius:3px;opacity:.2;\"></div>' +
          '<span class=\"conf-bar-txt\" style=\"position:relative;color:' + col + ';font-weight:600;font-size:12px;\">' +
          pct + '%</span></div>';
      }
    ")

    # State badge renderer
    .state_renderer <- DT::JS("
      function(data, type, row) {
        if (type !== 'display') return data;
        var bg = data === 'confirmed' ? '#dcfce7' : '#fef3c7';
        var col = data === 'confirmed' ? '#15803d' : '#b45309';
        return '<span style=\"background:' + bg + ';color:' + col +
          ';padding:2px 8px;border-radius:10px;font-size:11px;font-weight:600;\">' +
          data + '</span>';
      }
    ")

    output$links_tbl <- DT::renderDataTable({
      d <- filtered_data()
      if (nrow(d) == 0) {
        return(DT::datatable(
          links_empty_display()[, c("lab_id","os_identifier","specimen_label",
                                    "cp_short_title","confidence","match_method",
                                    "state","n_edits","link_id")],
          options = list(dom = "t", pageLength = 50),
          rownames = FALSE
        ))
      }

      display <- d |>
        dplyr::select(
          `Vitek Lab ID`  = lab_id,
          `OS Identifier` = os_identifier,
          Specimen        = specimen_label,
          `CP / Study`    = cp_short_title,
          Confidence      = confidence,
          Method          = match_method,
          State           = state,
          Edits           = n_edits,
          .link_id        = link_id
        )

      DT::datatable(
        display,
        selection  = "single",
        rownames   = FALSE,
        extensions = "Scroller",
        options    = list(
          dom        = "ti",
          scrollY    = "calc(100vh - 220px)",
          scroller   = TRUE,
          deferRender= TRUE,
          pageLength = 200,
          columnDefs = list(
            list(targets = 4, render = .conf_renderer),
            list(targets = 6, render = .state_renderer),
            list(targets = 8, visible = FALSE)   # hide link_id
          ),
          language   = list(emptyTable = "No links match the current filter.")
        )
      ) |>
        DT::formatStyle(
          "Edits",
          color = DT::styleInterval(0, c("#9ca3af", "#1f3a5f")),
          fontWeight = DT::styleInterval(0, c("normal", "700"))
        )
    }, server = TRUE)

    # ── Row-selection → selected_id ───────────────────────────────────────────
    shiny::observeEvent(input$links_tbl_rows_selected, {
      row <- input$links_tbl_rows_selected
      if (length(row) == 0) { rv$selected_id <- NULL; return() }

      d <- filtered_data()
      if (row > nrow(d)) { rv$selected_id <- NULL; return() }

      rv$selected_id    <- d$link_id[row]
      rv$pending_edits  <- list()
      rv$save_status    <- NULL
    })

    # ── Edit field observers (update pending_edits) ───────────────────────────
    # Observe each reconcile-field input when edit mode is on
    for (.fld in .RECONCILE_FIELDS) {
      local({
        fld <- .fld
        shiny::observeEvent(
          input[[paste0("clean_", fld$id)]],
          {
            if (!isTRUE(input$edit_mode)) return()
            rv$pending_edits[[fld$id]] <- input[[paste0("clean_", fld$id)]]
            rv$save_status <- NULL
          },
          ignoreInit = TRUE
        )
      })
    }

    # ── Save edits ────────────────────────────────────────────────────────────
    shiny::observeEvent(input$btn_save, {
      req(rv$selected_id, length(rv$pending_edits) > 0)
      if (startsWith(rv$selected_id, "staged::")) {
        shiny::showNotification(
          "This is a staged match candidate. Commit or manually confirm it before saving cleaned edits.",
          type = "warning",
          duration = 6
        )
        return()
      }

      lid     <- rv$selected_id
      edits   <- rv$pending_edits
      conn    <- app_state$db_conn
      who     <- "analyst"
      now_ts  <- lubridate::now()
      current_vals <- .current_cleaned_values(lid, links_data(), app_state$cleaned_overrides,
                                              app_state$vitek_unique, app_state$specimens)

      # Build cleaned_overrides rows
      overrides <- purrr::map_dfr(names(edits), function(fid) {
        tibble::tibble(
          link_id       = lid,
          field         = fid,
          cleaned_value = as.character(edits[[fid]]),
          source_hint   = "manual",
          rationale     = "",
          edited_at     = now_ts,
          edited_by     = who
        )
      })

      # Build edit_log rows
      log_entries <- purrr::map_dfr(names(edits), function(fid) {
        tibble::tibble(
          event_id   = uuid::UUIDgenerate(),
          link_id    = lid,
          event_type = "field.edited",
          field      = fid,
          from_value = as.character(current_vals[[fid]] %||% ""),
          to_value   = as.character(edits[[fid]]),
          who        = who,
          when_ts    = now_ts
        )
      })

      tryCatch({
        write_overrides(conn, overrides, log_entries)

        # Reload overrides into app_state
        app_state$cleaned_overrides <- read_table(conn, "cleaned_overrides")
        app_state$edit_log          <- read_table(conn, "edit_log")

        rv$pending_edits <- list()
        rv$save_status   <- "saved"
      }, error = function(e) {
        warning("Linking save_edits error: ", e$message)
        rv$save_status <- "error"
      })
    })

    # ── Revert edits ──────────────────────────────────────────────────────────
    shiny::observeEvent(input$btn_revert, {
      rv$pending_edits <- list()
      rv$save_status   <- NULL
      # Reset each text input to empty (UI will re-render from saved value)
      for (fld in .RECONCILE_FIELDS) {
        shiny::updateTextInput(session, paste0("clean_", fld$id), value = "")
      }
    })

    # ── Detail rail ───────────────────────────────────────────────────────────
    output$detail_rail <- shiny::renderUI({
      lid <- rv$selected_id
      if (is.null(lid)) return(.rail_empty())

      # Fetch the selected link row
      lc <- links_data()
      if (is.null(lc) || nrow(lc) == 0) return(.rail_empty())

      link_row <- lc[lc$link_id == lid, ]
      if (nrow(link_row) == 0) return(.rail_empty())

      # Fetch corresponding vitek + specimen rows
      vu <- app_state$vitek_unique
      sp <- app_state$specimens

      vitek_row <- if (!is.null(vu) && nrow(vu) > 0)
        vu[vu$lab_id == link_row$lab_id[1] &
           vu$isolate_number == link_row$isolate_number[1], ]
      else tibble::tibble()

      spec_row <- if (!is.null(sp) && nrow(sp) > 0)
        sp[sp$os_identifier == link_row$os_identifier[1], ]
      else tibble::tibble()

      # Fetch saved overrides for this link
      ov <- app_state$cleaned_overrides
      saved_ov <- if (!is.null(ov) && nrow(ov) > 0)
        ov[ov$link_id == lid, ]
      else tibble::tibble(link_id=character(), field=character(),
                          cleaned_value=character(), edited_at=lubridate::ymd_hms(character()))

      # Latest override per field
      saved_vals <- list()
      if (nrow(saved_ov) > 0) {
        latest_ov <- saved_ov |>
          dplyr::arrange(dplyr::desc(edited_at)) |>
          dplyr::distinct(field, .keep_all = TRUE)
        for (i in seq_len(nrow(latest_ov))) {
          saved_vals[[ latest_ov$field[i] ]] <- latest_ov$cleaned_value[i]
        }
      }

      # Fetch audit timeline
      el <- app_state$edit_log
      link_audit <- if (!is.null(el) && nrow(el) > 0)
        el[el$link_id == lid, ]
      else tibble::tibble()

      # ── Confidence ──────────────────────────────────────────────────────────
      conf_pct   <- round((link_row$confidence[1] %||% 0) * 100)
      conf_color <- if (conf_pct >= 80) .LK$ok else if (conf_pct >= 50) .LK$warn else .LK$err

      # ── Render ───────────────────────────────────────────────────────────────
      shiny::div(
        class = "lk-rail-content",

        # Identity card
        shiny::div(
          class = "lk-id-card",
          shiny::tags$h6("Record identity"),

          # Vitek row
          shiny::div(class = "lk-id-row",
            shiny::span(class = "lk-id-chip vitek",
              shiny::tags$small("V2"), link_row$lab_id[1]),
            shiny::span(style="color:#9ca3af;font-size:11px;", "isolate"),
            shiny::tags$code(style="font-size:11px;", link_row$isolate_number[1])
          ),

          # OS row
          shiny::div(class = "lk-id-row",
            shiny::span(class = "lk-id-chip os",
              shiny::tags$small("OS"), link_row$os_identifier[1]),
            shiny::span(style="color:#9ca3af;font-size:11px;", link_row$cp_short_title[1] %||% "")
          ),

          # Confidence bar
          shiny::div(class = "lk-conf-bar-wrap",
            shiny::div(class = "lk-conf-bar-track",
              shiny::div(class = "lk-conf-bar-fill",
                style = sprintf("width:%d%%;background:%s;", conf_pct, conf_color))
            ),
            shiny::div(class = "lk-conf-label",
              sprintf("%d%% confidence · %s", conf_pct, link_row$match_method[1] %||% "auto"))
          )
        ),

        # Edit-mode banner (shown when edit_mode is ON)
        if (isTRUE(isolate(input$edit_mode))) {
          batch_label <- link_row$batch_id[1] %||% "?"
          shiny::div(class = "lk-edit-banner",
            shiny::tags$strong("✏️ Edit mode ON"),
            shiny::tags$br(),
            if (startsWith(lid, "staged::")) {
              "This candidate is not committed yet. Save is disabled until the link is confirmed."
            } else {
              sprintf("Changes write to AXIS_clean_b%s only — source data is never modified.", batch_label)
            }
          )
        },

        # Save-status banner
        if (!is.null(rv$save_status)) {
          if (rv$save_status == "saved") {
            shiny::div(
              style = sprintf("background:%s;border:1px solid #bbf7d0;border-radius:7px;padding:8px 13px;font-size:12px;color:%s;",
                              .LK$okSoft, .LK$ok),
              "✓ Changes saved successfully."
            )
          } else {
            shiny::div(
              style = sprintf("background:%s;border:1px solid #fecaca;border-radius:7px;padding:8px 13px;font-size:12px;color:%s;",
                              .LK$errSoft, .LK$err),
              "⚠ Save failed — check console for details."
            )
          }
        },

        # Field reconciler
        shiny::div(
          shiny::div(class = "lk-section-label", "Field reconciler"),
          shiny::div(
            class = "lk-recon-grid",

            # Column headers row
            shiny::div(
              style = "display:grid; grid-template-columns:120px 1fr 1fr 1fr; gap:6px; padding:0 12px;",
              shiny::div(),
              shiny::div(class = "lk-recon-col-hdr", "Vitek2"),
              shiny::div(class = "lk-recon-col-hdr", "OpenSpecimen"),
              shiny::div(class = "lk-recon-col-hdr",
                if (isTRUE(isolate(input$edit_mode))) "Cleaned ✏" else "Cleaned")
            ),

            # One row per field
            purrr::map(.RECONCILE_FIELDS, function(fld) {
              v_val   <- .get_field_value(vitek_row, fld$src_v)
              o_val   <- .get_field_value(spec_row,  fld$src_o)
              c_saved <- saved_vals[[ fld$id ]] %||% ""
              c_pend  <- rv$pending_edits[[ fld$id ]]

              status  <- .reconcile_status(v_val, o_val, c_saved)
              dot_col <- .status_color(status)

              c_default <- .default_cleaned_value(v_val, o_val, c_saved)
              c_display <- if (!is.null(c_pend)) c_pend else c_default

              mono_style <- if (fld$mono) "font-family:'IBM Plex Mono',monospace;font-size:11px;" else ""

              shiny::div(
                class = "lk-recon-row",

                # Label + status dot
                shiny::div(class = "lk-recon-label",
                  shiny::span(class = "lk-status-dot",
                    style = sprintf("background:%s;", dot_col)),
                  fld$label
                ),

                # Values row
                shiny::div(class = "lk-recon-values",
                  # Vitek value
                  shiny::div(
                    shiny::div(
                      class = paste("lk-recon-val", if (fld$mono) "mono" else "",
                                    if (is.na(v_val) || v_val == "") "dim" else ""),
                      style = mono_style,
                      if (is.na(v_val) || v_val == "") "—" else v_val
                    )
                  ),

                  # OS value
                  shiny::div(
                    shiny::div(
                      class = paste("lk-recon-val", if (fld$mono) "mono" else "",
                                    if (is.na(o_val) || o_val == "") "dim" else ""),
                      style = mono_style,
                      if (is.na(o_val) || o_val == "") "—" else o_val
                    )
                  ),

                  # Cleaned value (editable or read-only)
                  shiny::div(
                    if (isTRUE(isolate(input$edit_mode))) {
                      if (fld$textarea) {
                        shiny::textAreaInput(
                          ns(paste0("clean_", fld$id)), label = NULL,
                          value = c_display,
                          rows  = 3,
                          placeholder = "Enter cleaned value…"
                        )
                      } else {
                        shiny::textInput(
                          ns(paste0("clean_", fld$id)), label = NULL,
                          value       = c_display,
                          placeholder = if (is.na(v_val) || v_val == "") o_val %||% "" else v_val
                        )
                      }
                    } else {
                      shiny::div(
                        class = paste("lk-recon-val", if (fld$mono) "mono" else "",
                                      if (c_display == "") "dim" else ""),
                        style = mono_style,
                        if (c_display == "") "—" else c_display
                      )
                    }
                  )
                )
              )
            })
          )
        ),

        # Edit summary (pending changes count)
        if (isTRUE(isolate(input$edit_mode))) {
          n_pend <- length(rv$pending_edits)
          shiny::div(
            style = sprintf("font-size:12px;color:%s;", if (n_pend > 0) .LK$primary else .LK$muted),
            if (n_pend > 0)
              sprintf("%d field%s modified (unsaved)", n_pend, if (n_pend == 1) "" else "s")
            else
              "No pending changes."
          )
        },

        # Save / Revert buttons (only in edit mode)
        if (isTRUE(isolate(input$edit_mode)) && !startsWith(lid, "staged::")) {
          shiny::div(
            class = "lk-action-row",
            shiny::tags$button(
              id      = ns("btn_save"),
              class   = "lk-btn-save action-button",
              onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("btn_save")),
              "Save changes"
            ),
            shiny::tags$button(
              id      = ns("btn_revert"),
              class   = "lk-btn-revert action-button",
              onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("btn_revert")),
              "Revert"
            )
          )
        },

        # Audit timeline
        shiny::div(
          class = "lk-audit",
          shiny::tags$h6("Audit timeline"),
          if (nrow(link_audit) == 0) {
            shiny::div(class = "lk-audit-empty", "No edits recorded for this link.")
          } else {
            # Most recent first
            timeline <- link_audit |>
              dplyr::arrange(dplyr::desc(when_ts)) |>
              utils::head(20)

            purrr::map(seq_len(nrow(timeline)), function(i) {
              row <- timeline[i, ]
              ts  <- tryCatch(
                format(row$when_ts, "%b %d %H:%M"),
                error = function(e) "?"
              )
              shiny::div(
                class = "lk-audit-item",
                shiny::span(class = "lk-audit-ts", ts),
                shiny::span(
                  shiny::tags$strong(row$who %||% "analyst"), " edited ",
                  shiny::tags$strong(row$field %||% "?"),
                  if (!is.na(row$to_value) && row$to_value != "")
                    shiny::span(style = "color:#6b7280;",
                      sprintf(" → \"%s\"", row$to_value))
                )
              )
            })
          }
        )
      )
    })

    # ── Re-render rail when edit_mode changes ─────────────────────────────────
    shiny::observeEvent(input$edit_mode, {
      rv$pending_edits <- list()
      rv$save_status   <- NULL
    })

    # ── Load persisted data from DB on startup ────────────────────────────────
    shiny::observe({
      conn <- app_state$db_conn
      if (is.null(conn)) return()

      if (is.null(app_state$links_confirmed)) {
        app_state$links_confirmed <- tryCatch(
          read_table(conn, "links_confirmed"),
          error = function(e) tibble::tibble()
        )
      }
      if (is.null(app_state$vitek_raw)) {
        app_state$vitek_raw <- tryCatch(
          read_table(conn, "vitek_raw"),
          error = function(e) tibble::tibble()
        )
      }
      if (is.null(app_state$vitek_unique) &&
          !is.null(app_state$vitek_raw) && nrow(app_state$vitek_raw) > 0) {
        app_state$vitek_unique <- tryCatch(
          dedup_vitek(app_state$vitek_raw, "latest"),
          error = function(e) tibble::tibble()
        )
      }
      if (is.null(app_state$specimens)) {
        app_state$specimens <- tryCatch(
          read_table(conn, "specimens"),
          error = function(e) tibble::tibble()
        )
      }
      if (is.null(app_state$cleaned_overrides)) {
        app_state$cleaned_overrides <- tryCatch(
          read_table(conn, "cleaned_overrides"),
          error = function(e) tibble::tibble()
        )
      }
      if (is.null(app_state$edit_log)) {
        app_state$edit_log <- tryCatch(
          read_table(conn, "edit_log"),
          error = function(e) tibble::tibble()
        )
      }
    })
  })
}

# ── Private helpers ───────────────────────────────────────────────────────────

#' Filter chip select control.
.lk_select_chip <- function(id, label, choices) {
  shiny::div(
    class = "lk-chip",
    id    = id,
    shiny::tags$small(paste0(label, ": ")),
    shiny::selectInput(
      inputId = paste0(id, "_select"),
      label = NULL,
      choices = choices,
      selected = "All",
      width = "100%"
    )
  )
}

.norm_filter_value <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == "" | x == "NA"] <- "Unspecified"
  x
}

.augment_link_filters <- function(d, app_state) {
  if (is.null(d) || nrow(d) == 0) return(d)

  vu <- app_state$vitek_unique
  sp <- app_state$specimens

  if (!is.null(vu) && nrow(vu) > 0) {
    d <- d |>
      dplyr::left_join(
        vu |>
          dplyr::select(
            lab_id, isolate_number,
            v_mdro_filter = dplyr::any_of("parsed_target")
          ) |>
          dplyr::distinct(),
        by = c("lab_id", "isolate_number")
      )
  } else {
    d$v_mdro_filter <- NA_character_
  }

  if (!is.null(sp) && nrow(sp) > 0) {
    d <- d |>
      dplyr::left_join(
        sp |>
          dplyr::select(
            os_identifier,
            o_mdro_filter = dplyr::any_of("custom_mdro")
          ) |>
          dplyr::distinct(),
        by = "os_identifier"
      )
  } else {
    d$o_mdro_filter <- NA_character_
  }

  d |>
    dplyr::mutate(
      mdro_filter = .norm_filter_value(dplyr::coalesce(
        dplyr::na_if(as.character(.data$v_mdro_filter), ""),
        dplyr::na_if(as.character(.data$o_mdro_filter), "")
      )),
      study_filter = .norm_filter_value(.data$cp_short_title),
      method_filter = .norm_filter_value(.data$match_method),
      state_filter = .norm_filter_value(.data$state)
    ) |>
    dplyr::select(-dplyr::any_of(c("v_mdro_filter", "o_mdro_filter")))
}

.default_cleaned_value <- function(v_val, o_val, saved_val = "") {
  saved_val <- saved_val %||% ""
  if (!is.na(saved_val) && saved_val != "") return(saved_val)
  if (!is.na(v_val) && v_val != "") return(v_val)
  if (!is.na(o_val) && o_val != "") return(o_val)
  ""
}

.current_cleaned_values <- function(link_id, links_data, overrides, vitek, specimens) {
  vals <- list()
  if (is.null(links_data) || nrow(links_data) == 0) return(vals)
  link_row <- links_data[links_data$link_id == link_id, ]
  if (nrow(link_row) == 0) return(vals)

  vitek_row <- if (!is.null(vitek) && nrow(vitek) > 0) {
    vitek[vitek$lab_id == link_row$lab_id[1] &
            vitek$isolate_number == link_row$isolate_number[1], ]
  } else tibble::tibble()

  spec_row <- if (!is.null(specimens) && nrow(specimens) > 0) {
    specimens[specimens$os_identifier == link_row$os_identifier[1], ]
  } else tibble::tibble()

  saved_vals <- list()
  if (!is.null(overrides) && nrow(overrides) > 0) {
    saved <- overrides |>
      dplyr::filter(link_id == !!link_id) |>
      dplyr::arrange(dplyr::desc(edited_at)) |>
      dplyr::distinct(field, .keep_all = TRUE)
    if (nrow(saved) > 0) {
      for (i in seq_len(nrow(saved))) saved_vals[[saved$field[i]]] <- saved$cleaned_value[i]
    }
  }

  for (fld in .RECONCILE_FIELDS) {
    vals[[fld$id]] <- .default_cleaned_value(
      .get_field_value(vitek_row, fld$src_v),
      .get_field_value(spec_row, fld$src_o),
      saved_vals[[fld$id]] %||% ""
    )
  }
  vals
}

#' Empty-state for the detail rail.
.rail_empty <- function() {
  shiny::div(
    class = "lk-rail-empty",
    shiny::tags$svg(
      xmlns   = "http://www.w3.org/2000/svg",
      width   = "32", height = "32", viewBox = "0 0 24 24",
      fill    = "none", stroke = "#d1d5db",
      `stroke-width` = "1.5",
      shiny::tags$path(`stroke-linecap`="round", `stroke-linejoin`="round",
        d = "M9 5H7a2 2 0 0 0-2 2v12a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2V7a2 2 0 0 0-2-2h-2M9 5a2 2 0 0 0 2 2h2a2 2 0 0 0 2-2M9 5a2 2 0 0 0-2 2")
    ),
    shiny::p("Select a link to view details", style = "margin:0;"),
    shiny::p(style = "color:#c4c4c4;margin:0;font-size:11px;",
      "Click any row in the table")
  )
}

#' Extract a field value from a single-row tibble (NA if missing/empty).
.get_field_value <- function(row_tbl, col_name) {
  if (is.null(col_name) || is.null(row_tbl) || nrow(row_tbl) == 0) return(NA_character_)
  if (!col_name %in% names(row_tbl)) return(NA_character_)
  val <- as.character(row_tbl[[col_name]][1])
  if (is.na(val) || val == "NA" || val == "") NA_character_ else val
}

#' Determine reconcile status from v_val, o_val, saved.
#' Returns one of: "match" | "fuzzy" | "fill" | "derived" | "edited" | "mismatch"
.reconcile_status <- function(v_val, o_val, saved_val) {
  has_v  <- !is.na(v_val)  && v_val  != ""
  has_o  <- !is.na(o_val)  && o_val  != ""
  has_s  <- !is.na(saved_val) && saved_val != ""

  if (has_s) return("edited")

  if (!has_v && !has_o) return("derived")
  if (!has_v ||  !has_o) return("fill")

  if (v_val == o_val) return("match")

  # Fuzzy: case-insensitive or prefix
  vn <- tolower(trimws(v_val))
  on <- tolower(trimws(o_val))
  if (vn == on || startsWith(vn, on) || startsWith(on, vn) ||
      grepl(vn, on, fixed = TRUE) || grepl(on, vn, fixed = TRUE))
    return("fuzzy")

  "mismatch"
}

#' Map status to a CSS color string.
.status_color <- function(status) {
  switch(status,
    match    = "#15803d",   # green
    fuzzy    = "#b45309",   # amber/warn
    fill     = "#0369a1",   # blue
    derived  = "#b45309",   # amber
    edited   = "#1f3a5f",   # primary
    mismatch = "#b91c1c",   # red
    "#9ca3af"               # fallback grey
  )
}

#' Empty links display tibble.
links_empty_display <- function() {
  tibble::tibble(
    lab_id         = character(),
    os_identifier  = character(),
    specimen_label = character(),
    cp_short_title = character(),
    confidence     = double(),
    match_method   = character(),
    state          = character(),
    n_edits        = integer(),
    link_id        = character(),
    conf_pct       = integer(),
    disputed       = logical()
  )
}

staged_links_display <- function(app_state) {
  buckets <- app_state$match_buckets
  if (is.null(buckets)) return(links_empty_display())

  matched <- if (!is.null(buckets$matched) && nrow(buckets$matched) > 0) {
    buckets$matched |>
      dplyr::mutate(
        link_id = paste("staged", lab_id, isolate_number, os_identifier, sep = "::"),
        confidence = score / 100,
        match_method = "auto_preview",
        state = "staged_matched",
        batch_id = app_state$batch_id %||% NA_character_
      )
  } else {
    tibble::tibble()
  }

  review <- if (!is.null(buckets$review) && nrow(buckets$review) > 0) {
    buckets$review |>
      dplyr::mutate(
        link_id = paste("staged", lab_id, isolate_number, os_identifier, sep = "::"),
        confidence = score / 100,
        match_method = "review_candidate",
        state = "needs_review",
        batch_id = app_state$batch_id %||% NA_character_
      )
  } else {
    tibble::tibble()
  }

  out <- dplyr::bind_rows(matched, review)
  if (nrow(out) == 0) return(links_empty_display())

  out |>
    dplyr::transmute(
      lab_id,
      isolate_number,
      os_identifier,
      project_id,
      specimen_label,
      cp_short_title,
      confidence,
      match_method,
      state,
      batch_id,
      n_edits = 0L,
      link_id,
      conf_pct = round(confidence * 100),
      disputed = dplyr::coalesce(mdro_disagree, FALSE) |
        dplyr::coalesce(organism_disagree, FALSE)
    )
}

.link_disagreement_flags <- function(match_candidates) {
  if (is.null(match_candidates) || nrow(match_candidates) == 0) {
    return(tibble::tibble(
      lab_id = character(),
      isolate_number = character(),
      os_identifier = character(),
      disputed = logical()
    ))
  }

  if (!"mdro_disagree" %in% names(match_candidates)) {
    match_candidates$mdro_disagree <- FALSE
  }
  if (!"organism_disagree" %in% names(match_candidates)) {
    match_candidates$organism_disagree <- FALSE
  }

  match_candidates |>
    dplyr::mutate(
      disputed = dplyr::coalesce(.data$mdro_disagree, FALSE) |
        dplyr::coalesce(.data$organism_disagree, FALSE)
    ) |>
    dplyr::group_by(lab_id, isolate_number, os_identifier) |>
    dplyr::summarise(disputed = any(disputed), .groups = "drop")
}
