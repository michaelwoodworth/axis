# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/mod_ingestion.R  — Ingestion module
#
# Implements the "2 · Vitek2 ingestion · multi-file, multi-project" artboard.
# Data flow:
#   Upload XLSX/CSV → parse → dedup → pick OS projects → auto-match
#   → three-bucket preview → commit → DuckDB
# ─────────────────────────────────────────────────────────────────────────────

# ── Design tokens (mirrors axis-shared.jsx AXIS object) ─────────────────────
.AX <- list(
  primary      = "#1f3a5f",
  primarySoft  = "#eef2f8",
  accent       = "#d4a017",
  ok           = "#15803d",
  okSoft       = "#dcfce7",
  warn         = "#b45309",
  warnSoft     = "#fef3c7",
  err          = "#b91c1c",
  errSoft      = "#fee2e2",
  bg           = "#f7f7f5",
  card         = "#ffffff",
  border       = "#e8e6e0",
  borderStrong = "#d1cec8",
  ink          = "#1a1d24",
  ink2         = "#3b4252",
  muted        = "#6b7280",
  faint        = "#9ca3af"
)

# ── UI ────────────────────────────────────────────────────────────────────────

ingestionUI <- function(id) {
  ns <- NS(id)

  tags$div(
    style = "display:flex; flex-direction:column; height:100%; overflow:hidden;",

    # ── Inline CSS ──────────────────────────────────────────────────────────
    tags$style(HTML(sprintf("
      .ing-scroll { flex:1; overflow-y:auto; padding:20px 28px 24px; }
      .ing-footer {
        background:%s; border-top:1px solid %s;
        padding:13px 28px; display:flex; flex-direction:column;
        align-items:stretch; gap:8px;
        flex-shrink:0;
      }
      .ing-footer-top {
        display:flex; align-items:center; gap:12px;
      }
      .ing-export-grid {
        display:grid; grid-template-columns:1fr 1fr 1fr; gap:10px;
      }
      .ing-export-row {
        display:grid; grid-template-columns:minmax(0, 1fr) auto;
        align-items:end; gap:8px;
      }
      .ing-export-label {
        font-size:10px; font-weight:600; color:%s;
        letter-spacing:0.5px; text-transform:uppercase; margin-bottom:3px;
      }
      .ing-export-row .form-group,
      .ing-export-row .shiny-input-container {
        margin-bottom:0 !important;
      }
      .ing-panel {
        background:%s; border:1px solid %s;
        border-radius:10px; overflow:hidden;
      }
      .ing-panel-hd {
        padding:12px 16px; border-bottom:1px solid %s;
        display:flex; align-items:center; gap:10px;
      }
      .ing-panel-bd { padding:14px 16px; }
      .vitek-file-row {
        padding:10px 12px; border-radius:8px; margin-bottom:8px;
        display:flex; align-items:center; gap:10px;
      }
      .proj-row {
        padding:10px 12px; border-radius:8px; margin-bottom:6px;
        display:flex; align-items:center; gap:10px;
      }
      .bucket-col {
        background:%s; border:1px solid %s; border-radius:10px;
        display:flex; flex-direction:column; min-height:240px; overflow:hidden;
      }
      .bucket-col-hd {
        padding:11px 13px; border-bottom:1px solid %s;
        display:flex; align-items:center; gap:8px; flex-shrink:0;
      }
      .bucket-tbl {
        width:100%%; font-size:11.5px; border-collapse:collapse;
        font-family:'IBM Plex Mono',monospace;
      }
      .bucket-tbl th {
        padding:6px 9px; text-align:left; font-size:10px;
        font-weight:600; color:%s; letter-spacing:0.5px;
        text-transform:uppercase; background:%s;
        border-bottom:1px solid %s; position:sticky; top:0; z-index:1;
      }
      .bucket-tbl td {
        padding:6px 9px; border-top:1px solid %s; color:%s;
        white-space:nowrap; overflow:hidden; max-width:130px;
        text-overflow:ellipsis;
      }
      .score-badge {
        display:inline-block; padding:2px 7px; border-radius:4px;
        font-size:11px; font-weight:600;
        font-family:'IBM Plex Mono',monospace;
      }
      .vitek-drop-zone {
        padding:18px; border-radius:8px; border:1.5px dashed %s;
        color:%s; font-size:12.5px; text-align:center; cursor:pointer;
        background:%s; transition:background .15s, border-color .15s, color .15s;
      }
      .vitek-drop-zone:hover,
      .vitek-drop-zone.drag-over {
        background:%s; border-color:%s; color:%s;
      }
      .vitek-drop-zone:focus-visible {
        outline:2px solid %s; outline-offset:2px;
      }
      .vitek-drop-zone .shiny-input-container {
        position:absolute; width:1px !important; height:1px !important;
        overflow:hidden; opacity:0; pointer-events:none; margin:0 !important;
      }
      .ing-merge-panel {
        height:100%%; min-height:210px; display:flex; flex-direction:column;
        align-items:center; justify-content:center; gap:10px; text-align:center;
      }
      .ing-merge-note {
        font-size:11.5px; line-height:1.45; color:%s; max-width:170px;
      }
    ",
    .AX$card,   .AX$border, .AX$muted,  # footer
    .AX$card,   .AX$border,   # ing-panel
    .AX$border,               # ing-panel-hd
    .AX$card,   .AX$border,   # bucket-col
    .AX$border,               # bucket-col-hd
    .AX$muted,  .AX$bg, .AX$border,  # th
    .AX$border, .AX$ink,      # td
    .AX$borderStrong, .AX$muted, .AX$card, # drop zone default
    .AX$primarySoft, .AX$primary, .AX$primary, # drop zone hover
    .AX$primary,              # focus
    .AX$muted                 # merge note
    ))),
    tags$script(HTML(sprintf("
      (function() {
        function setupDropZone(zoneId, inputId) {
          var zone = document.getElementById(zoneId);
          var input = document.getElementById(inputId);
          if (!zone || !input || zone.dataset.axisBound === 'true') return;
          zone.dataset.axisBound = 'true';

          function hasFiles(evt) {
            return evt.dataTransfer && evt.dataTransfer.files && evt.dataTransfer.files.length > 0;
          }
          function stop(evt) {
            evt.preventDefault();
            evt.stopPropagation();
          }

          zone.addEventListener('click', function(evt) {
            if (evt.target !== input) input.click();
          });
          zone.addEventListener('keydown', function(evt) {
            if (evt.key === 'Enter' || evt.key === ' ') {
              stop(evt);
              input.click();
            }
          });
          zone.addEventListener('dragenter', function(evt) {
            if (!hasFiles(evt)) return;
            stop(evt);
            zone.classList.add('drag-over');
          });
          zone.addEventListener('dragover', function(evt) {
            if (!hasFiles(evt)) return;
            stop(evt);
            zone.classList.add('drag-over');
          });
          zone.addEventListener('dragleave', function(evt) {
            stop(evt);
            if (!zone.contains(evt.relatedTarget)) zone.classList.remove('drag-over');
          });
          zone.addEventListener('drop', function(evt) {
            if (!hasFiles(evt)) return;
            stop(evt);
            zone.classList.remove('drag-over');
            input.files = evt.dataTransfer.files;
            input.dispatchEvent(new Event('change', { bubbles: true }));
          });
        }
        function setupAllDropZones() {
          setupDropZone(%s, %s);
          setupDropZone(%s, %s);
        }
        document.addEventListener('DOMContentLoaded', setupAllDropZones);
        document.addEventListener('shiny:bound', setupAllDropZones);
        setTimeout(setupAllDropZones, 250);
      })();
    ",
      shQuote(ns("vitek_drop_zone")), shQuote(ns("vitek_files")),
      shQuote(ns("os_drop_zone")), shQuote(ns("os_files"))
    ))),

    # ── Scrollable content ──────────────────────────────────────────────────
    tags$div(class = "ing-scroll",

      # Header
      tags$div(
        style = "display:flex; align-items:flex-end; gap:14px; margin-bottom:18px;",
        tags$div(
          style = "flex:1;",
          uiOutput(ns("batch_label")),
          tags$div(
            "Multi-file Vitek2 ingest",
            style = sprintf("font-size:22px; font-weight:600; color:%s;
                             font-family:'IBM Plex Serif',Georgia,serif;
                             margin-top:3px;", .AX$ink)
          ),
          tags$div(
            "Drop one or more Vitek2 exports, dedupe by accession + collection date,
             then auto-link across one or more OpenSpecimen projects.",
            style = sprintf("font-size:12.5px; color:%s; margin-top:4px;",
                            .AX$muted)
          )
        ),
        actionButton(ns("view_batches"), "View past batches",
                     class = "btn btn-sm btn-outline-secondary"),
        actionButton(ns("mapping_settings"), "Mapping settings ⚙",
                     class = "btn btn-sm btn-outline-secondary")
      ),

      # ── Source upload row: Vitek | automerge | OpenSpecimen ────────────
      bslib::layout_columns(
        col_widths = c(5, 2, 5),
        gap = "14px",

        # LEFT: Vitek2 exports card
        tags$div(class = "ing-panel",
          tags$div(class = "ing-panel-hd",
            tags$span(
              style = sprintf("font-size:13px; font-weight:600; color:%s;", .AX$ink),
              "Vitek2 exports"
            ),
            tags$span(
              style = sprintf("font-size:11px; color:%s; flex:1; margin-left:6px;",
                              .AX$muted),
              "multiple files · auto-deduped on (lab_id + isolate_number)"
            )
          ),
          tags$div(class = "ing-panel-bd",
            tags$div(
              id = ns("vitek_drop_zone"),
              class = "vitek-drop-zone",
              tabindex = "0",
              tags$div(
                style = sprintf("font-size:16px; margin-bottom:6px; color:%s;", .AX$faint),
                "⇧"
              ),
              tags$div(
                style = sprintf("font-weight:600; color:%s;", .AX$ink2),
                "Drop Vitek2 exports here"
              ),
              tags$div(
                style = sprintf("font-size:11.5px; color:%s; margin-top:3px;", .AX$muted),
                "or click to choose one or more .xlsx/.xls files"
              ),
              fileInput(
                ns("vitek_files"),
                label       = NULL,
                multiple    = TRUE,
                accept      = c(".xlsx", ".xls"),
                buttonLabel = "Choose files",
                placeholder = ""
              )
            ),
            uiOutput(ns("vitek_file_rows")),
            uiOutput(ns("dedup_summary"))
          )
        ),

        tags$div(class = "ing-panel",
          tags$div(class = "ing-merge-panel",
            uiOutput(ns("run_match_button")),
            tags$div(
              class = "ing-merge-note",
              "Parse Vitek2 and OpenSpecimen files first, then run candidate linkage."
            ),
            uiOutput(ns("merge_ready_status"))
          )
        ),

        # RIGHT: OpenSpecimen exports card
        tags$div(class = "ing-panel",
          tags$div(class = "ing-panel-hd",
            tags$span(
              style = sprintf("font-size:13px; font-weight:600; color:%s;", .AX$ink),
              "OpenSpecimen exports"
            ),
            tags$span(
              style = sprintf("font-size:11px; color:%s; flex:1; margin-left:6px;",
                              .AX$muted),
              "one or more .csv/.zip/.xlsx/.xls inventory exports"
            )
          ),
          tags$div(class = "ing-panel-bd",
            tags$div(
              id = ns("os_drop_zone"),
              class = "vitek-drop-zone",
              tabindex = "0",
              tags$div(
                style = sprintf("font-size:16px; margin-bottom:6px; color:%s;", .AX$faint),
                "⇧"
              ),
              tags$div(
                style = sprintf("font-weight:600; color:%s;", .AX$ink2),
                "Drop OpenSpecimen exports here"
              ),
              tags$div(
                style = sprintf("font-size:11.5px; color:%s; margin-top:3px;", .AX$muted),
                "or click to choose one or more .csv/.zip/.xlsx/.xls files"
              ),
              fileInput(
                ns("os_files"),
                label       = NULL,
                multiple    = TRUE,
                accept      = c(".csv", ".zip", ".xlsx", ".xls"),
                buttonLabel = "Choose files",
                placeholder = ""
              )
            ),
            uiOutput(ns("os_file_rows")),
            tags$hr(style = sprintf("border-color:%s; margin:12px 0;", .AX$border)),
            tags$div(
              style = sprintf("font-size:11px; color:%s; font-weight:600;
                               letter-spacing:0.5px; text-transform:uppercase;
                               margin-bottom:6px;", .AX$muted),
              "Shared match key"
            ),
            selectInput(
              ns("match_key"),
              label    = NULL,
              choices  = c(
                "Accession ID + collection date" = "acc_date",
                "Patient ID + collection date"   = "pid_date",
                "Accession ID only"              = "acc_only"
              ),
              selected = "acc_date",
              width    = "100%"
            ),
            tags$div(
              style = sprintf("font-size:11.5px; color:%s; margin-top:6px;
                               line-height:1.5;", .AX$muted),
              "Each Vitek2 row is tried against every selected project; the highest-
               confidence hit wins. Ties are flagged for review."
            )
          )
        )
      ),

      tags$div(style = "height:14px;"),

      # ── Match preview card ────────────────────────────────────────────────
        tags$div(class = "ing-panel",
        tags$div(class = "ing-panel-hd",
          tags$div(
            style = "flex:1;",
            tags$span(
              style = sprintf("font-size:13px; font-weight:600; color:%s;",
                              .AX$ink),
              "Auto-match preview"
            ),
            tags$br(),
            uiOutput(ns("match_preview_subtitle"), inline = TRUE)
          ),
          uiOutput(ns("match_bar_ui")),
          uiOutput(ns("rerun_match_button"))
        ),
        uiOutput(ns("project_match_breakdowns"))
      ),

      tags$div(style = "height:14px;"),

      # ── Three-bucket layout ───────────────────────────────────────────────
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        gap = "14px",
        uiOutput(ns("bucket_matched")),
        uiOutput(ns("bucket_review")),
        uiOutput(ns("bucket_none"))
      ),

      tags$div(style = "height:80px;")  # breathing room above footer
    ),

    # ── Sticky footer ─────────────────────────────────────────────────────────
    tags$div(class = "ing-footer",
      tags$div(class = "ing-footer-top",
        uiOutput(ns("footer_status"), inline = TRUE),
        tags$div(style = "flex:1;"),
        actionButton(
          ns("open_review"), "Open review queue →",
          class = "btn btn-sm btn-primary",
          style = sprintf("background:%s; border-color:%s;",
                          .AX$primary, .AX$primary)
        )
      ),
      tags$div(class = "ing-export-grid",
        tags$div(class = "ing-export-row",
          tags$div(
            tags$div(class = "ing-export-label", "Cleaned CSV path"),
            uiOutput(ns("cleaned_csv_path_control"))
          ),
          actionButton(ns("commit_matched"), "Commit matched only",
                       class = "btn btn-sm btn-outline-secondary")
        ),
        tags$div(class = "ing-export-row",
          tags$div(
            tags$div(class = "ing-export-label", "Needs review CSV path"),
            uiOutput(ns("review_csv_path_control"))
          ),
          actionButton(ns("write_review_csv"), "Write Needs review CSV",
                       class = "btn btn-sm btn-outline-secondary")
        ),
        tags$div(class = "ing-export-row",
          tags$div(
            tags$div(class = "ing-export-label", "No match CSV path"),
            uiOutput(ns("none_csv_path_control"))
          ),
          actionButton(ns("write_none_csv"), "Write No match CSV",
                       class = "btn btn-sm btn-outline-secondary")
        )
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

ingestionServer <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      file_results       = list(),
      os_file_results    = list(),
      vitek_raw          = NULL,
      vitek_ast          = NULL,
      vitek_unique       = NULL,
      available_projects = NULL,
      specimens          = NULL,
      match_candidates   = NULL,
      buckets            = NULL,
      proj_summary       = NULL,
      batch_id           = NULL,
      db_conn            = NULL
    )

    # ── Init ──────────────────────────────────────────────────────────────
    observe({
      rv$batch_id <- format(Sys.time(), "B-%Y%m%d%H%M")
      app_state$batch_id <- rv$batch_id

      rv$db_conn <- tryCatch(open_db(), error = function(e) {
        showNotification(paste("DB init failed:", e$message),
                         type = "error", duration = 8)
        NULL
      })
      if (!is.null(rv$db_conn)) app_state$db_conn <- rv$db_conn

      rv$available_projects <- tibble::tibble(
        project_id  = character(), file_path   = character(),
        file_name   = character(), study_label = character(),
        n_specimens = integer(),   color       = character()
      )
    })

    session$onSessionEnded(function() close_db(isolate(rv$db_conn)))

    # ── Output: batch label ────────────────────────────────────────────────
    output$batch_label <- renderUI({
      tags$div(
        paste0("New ingestion · batch ", rv$batch_id %||% "B-…"),
        style = sprintf("font-size:11.5px; color:%s; font-weight:500;
                         letter-spacing:0.6px; text-transform:uppercase;",
                        .AX$muted)
      )
    })

    # ── Parse uploaded files ───────────────────────────────────────────────
    observeEvent(input$vitek_files, {
      req(input$vitek_files)

      shinybusy::show_modal_spinner(
        spin  = "orbit",
        color = .AX$primary,
        text  = "Parsing Vitek2 files…"
      )

      tryCatch({
        parsed <- parse_vitek_files(
          paths = input$vitek_files$datapath,
          names = input$vitek_files$name
        )
        rv$file_results <- parsed$results   # list of per-file results

        rv$vitek_raw    <- if (!is.null(parsed$vitek_raw) && nrow(parsed$vitek_raw) > 0)
          parsed$vitek_raw else NULL
        rv$vitek_ast    <- if (!is.null(parsed$vitek_ast) && nrow(parsed$vitek_ast) > 0)
          parsed$vitek_ast else NULL
        rv$vitek_unique <- if (!is.null(rv$vitek_raw))
          dedup_vitek(rv$vitek_raw, "latest") else NULL

        app_state$vitek_raw    <- rv$vitek_raw
        app_state$vitek_ast    <- rv$vitek_ast
        app_state$vitek_unique <- rv$vitek_unique

        # Reset match so it recalculates
        rv$match_candidates <- NULL
        rv$buckets          <- NULL
        rv$proj_summary     <- NULL

      }, error = function(e) {
        showNotification(paste("Parse error:", e$message), type = "error")
      }, finally = {
        shinybusy::remove_modal_spinner()
      })
    })

    # ── Parse uploaded OpenSpecimen files ─────────────────────────────────
    observeEvent(input$os_files, {
      req(input$os_files)

      shinybusy::show_modal_spinner(
        spin  = "orbit",
        color = .AX$primary,
        text  = "Parsing OpenSpecimen exports…"
      )

      tryCatch({
        parsed_os <- parse_uploaded_os_files(input$os_files)
        rv$specimens          <- parsed_os$specimens
        rv$available_projects <- parsed_os$projects
        rv$os_file_results    <- parsed_os$results

        app_state$specimens <- rv$specimens

        rv$match_candidates <- NULL
        rv$buckets          <- NULL
        rv$proj_summary     <- NULL
      }, error = function(e) {
        showNotification(paste("OpenSpecimen parse error:", e$message), type = "error")
      }, finally = {
        shinybusy::remove_modal_spinner()
      })
    })

    # ── Collect loaded project IDs ─────────────────────────────────────────
    selected_proj_ids <- reactive({
      req(rv$available_projects)
      rv$available_projects$project_id
    })

    default_cleaned_csv_path <- function(batch_id = rv$batch_id) {
      slug <- gsub("[^A-Za-z0-9_-]+", "_", batch_id %||% "B")
      file.path("data", "exports", paste0("AXIS_clean_", slug, "_isolates.csv"))
    }

    default_flagged_csv_path <- function(kind, batch_id = rv$batch_id) {
      slug <- gsub("[^A-Za-z0-9_-]+", "_", batch_id %||% "B")
      file.path("data", "exports", paste0("AXIS_", kind, "_", slug, ".csv"))
    }

    normalize_csv_path <- function(path, fallback) {
      path <- trimws(path %||% "")
      if (!nzchar(path)) path <- fallback
      if (!grepl("\\.csv$", path, ignore.case = TRUE)) path <- paste0(path, ".csv")
      path
    }

    cleaned_csv_path <- reactive({
      normalize_csv_path(input$cleaned_csv_path, default_cleaned_csv_path())
    })

    review_csv_path <- reactive({
      normalize_csv_path(input$review_csv_path, default_flagged_csv_path("needs_review"))
    })

    none_csv_path <- reactive({
      normalize_csv_path(input$none_csv_path, default_flagged_csv_path("no_match"))
    })

    merge_ready <- reactive({
      n_v <- if (!is.null(rv$vitek_unique)) nrow(rv$vitek_unique) else 0L
      n_s <- if (!is.null(rv$specimens)) nrow(rv$specimens) else 0L
      list(
        n_v = n_v,
        n_s = n_s,
        ready = n_v > 0L && n_s > 0L
      )
    })

    # ── Auto-match logic ───────────────────────────────────────────────────
    run_match <- function() {
      if (is.null(rv$vitek_unique) || nrow(rv$vitek_unique) == 0L) {
        showNotification("Upload one or more Vitek2 exports before automerge.", type = "warning")
        return(invisible(NULL))
      }
      if (is.null(rv$specimens) || nrow(rv$specimens) == 0L) {
        showNotification(
          "Upload one or more OpenSpecimen CSV/ZIP/XLSX/XLS exports before automerge.",
          type = "warning"
        )
        return(invisible(NULL))
      }

      state <- merge_ready()
      shinybusy::show_modal_spinner(
        spin  = "orbit",
        color = .AX$primary,
        text  = sprintf(
          "Running auto-match for %s Vitek rows against %s OpenSpecimen specimens…",
          format(state$n_v, big.mark = ","),
          format(state$n_s, big.mark = ",")
        )
      )

      tryCatch({
        cands <- auto_match(rv$vitek_unique, rv$specimens,
                             thresh_auto = 80, thresh_review = 50)
        rv$match_candidates  <- cands
        rv$buckets           <- bucket_results(cands, rv$vitek_unique)

        sel_proj <- rv$available_projects |>
          dplyr::filter(project_id %in% selected_proj_ids())
        rv$proj_summary <- project_match_summary(rv$buckets, sel_proj)

        app_state$match_candidates <- cands
        app_state$match_buckets    <- rv$buckets

      }, error = function(e) {
        showNotification(paste("Match error:", e$message), type = "error")
      }, finally = {
        shinybusy::remove_modal_spinner()
      })
    }

    observeEvent(input$run_match, run_match())
    observeEvent(input$rerun_match, run_match())

    # ── Commit → DuckDB ────────────────────────────────────────────────────
    observeEvent(input$commit_matched, {
      tryCatch({
        session$sendCustomMessage(
          "axis_busy_show",
          list(text = "Committing matched rows, rebuilding cleaned tables, and refreshing inventory panels.")
        )
        req(rv$buckets, rv$db_conn)
        matched <- rv$buckets$matched
        if (is.null(matched) || nrow(matched) == 0) {
          showNotification("No auto-matched records to commit.", type = "warning")
          return()
        }

        result <- commit_matched_links(
          conn              = rv$db_conn,
          matched           = matched,
          batch_id          = rv$batch_id,
          vitek_raw         = rv$vitek_raw,
          vitek_ast         = rv$vitek_ast,
          vitek_unique      = rv$vitek_unique,
          specimens         = rv$specimens,
          cleaned_overrides = app_state$cleaned_overrides,
          csv_path          = cleaned_csv_path(),
          formats           = c("csv", "xlsx", "duckdb")
        )
        app_state$links_confirmed   <- result$links_confirmed
        app_state$cleaned_overrides <- result$cleaned_overrides
        app_state$cleaned_links     <- result$cleaned_links
        app_state$cleaned_ast       <- result$cleaned_ast
        app_state$specimen_dataset  <- result$specimen_dataset

        showNotification(
          sprintf(
            "%d link%s committed to AXIS_clean_%s. Exported %d isolate links, %d AST rows, and %d parent specimens.",
            result$n_committed,
            if (result$n_committed == 1L) "" else "s",
            rv$batch_id,
            result$export_info$n_cleaned,
            result$export_info$n_ast,
            result$export_info$n_specimens
          ),
          type = "message", duration = 6
        )
      }, error = function(e) {
        showNotification(
          paste("Commit failed:", e$message),
          type = "error",
          duration = 10
        )
        warning("AXIS commit_matched failed: ", e$message)
      }, finally = {
        session$sendCustomMessage("axis_busy_hide", list())
      })
    })

    write_flagged_csv <- function(tbl, path, empty_msg) {
      if (is.null(tbl) || nrow(tbl) == 0L) {
        showNotification(empty_msg, type = "warning")
        return(invisible(NULL))
      }
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      readr::write_csv(.exportable_df(tbl), path, na = "")
      showNotification(
        sprintf("Wrote %s rows to %s.", format(nrow(tbl), big.mark = ","), path),
        type = "message",
        duration = 6
      )
      invisible(path)
    }

    observeEvent(input$write_review_csv, {
      tryCatch({
        if (is.null(rv$buckets)) {
          showNotification("Run automerge before writing Needs review CSV.", type = "warning")
          return(invisible(NULL))
        }
        review <- rv$buckets$review
        if (!is.null(review) && nrow(review) > 0L) {
          review <- review |>
            dplyr::group_by(lab_id, isolate_number) |>
            dplyr::arrange(dplyr::desc(score), .by_group = TRUE) |>
            dplyr::mutate(
              flag_status = "needs_review",
              candidate_rank = dplyr::row_number(),
              batch_id = rv$batch_id
            ) |>
            dplyr::ungroup()
        }
        write_flagged_csv(
          tbl = review,
          path = review_csv_path(),
          empty_msg = "No Needs review candidates to write."
        )
      }, error = function(e) {
        showNotification(paste("Needs review CSV export failed:", e$message),
                         type = "error", duration = 10)
        warning("AXIS write_review_csv failed: ", e$message)
      })
    })

    observeEvent(input$write_none_csv, {
      tryCatch({
        if (is.null(rv$buckets)) {
          showNotification("Run automerge before writing No match CSV.", type = "warning")
          return(invisible(NULL))
        }
        none <- rv$buckets$none
        if (!is.null(none) && nrow(none) > 0L) {
          none <- none |>
            dplyr::mutate(
              flag_status = "no_match",
              batch_id = rv$batch_id
            )
        }
        write_flagged_csv(
          tbl = none,
          path = none_csv_path(),
          empty_msg = "No No match rows to write."
        )
      }, error = function(e) {
        showNotification(paste("No match CSV export failed:", e$message),
                         type = "error", duration = 10)
        warning("AXIS write_none_csv failed: ", e$message)
      })
    })

    observeEvent(input$open_review, {
      session$sendCustomMessage("axis_select_nav", list(value = "Linking"))
    })

    # ── Per-file rows ──────────────────────────────────────────────────────
    output$vitek_file_rows <- renderUI({
      if (length(rv$file_results) == 0) return(NULL)

      # Last file in list = "latest" (matches dedup rule "latest")
      n_files     <- length(rv$file_results)
      primary_idx <- n_files   # last uploaded is primary

      tagList(purrr::imap(rv$file_results, function(r, i) {
        ui_vitek_file_row(r, is_primary = (i == primary_idx))
      }))
    })

    output$vitek_drop_hint <- renderUI({
      if (length(rv$file_results) > 0) return(NULL)
      tags$div(
        style = sprintf(
          "padding:18px; border-radius:8px; border:1.5px dashed %s;
           color:%s; font-size:12.5px; text-align:center;",
          .AX$borderStrong, .AX$muted
        ),
        tags$span(
          style = sprintf("font-size:16px; margin-right:6px; color:%s;",
                          .AX$faint),
          "⤒"
        ),
        "Drop Vitek2 exports here · or click ‘+ Add files’ above"
      )
    })

    output$dedup_summary <- renderUI({
      if (is.null(rv$vitek_raw) || is.null(rv$vitek_unique)) return(NULL)
      s <- dedup_summary(rv$vitek_raw, rv$vitek_unique)
      ui_dedup_summary(s, rv$batch_id)
    })

    # ── OS upload rows ─────────────────────────────────────────────────────
    output$os_file_rows <- renderUI({
      if (length(rv$os_file_results) == 0) {
        return(tags$div(
          style = sprintf("color:%s; font-size:12px; padding:8px 0;", .AX$muted),
          "No OpenSpecimen exports uploaded yet."
        ))
      }
      tagList(purrr::map(rv$os_file_results, ui_os_file_row))
    })

    output$merge_ready_status <- renderUI({
      state <- merge_ready()
      n_v <- state$n_v
      n_s <- state$n_s
      ready <- state$ready
      status <- dplyr::case_when(
        ready ~ sprintf("%s Vitek rows · %s OS specimens · ready",
                        format(n_v, big.mark = ","),
                        format(n_s, big.mark = ",")),
        n_v == 0L && n_s == 0L ~ "need Vitek2 rows and OpenSpecimen specimens",
        n_v == 0L ~ sprintf("need Vitek2 rows · %s OS specimens loaded",
                            format(n_s, big.mark = ",")),
        n_s == 0L ~ sprintf("%s Vitek rows loaded · need OpenSpecimen specimens",
                            format(n_v, big.mark = ",")),
        TRUE ~ "waiting for both inputs"
      )
      tags$div(
        style = sprintf(
          "font-size:10.5px; font-family:'IBM Plex Mono',monospace; color:%s;
           line-height:1.35;",
          if (ready) .AX$ok else .AX$muted
        ),
        status
      )
    })

    output$run_match_button <- renderUI({
      state <- merge_ready()
      tags$button(
        id = ns("run_match"),
        type = "button",
        "Start automerge linkage",
        class = "btn btn-sm btn-primary action-button",
        style = sprintf(
          "background:%s; border-color:%s; white-space:normal; opacity:%s;",
          .AX$primary, .AX$primary, if (state$ready) "1" else ".55"
        ),
        disabled = if (state$ready) NULL else "disabled",
        title = if (state$ready) {
          "Run linkage"
        } else {
          "Upload parsed Vitek2 and OpenSpecimen exports before running linkage"
        }
      )
    })

    output$rerun_match_button <- renderUI({
      state <- merge_ready()
      tags$button(
        id = ns("rerun_match"),
        type = "button",
        "Re-run automerge",
        class = "btn btn-sm btn-primary ms-2 action-button",
        style = sprintf(
          "background:%s; border-color:%s; opacity:%s;",
          .AX$primary, .AX$primary, if (state$ready) "1" else ".55"
        ),
        disabled = if (state$ready) NULL else "disabled",
        title = if (state$ready) {
          "Re-run linkage"
        } else {
          "Upload parsed Vitek2 and OpenSpecimen exports before running linkage"
        }
      )
    })

    output$cleaned_csv_path_control <- renderUI({
      textInput(
        ns("cleaned_csv_path"),
        label = NULL,
        value = default_cleaned_csv_path(),
        placeholder = "data/exports/AXIS_clean_B-YYYYMMDDHHMM_isolates.csv",
        width = "100%"
      )
    })

    output$review_csv_path_control <- renderUI({
      textInput(
        ns("review_csv_path"),
        label = NULL,
        value = default_flagged_csv_path("needs_review"),
        placeholder = "data/exports/AXIS_needs_review_B-YYYYMMDDHHMM.csv",
        width = "100%"
      )
    })

    output$none_csv_path_control <- renderUI({
      textInput(
        ns("none_csv_path"),
        label = NULL,
        value = default_flagged_csv_path("no_match"),
        placeholder = "data/exports/AXIS_no_match_B-YYYYMMDDHHMM.csv",
        width = "100%"
      )
    })

    # ── Match preview ──────────────────────────────────────────────────────
    output$match_preview_subtitle <- renderUI({
      n_v <- if (!is.null(rv$vitek_unique)) nrow(rv$vitek_unique) else 0
      n_p <- length(selected_proj_ids())
      tags$span(
        sprintf("%s deduped Vitek2 rows · %d selected project%s",
                format(n_v, big.mark = ","), n_p, if (n_p == 1L) "" else "s"),
        style = sprintf("font-size:11.5px; color:%s;", .AX$muted)
      )
    })

    output$match_bar_ui <- renderUI({
      b   <- rv$buckets
      n_v <- if (!is.null(rv$vitek_unique)) nrow(rv$vitek_unique) else 0
      if (is.null(b) || n_v == 0L) return(ui_match_bar(0L, 0L, 0L, 1L))
      counts <- match_bucket_counts(b)
      ui_match_bar(counts$matched, counts$review, counts$none, n_v)
    })

    output$project_match_breakdowns <- renderUI({
      req(rv$proj_summary)
      if (nrow(rv$proj_summary) == 0L) return(NULL)
      tags$div(
        style = "padding:14px 16px; display:grid;
                 grid-template-columns:repeat(auto-fill, minmax(210px, 1fr));
                 gap:12px;",
        purrr::pmap(rv$proj_summary,
          function(project_id, study_label, color,
                   n_matched, n_review, n_none, pct_matched, ...) {
            total <- n_matched + n_review + n_none
            ui_project_mini(project_id, study_label, color,
                            n_matched, n_review, n_none, total, pct_matched)
          }
        )
      )
    })

    # ── Bucket columns ─────────────────────────────────────────────────────
    output$bucket_matched <- renderUI({
      b   <- rv$buckets
      rows <- if (!is.null(b) && !is.null(b$matched) && nrow(b$matched) > 0L) {
        b$matched |> dplyr::slice_head(n = 60L) |>
          purrr::pmap(function(lab_id, isolate_number,
                                os_identifier, cp_short_title, score, ...) {
            list(paste0(lab_id, if (!is.na(isolate_number) && isolate_number != "") paste0("·", isolate_number) else ""),
                 paste0(os_identifier %||% "—", " · ",
                        substr(cp_short_title %||% "", 1L, 10L)),
                 "—",
                 paste0(round(score), "%"))
          })
      } else list()
      counts <- match_bucket_counts(b)
      ui_bucket_col("Matched", counts$matched,
                    "will commit on confirm",
                    .AX$ok, .AX$okSoft, rows, FALSE)
    })

    output$bucket_review <- renderUI({
      b <- rv$buckets
      rows <- if (!is.null(b) && !is.null(b$review) && nrow(b$review) > 0L) {
        b$review |>
          dplyr::group_by(lab_id, isolate_number) |>
          dplyr::summarise(
            n_cands   = dplyr::n(),
            top_proj  = dplyr::first(cp_short_title %||% ""),
            top_score = max(score),
            .groups   = "drop"
          ) |>
          dplyr::slice_head(n = 60L) |>
          purrr::pmap(function(lab_id, isolate_number, n_cands, top_proj, top_score, ...) {
            list(paste0(lab_id, if (!is.na(isolate_number) && isolate_number != "") paste0("·", isolate_number) else ""),
                 paste0(n_cands, " candidate",
                        if (n_cands > 1L) "s" else "",
                        " · ", substr(top_proj, 1L, 8L)),
                 "—",
                 paste0(round(top_score), "%"))
          })
      } else list()
      counts <- match_bucket_counts(b)
      n_review <- counts$review
      ui_bucket_col("Needs review", n_review,
                    "low confidence · pick a candidate",
                    .AX$warn, .AX$warnSoft, rows, TRUE)
    })

    output$bucket_none <- renderUI({
      b    <- rv$buckets
      rows <- if (!is.null(b) && !is.null(b$none) && nrow(b$none) > 0L) {
        b$none |> dplyr::slice_head(n = 60L) |>
          purrr::pmap(function(lab_id, isolate_number, parsed_study, ...) {
            list(lab_id,
                 paste0(parsed_study %||% "—"),
                 as.character(isolate_number %||% "—"),
                 "—")
          })
      } else list()
      counts <- match_bucket_counts(b)
      ui_bucket_col("No match",
                    counts$none,
                    "not in any selected project",
                    .AX$err, .AX$errSoft, rows, FALSE)
    })

    # ── Footer ─────────────────────────────────────────────────────────────
    output$footer_status <- renderUI({
      tags$span(
        style = sprintf("font-size:12.5px; color:%s;", .AX$muted),
        "Writes cleaned CSV to ",
        tags$code(cleaned_csv_path(),
                  style = sprintf("color:%s; font-size:12px;", .AX$ink)),
        " · source CSVs untouched · audit trail preserved"
      )
    })

    observe({
      b   <- rv$buckets
      counts <- match_bucket_counts(b)
      n_m <- counts$matched
      n_r <- counts$review
      n_n <- counts$none
      updateActionButton(session, "commit_matched",
                         label = sprintf("Commit matched only (%d)", n_m))
      updateActionButton(session, "open_review",
                         label = sprintf("Open review queue (%d) →", n_r))
      updateActionButton(session, "write_review_csv",
                         label = sprintf("Write Needs review CSV (%d)", n_r))
      updateActionButton(session, "write_none_csv",
                         label = sprintf("Write No match CSV (%d)", n_n))
    })

  }) # end moduleServer
}

# ── UI helper rendering functions ─────────────────────────────────────────────

ui_vitek_file_row <- function(file_info, is_primary = FALSE) {
  bg  <- if (is_primary) paste0(.AX$primarySoft) else .AX$bg
  bdr <- if (is_primary) paste0(.AX$primary, "55") else .AX$border

  schema_ok_val <- if (is.logical(file_info$schema_ok))
    all(file_info$schema_ok, na.rm = TRUE) else isTRUE(file_info$schema_ok)

  schema_tag <- if (!is.null(file_info$error)) {
    tags$span(style = sprintf("color:%s;", .AX$err), "parse error")
  } else if (schema_ok_val) {
    tags$span(style = sprintf("color:%s;", .AX$ok), "schema OK")
  } else {
    tags$span(style = sprintf("color:%s;", .AX$warn), "schema warn")
  }

  dupe_col <- if (!is.null(file_info$n_dupes) && file_info$n_dupes > 0L)
    .AX$warn else .AX$muted

  tags$div(
    class = "vitek-file-row",
    style = sprintf("background:%s; border:1px solid %s;", bg, bdr),

    tags$div(
      style = sprintf(
        "width:32px; height:32px; border-radius:6px;
         background:%s; color:%s; border:1px solid %s;
         display:grid; place-items:center; font-size:10px;
         font-family:'IBM Plex Mono',monospace; font-weight:600; flex-shrink:0;",
        if (is_primary) .AX$primary else .AX$card,
        if (is_primary) "#fff" else .AX$muted,
        if (is_primary) "none" else .AX$border
      ),
      if (is_primary) "✓" else "XLS"
    ),

    tags$div(
      style = "flex:1; min-width:0;",
      tags$div(
        style = "display:flex; align-items:baseline; gap:8px;",
        tags$span(
          file_info$file_name %||% "unknown",
          style = sprintf("font-size:12.5px; font-weight:600; color:%s;
                           font-family:'IBM Plex Mono',monospace; overflow:hidden;
                           text-overflow:ellipsis; white-space:nowrap;",
                          .AX$ink)
        ),
        if (is_primary) tags$span(
          "latest",
          style = sprintf(
            "font-size:9.5px; padding:1px 6px; border-radius:3px;
             background:%s; color:#fff; font-weight:600;
             letter-spacing:0.4px; text-transform:uppercase; flex-shrink:0;",
            .AX$primary
          )
        )
      ),
      tags$div(
        style = sprintf("font-size:11.5px; color:%s; margin-top:2px;",
                        .AX$muted),
        tags$span(
          format(file_info$n_rows, big.mark = ","),
          style = sprintf("font-family:'IBM Plex Mono',monospace; color:%s;",
                          .AX$ink2)
        ),
        sprintf(" rows · %s · ", file_info$date_range),
        tags$span(
          style = sprintf("color:%s; font-family:'IBM Plex Mono',monospace;",
                          dupe_col),
          file_info$n_dupes
        ),
        " dupes · ",
        schema_tag
      )
    )
  )
}

parse_uploaded_os_files <- function(file_input) {
  empty_projects <- tibble::tibble(
    project_id  = character(), file_path   = character(),
    file_name   = character(), study_label = character(),
    n_specimens = integer(),   color       = character()
  )
  if (is.null(file_input) || nrow(file_input) == 0L) {
    return(list(specimens = specimens_empty(), projects = empty_projects, results = list()))
  }

  palette <- c("#1f3a5f", "#5b8def", "#d4a017", "#15803d",
               "#7c3aed", "#0891b2", "#be185d")
  project_ids <- make.unique(tools::file_path_sans_ext(basename(file_input$name)),
                             sep = "_")

  parsed <- purrr::map(seq_len(nrow(file_input)), function(i) {
    file_name <- file_input$name[[i]]
    project_id <- project_ids[[i]]
    color <- palette[((i - 1L) %% length(palette)) + 1L]

    specimens <- parse_os_specimens(file_input$datapath[[i]], project_id = project_id)
    if (nrow(specimens) > 0L) specimens$source_file <- file_name

    study_label <- specimens |>
      dplyr::filter(!is.na(cp_short_title), cp_short_title != "") |>
      dplyr::pull(cp_short_title) |>
      unique()
    study_label <- if (length(study_label) > 0L) study_label[[1]] else project_id

    project <- tibble::tibble(
      project_id  = project_id,
      file_path   = file_input$datapath[[i]],
      file_name   = file_name,
      study_label = study_label,
      n_specimens = nrow(specimens),
      color       = color
    )
    result <- list(
      file_name = file_name,
      project_id = project_id,
      study_label = study_label,
      n_specimens = nrow(specimens),
      file_type = toupper(tools::file_ext(file_name)),
      schema_ok = nrow(specimens) > 0L,
      color = color
    )
    list(specimens = specimens, project = project, result = result)
  })

  list(
    specimens = purrr::map_dfr(parsed, "specimens"),
    projects  = purrr::map_dfr(parsed, "project"),
    results   = purrr::map(parsed, "result")
  )
}

ui_os_file_row <- function(file_info) {
  schema_tag <- if (isTRUE(file_info$schema_ok)) {
    tags$span(style = sprintf("color:%s;", .AX$ok), "parsed")
  } else {
    tags$span(style = sprintf("color:%s;", .AX$warn), "no rows")
  }

  tags$div(
    class = "proj-row",
    style = sprintf("background:%s; border:1px solid %s;",
                    .AX$bg, .AX$border),
    tags$div(
      style = sprintf(
        "width:32px; height:32px; border-radius:6px;
         background:%s; color:#fff; display:grid; place-items:center;
         font-size:9.5px; font-family:'IBM Plex Mono',monospace;
         font-weight:600; flex-shrink:0;",
        file_info$color %||% .AX$primary
      ),
      file_info$file_type %||% "OS"
    ),
    tags$div(style = sprintf(
      "width:4px; align-self:stretch; border-radius:2px; background:%s;",
      file_info$color %||% .AX$primary
    )),
    tags$div(
      style = "flex:1; min-width:0;",
      tags$div(
        style = "display:flex; align-items:baseline; gap:8px;",
        tags$span(
          file_info$file_name %||% "unknown",
          style = sprintf("font-size:12.5px; font-weight:600; color:%s;
                           font-family:'IBM Plex Mono',monospace; overflow:hidden;
                           text-overflow:ellipsis; white-space:nowrap;",
                          .AX$ink)
        ),
        schema_tag
      ),
      tags$div(
        sprintf("%s · %s specimens",
                file_info$study_label %||% file_info$project_id %||% "OpenSpecimen",
                format(file_info$n_specimens %||% 0L, big.mark = ",")),
        style = sprintf("font-size:11.5px; color:%s; margin-top:2px;",
                        .AX$muted)
      )
    )
  )
}

ui_dedup_summary <- function(s, batch_id) {
  tags$div(
    style = sprintf(
      "margin-top:12px; padding:12px 14px; background:%s;
       border-radius:8px; display:flex; align-items:center; gap:18px;",
      .AX$primarySoft
    ),
    tags$div(
      tags$div("Combined",
               style = sprintf("font-size:10px; color:%s; font-weight:600;
                                letter-spacing:0.5px; text-transform:uppercase;",
                               .AX$primary)),
      tags$div(format(s$n_raw, big.mark = ","),
               style = sprintf("font-size:20px; font-weight:600; color:%s;
                                font-family:'IBM Plex Mono',monospace; margin-top:2px;",
                               .AX$ink)),
      tags$div("raw rows",
               style = sprintf("font-size:10.5px; color:%s;", .AX$muted))
    ),
    tags$span("→",
              style = sprintf("color:%s; font-size:16px;", .AX$primary)),
    tags$div(
      tags$div("After dedup",
               style = sprintf("font-size:10px; color:%s; font-weight:600;
                                letter-spacing:0.5px; text-transform:uppercase;",
                               .AX$primary)),
      tags$div(format(s$n_unique, big.mark = ","),
               style = sprintf("font-size:20px; font-weight:600; color:%s;
                                font-family:'IBM Plex Mono',monospace; margin-top:2px;",
                               .AX$ink)),
      tags$div("unique rows",
               style = sprintf("font-size:10.5px; color:%s;", .AX$muted))
    ),
    tags$div(
      style = sprintf("flex:1; font-size:11.5px; color:%s; line-height:1.5;",
                      .AX$ink2),
      tags$span(
        s$n_collapsed,
        style = "font-family:'IBM Plex Mono',monospace; font-weight:600;"
      ),
      " overlapping rows collapsed · latest file wins on conflict (configurable).",
      tags$br(),
      tags$span("Cleaned output: ", style = sprintf("color:%s;", .AX$muted)),
      tags$code(
        paste0("AXIS_clean_", batch_id %||% "…"),
        style = sprintf("color:%s; font-size:11px;", .AX$ink)
      )
    )
  )
}

ui_match_bar <- function(n_m, n_r, n_n, total) {
  if (total == 0L) total <- 1L
  pct_m <- n_m / total * 100
  pct_r <- n_r / total * 100
  pct_n <- n_n / total * 100

  tags$div(
    style = "flex:1; max-width:500px; padding:0 8px;",
    tags$div(
      style = "display:flex; height:12px; border-radius:6px;
               overflow:hidden; background:#e5e7eb;",
      tags$div(style = sprintf("width:%.1f%%; background:%s;", pct_m, .AX$ok)),
      tags$div(style = sprintf("width:%.1f%%; background:%s;", pct_r, .AX$warn)),
      tags$div(style = sprintf("width:%.1f%%; background:%s;", pct_n, .AX$err))
    ),
    tags$div(
      style = "display:flex; justify-content:space-between; margin-top:5px;",
      purrr::pmap(
        list(
          list(.AX$ok,   .AX$warn,  .AX$err),
          list(n_m,      n_r,       n_n),
          list("Matched","Needs review","No match")
        ),
        function(col, n, lbl) {
          tags$span(
            style = sprintf("font-size:11px; color:%s; display:flex;
                             align-items:center; gap:4px;", .AX$muted),
            tags$span(style = sprintf(
              "display:inline-block; width:8px; height:8px;
               border-radius:2px; background:%s;", col)),
            paste0(lbl, " · ", n)
          )
        }
      )
    )
  )
}

ui_project_mini <- function(pid, study, color, n_m, n_r, n_n, total, pct) {
  if (total == 0L) total <- 1L
  tags$div(
    style = sprintf(
      "padding:12px 14px; background:%s; border-radius:8px;
       border-left:3px solid %s;",
      .AX$bg, color
    ),
    tags$div(
      style = "display:flex; align-items:baseline; gap:8px; margin-bottom:6px;",
      tags$span(pid,
                style = sprintf("font-size:11px; color:%s;
                                 font-family:'IBM Plex Mono',monospace;",
                                .AX$muted)),
      tags$span(study,
                style = sprintf("font-size:12.5px; font-weight:600; color:%s;",
                                .AX$ink)),
      tags$span(
        style = "margin-left:auto; font-size:11px;",
        tags$span(paste0(pct, "%"),
                  style = sprintf("font-family:'IBM Plex Mono',monospace;
                                   color:%s; font-weight:600;", color)),
        tags$span(" matched",
                  style = sprintf("color:%s;", .AX$muted))
      )
    ),
    tags$div(
      style = "display:flex; height:6px; border-radius:3px;
               overflow:hidden; background:#e5e7eb;",
      tags$div(style = sprintf("width:%.1f%%; background:%s;",
                               n_m/total*100, .AX$ok)),
      tags$div(style = sprintf("width:%.1f%%; background:%s;",
                               n_r/total*100, .AX$warn)),
      tags$div(style = sprintf("width:%.1f%%; background:%s;",
                               n_n/total*100, .AX$err))
    ),
    tags$div(
      style = "display:flex; justify-content:space-between; margin-top:5px;",
      tags$span(sprintf("✓ %d", n_m),
                style = sprintf("font-size:11px; color:%s;
                                 font-family:'IBM Plex Mono',monospace;", .AX$ok)),
      tags$span(sprintf("⚠ %d", n_r),
                style = sprintf("font-size:11px; color:%s;
                                 font-family:'IBM Plex Mono',monospace;", .AX$warn)),
      tags$span(sprintf("✕ %d", n_n),
                style = sprintf("font-size:11px; color:%s;
                                 font-family:'IBM Plex Mono',monospace;", .AX$err))
    )
  )
}

ui_bucket_col <- function(title, count, subtitle, tone, tone_soft,
                            rows = list(), highlight = FALSE) {
  border_css <- if (highlight)
    sprintf("border:1.5px solid %s !important; box-shadow:0 0 0 3px %s22;",
            tone, tone)
  else ""

  tbl_rows <- if (length(rows) > 0) {
    tagList(purrr::map(rows, function(r) {
      score_txt <- as.character(r[[4]])
      score_num <- suppressWarnings(as.numeric(gsub("%", "", score_txt)))
      if (is.na(score_num) || score_txt == "—") {
        sc <- tone; sb <- tone_soft
      } else if (score_num >= 80L) {
        sc <- .AX$ok;   sb <- .AX$okSoft
      } else if (score_num >= 50L) {
        sc <- .AX$warn; sb <- .AX$warnSoft
      } else {
        sc <- .AX$err;  sb <- .AX$errSoft
      }
      tags$tr(
        tags$td(as.character(r[[1]])),
        tags$td(style = sprintf("color:%s;", .AX$muted), as.character(r[[2]])),
        tags$td(style = sprintf("color:%s;", .AX$muted), as.character(r[[3]])),
        tags$td(
          tags$span(score_txt, class = "score-badge",
                    style = sprintf("background:%s; color:%s;", sb, sc))
        )
      )
    }))
  } else {
    tags$tr(tags$td(
      colspan = "4",
      style   = sprintf("color:%s; text-align:center; padding:20px; font-size:12px;",
                        .AX$faint),
      if (count == 0L) "None" else "Upload Vitek2 files to begin"
    ))
  }

  tags$div(
    class = "bucket-col",
    style = border_css,
    tags$div(
      class = "bucket-col-hd",
      tags$span(style = sprintf(
        "width:8px; height:8px; border-radius:50%%;
         background:%s; display:inline-block; flex-shrink:0;", tone
      )),
      tags$div(
        tags$div(title,
                 style = sprintf("font-size:13px; font-weight:600; color:%s;",
                                 .AX$ink)),
        tags$div(subtitle,
                 style = sprintf("font-size:11px; color:%s;", .AX$muted))
      ),
      tags$span(
        style = "margin-left:auto;",
        format(count, big.mark = ","),
        style = sprintf("font-size:17px; font-weight:600; color:%s;
                         font-family:'IBM Plex Mono',monospace;", tone)
      )
    ),
    tags$div(
      style = "overflow-y:auto; max-height:260px; flex:1;",
      tags$table(
        class = "bucket-tbl",
        tags$thead(tags$tr(
          tags$th("Vitek"),
          tags$th("Match"),
          tags$th("Patient"),
          tags$th("Score")
        )),
        tags$tbody(tbl_rows)
      )
    )
  )
}

# ── Utility ───────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
}
