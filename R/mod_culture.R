# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/mod_culture.R — quantitative culture dashboard
# ─────────────────────────────────────────────────────────────────────────────

.culture_empty <- function() {
  specimens_empty() |>
    dplyr::mutate(
      time_point_label = character(),
      time_point_date = as.Date(character()),
      time_point_sort = as.Date(character()),
      cfu_status = character()
    )
}

.culture_specimens <- function(app_state) {
  sp <- app_state$specimens
  if ((is.null(sp) || nrow(sp) == 0) && !is.null(app_state$db_conn)) {
    sp <- tryCatch(read_table(app_state$db_conn, "specimens"), error = function(e) tibble::tibble())
  }
  if (is.null(sp) || nrow(sp) == 0) return(.culture_empty())

  sp |>
    tibble::as_tibble() |>
    dplyr::mutate(
      custom_day = dplyr::na_if(trimws(as.character(.data$custom_day)), ""),
      custom_selective_media = dplyr::na_if(trimws(as.character(.data$custom_selective_media)), ""),
      custom_organism = dplyr::na_if(trimws(as.character(.data$custom_organism)), ""),
      custom_mdro = dplyr::na_if(trimws(as.character(.data$custom_mdro)), ""),
      participant_id = dplyr::na_if(trimws(as.character(.data$participant_id)), ""),
      time_point_date = dplyr::coalesce(as.Date(.data$custom_collection_date), as.Date(.data$collection_dt)),
      time_point_label = dplyr::coalesce(.data$custom_day, as.character(.data$time_point_date), "—"),
      time_point_sort = .data$time_point_date,
      has_quant = dplyr::coalesce(as.logical(.data$has_quant), FALSE),
      is_pseudocount = dplyr::coalesce(as.logical(.data$is_pseudocount), FALSE),
      cfu_censored = dplyr::coalesce(as.logical(.data$cfu_censored), FALSE),
      cfu_status = dplyr::case_when(
        .data$is_pseudocount ~ "pseudocount",
        .data$cfu_flag == "renormalized" | .data$cfu_censored ~ "normalized",
        !is.na(.data$cfu_flag) ~ "review",
        .data$has_quant ~ "parsed",
        TRUE ~ "none"
      )
    )
}

.culture_fmt_sci <- function(x) {
  out <- rep("—", length(x))
  idx <- !is.na(x)
  out[idx] <- format(x[idx], scientific = TRUE, digits = 3)
  out
}

cultureUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tags$style("
      .cul-outer { display:flex; flex-direction:column; gap:16px; padding:20px; }
      .cul-chip-bar { display:flex; gap:8px; flex-wrap:wrap; align-items:center; }
      .cul-chip { display:flex; align-items:center; gap:8px; border:1px solid #d1d5db;
        border-radius:18px; background:#fff; padding:3px 10px 3px 12px; min-height:34px; }
      .cul-chip-label { font-size:11px; font-weight:700; color:#6b7280; text-transform:uppercase;
        letter-spacing:.4px; white-space:nowrap; }
      .cul-chip .shiny-input-container { margin:0 !important; width:180px !important; }
      .cul-chip .selectize-input { min-height:25px; padding:3px 8px; border:0; box-shadow:none; }
      .cul-grid { display:grid; grid-template-columns:minmax(0,1fr) 280px; gap:16px; align-items:start; }
      .cul-kpis { display:grid; grid-template-columns:repeat(5,minmax(120px,1fr)); gap:10px; }
      .cul-kpi { background:#fff; border:1px solid #e8e6e0; border-radius:8px; padding:12px; }
      .cul-kpi-label { font-size:10.5px; color:#6b7280; font-weight:700; text-transform:uppercase; letter-spacing:.45px; }
      .cul-kpi-value { font-family:'IBM Plex Mono',monospace; color:#1f3a5f; font-size:22px; font-weight:700; line-height:1.15; }
      .cul-kpi-sub { color:#6b7280; font-size:11px; }
      .cul-export-row { display:grid; grid-template-columns:repeat(2,minmax(220px,1fr)); gap:10px; }
      .cul-export { background:#fff; border:1px solid #e8e6e0; border-radius:8px; padding:12px;
        display:flex; align-items:center; justify-content:space-between; gap:12px; }
      .cul-export-path { min-width:0; color:#6b7280; font-size:11px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
      .cul-card { background:#fff; border:1px solid #e8e6e0; border-radius:10px; padding:16px; box-shadow:0 1px 3px rgba(0,0,0,.05); }
      .cul-card-title { font-size:12px; font-weight:700; text-transform:uppercase; letter-spacing:.6px; color:#6b7280; margin-bottom:10px; }
      .cul-rail-list { display:flex; flex-direction:column; gap:6px; max-height:330px; overflow:auto; }
      .cul-rail-btn { text-align:left; border:1px solid #e8e6e0; background:#fff; border-radius:8px; padding:8px 10px; color:#1f3a5f; font-size:12px; }
      .cul-rail-btn.active { border-color:#1f3a5f; background:#eef2f8; }
      .cul-rail-meta { color:#6b7280; font-size:11px; margin-top:2px; }
      .cul-muted { color:#6b7280; }
      @media (max-width: 1000px) { .cul-grid { grid-template-columns:1fr; } .cul-kpis { grid-template-columns:repeat(2,1fr); } }
    "),
    shiny::div(
      class = "cul-outer",
      shiny::div(
        class = "cul-chip-bar",
        .cul_select_chip(ns, "project", "Project", multiple = TRUE),
        .cul_select_chip(ns, "organism", "Organism", multiple = TRUE),
        .cul_select_chip(ns, "mdro", "MDRO", multiple = TRUE),
        .cul_select_chip(ns, "media", "Media", multiple = TRUE),
        .cul_select_chip(ns, "unit", "Unit", multiple = TRUE),
        shiny::div(
          class = "cul-chip",
          shiny::span(class = "cul-chip-label", "Include EB"),
          shiny::checkboxInput(ns("include_eb"), label = NULL, value = TRUE, width = "42px")
        )
      ),
      shiny::div(
        class = "cul-kpis",
        .cul_kpi(ns("kpi_participants"), "Participants with quant", ns("kpi_participants_sub")),
        .cul_kpi(ns("kpi_quant"), "Quantitative isolates", ns("kpi_quant_sub")),
        .cul_kpi(ns("kpi_median"), "Median CFU/mL", ns("kpi_median_sub")),
        .cul_kpi(ns("kpi_eb"), "EB-only", ns("kpi_eb_sub")),
        .cul_kpi(ns("kpi_flagged"), "Flagged for review", ns("kpi_flagged_sub"))
      ),
      shiny::div(
        class = "cul-export-row",
        shiny::div(
          class = "cul-export",
          shiny::div(
            shiny::div(class = "cul-card-title", "Review CSV"),
            shiny::div(class = "cul-export-path", shiny::textOutput(ns("review_export_path"), inline = TRUE))
          ),
          shiny::actionButton(ns("export_review_csv"), "Export review CSV", class = "btn btn-outline-primary btn-sm")
        ),
        shiny::div(
          class = "cul-export",
          shiny::div(
            shiny::div(class = "cul-card-title", "Summary CSV"),
            shiny::div(class = "cul-export-path", shiny::textOutput(ns("summary_export_path"), inline = TRUE))
          ),
          shiny::actionButton(ns("export_summary_csv"), "Export summary CSV", class = "btn btn-outline-primary btn-sm")
        )
      ),
      shiny::div(
        class = "cul-grid",
        shiny::div(
          class = "cul-card",
          shiny::div(class = "cul-card-title", shiny::textOutput(ns("chart_title"), inline = TRUE)),
          echarts4r::echarts4rOutput(ns("cfu_chart"), height = "340px"),
          shiny::hr(),
          shiny::div(class = "cul-card-title", shiny::textOutput(ns("table_title"), inline = TRUE)),
          DT::DTOutput(ns("specimen_table"))
        ),
        shiny::div(
          class = "cul-card",
          shiny::div(class = "cul-card-title", "Participant"),
          shiny::uiOutput(ns("participant_rail")),
          shiny::hr(),
          shiny::div(class = "cul-card-title", "Time points"),
          shiny::uiOutput(ns("timepoint_rail"))
        )
      )
    )
  )
}

.cul_select_chip <- function(ns, id, label, multiple = FALSE) {
  shiny::div(
    class = "cul-chip",
    shiny::span(class = "cul-chip-label", label),
    shiny::selectizeInput(ns(id), label = NULL, choices = NULL, multiple = multiple,
                          options = list(plugins = list("remove_button")))
  )
}

.cul_kpi <- function(value_id, label, sub_id) {
  shiny::div(
    class = "cul-kpi",
    shiny::div(class = "cul-kpi-label", label),
    shiny::div(class = "cul-kpi-value", shiny::textOutput(value_id, inline = TRUE)),
    shiny::div(class = "cul-kpi-sub", shiny::textOutput(sub_id, inline = TRUE))
  )
}

cultureServer <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected_participant <- shiny::reactiveVal(NULL)
    selected_timepoint <- shiny::reactiveVal(NULL)
    last_review_export <- shiny::reactiveVal("Not exported yet")
    last_summary_export <- shiny::reactiveVal("Not exported yet")

    base_data <- shiny::reactive(.culture_specimens(app_state))

    shiny::observe({
      dat <- base_data()
      shiny::updateSelectizeInput(session, "project", choices = sort(unique(stats::na.omit(dat$cp_short_title))), server = TRUE)
      shiny::updateSelectizeInput(session, "organism", choices = sort(unique(stats::na.omit(dat$custom_organism))), server = TRUE)
      shiny::updateSelectizeInput(session, "mdro", choices = sort(unique(stats::na.omit(dat$custom_mdro))), server = TRUE)
      shiny::updateSelectizeInput(session, "media", choices = sort(unique(stats::na.omit(dat$custom_selective_media))), server = TRUE)
      shiny::updateSelectizeInput(session, "unit", choices = sort(unique(stats::na.omit(dat$cfu_unit))), server = TRUE)
    })

    filtered_data <- shiny::reactive({
      dat <- base_data()
      if (length(input$project)) dat <- dplyr::filter(dat, .data$cp_short_title %in% input$project)
      if (length(input$organism)) dat <- dplyr::filter(dat, .data$custom_organism %in% input$organism)
      if (length(input$mdro)) dat <- dplyr::filter(dat, .data$custom_mdro %in% input$mdro)
      if (length(input$media)) dat <- dplyr::filter(dat, .data$custom_selective_media %in% input$media)
      if (length(input$unit)) dat <- dplyr::filter(dat, .data$cfu_unit %in% input$unit)
      if (!isTRUE(input$include_eb)) dat <- dplyr::filter(dat, .data$growth_method != "EB" | is.na(.data$growth_method))
      dat
    })

    culture_batch_id <- function() {
      app_state$batch_id %||% format(Sys.time(), "%Y%m%d%H%M%S")
    }

    output$review_export_path <- shiny::renderText(last_review_export())
    output$summary_export_path <- shiny::renderText(last_summary_export())

    shiny::observeEvent(input$export_review_csv, {
      tryCatch({
        info <- write_cfu_review_csv(
          filtered_data(),
          batch_id = culture_batch_id(),
          output_dir = file.path("data", "exports")
        )
        last_review_export(info$path)
        shiny::showNotification(
          sprintf("Wrote %s CFU review rows to %s.", format(info$n, big.mark = ","), info$path),
          type = "message",
          duration = 6
        )
      }, error = function(e) {
        shiny::showNotification(paste("CFU review CSV export failed:", e$message),
                                type = "error", duration = 10)
        warning("AXIS export_review_csv failed: ", e$message)
      })
    })

    shiny::observeEvent(input$export_summary_csv, {
      tryCatch({
        info <- write_cfu_summary_csv(
          filtered_data(),
          batch_id = culture_batch_id(),
          output_dir = file.path("data", "exports")
        )
        last_summary_export(info$path)
        shiny::showNotification(
          sprintf("Wrote %s CFU summary rows to %s.", format(info$n, big.mark = ","), info$path),
          type = "message",
          duration = 6
        )
      }, error = function(e) {
        shiny::showNotification(paste("CFU summary CSV export failed:", e$message),
                                type = "error", duration = 10)
        warning("AXIS export_summary_csv failed: ", e$message)
      })
    })

    shiny::observe({
      quant <- filtered_data() |> dplyr::filter(.data$has_quant, !is.na(.data$participant_id))
      if (nrow(quant) == 0) return(selected_participant(NULL))
      if (is.null(selected_participant()) || !selected_participant() %in% quant$participant_id) {
        selected_participant(quant$participant_id[[1]])
      }
    })

    shiny::observeEvent(input$participant_pick, {
      selected_participant(input$participant_pick)
      selected_timepoint(NULL)
    }, ignoreInit = TRUE)

    participant_data <- shiny::reactive({
      req <- selected_participant()
      if (is.null(req) || !nzchar(req)) return(filtered_data()[0, ])
      filtered_data() |> dplyr::filter(.data$participant_id == req)
    })

    shiny::observe({
      dat <- participant_data() |> dplyr::filter(.data$has_quant)
      if (nrow(dat) == 0) return(selected_timepoint(NULL))
      keys <- dat |>
        dplyr::arrange(.data$time_point_sort, .data$time_point_label) |>
        dplyr::pull(.data$time_point_label) |>
        unique()
      if (is.null(selected_timepoint()) || !selected_timepoint() %in% keys) selected_timepoint(keys[[1]])
    })

    shiny::observeEvent(input$timepoint_pick, {
      selected_timepoint(input$timepoint_pick)
    }, ignoreInit = TRUE)

    output$kpi_participants <- shiny::renderText({
      length(unique(stats::na.omit(filtered_data()$participant_id[filtered_data()$has_quant])))
    })
    output$kpi_participants_sub <- shiny::renderText("with parsed or pseudocounted CFU")
    output$kpi_quant <- shiny::renderText(sum(filtered_data()$has_quant, na.rm = TRUE))
    output$kpi_quant_sub <- shiny::renderText(paste0("of ", nrow(filtered_data()), " cultured specimens"))
    output$kpi_median <- shiny::renderText({
      vals <- filtered_data() |>
        dplyr::filter(.data$cfu_unit == "CFU/mL", .data$growth_method != "EB", !.data$is_pseudocount) |>
        dplyr::pull(.data$cfu_value)
      if (length(stats::na.omit(vals)) == 0) return("—")
      .culture_fmt_sci(stats::median(vals, na.rm = TRUE))
    })
    output$kpi_median_sub <- shiny::renderText({
      vals <- filtered_data() |>
        dplyr::filter(.data$cfu_unit == "CFU/mL", .data$growth_method != "EB", !.data$is_pseudocount) |>
        dplyr::pull(.data$cfu_log10)
      if (length(stats::na.omit(vals)) == 0) return("DP only")
      paste0("log10 ", round(stats::median(vals, na.rm = TRUE), 2), " · DP only")
    })
    output$kpi_eb <- shiny::renderText(sum(filtered_data()$growth_method == "EB" | filtered_data()$is_pseudocount, na.rm = TRUE))
    output$kpi_eb_sub <- shiny::renderText("EB or pseudocount rows")
    output$kpi_flagged <- shiny::renderText(sum(!is.na(filtered_data()$cfu_flag), na.rm = TRUE))
    output$kpi_flagged_sub <- shiny::renderText("normalization/review flags")

    output$chart_title <- shiny::renderText({
      paste0(selected_participant() %||% "No participant", " · quantitative culture across time points")
    })

    output$cfu_chart <- echarts4r::renderEcharts4r({
      dat <- participant_data() |>
        dplyr::filter(.data$has_quant, !is.na(.data$cfu_log10)) |>
        dplyr::mutate(
          organism = dplyr::coalesce(.data$custom_organism, "Unspecified"),
          time_point_label = factor(.data$time_point_label, levels = unique(.data$time_point_label[order(.data$time_point_sort)]))
        )
      if (nrow(dat) == 0) {
        return(
          tibble::tibble(time_point = "No data", cfu_log10 = 0) |>
            echarts4r::e_charts(time_point) |>
            echarts4r::e_line(cfu_log10, symbol_size = 0) |>
            echarts4r::e_y_axis(show = FALSE) |>
            echarts4r::e_x_axis(show = FALSE) |>
            echarts4r::e_title("No quantitative culture values yet")
        )
      }

      dat |>
        dplyr::group_by(.data$organism) |>
        echarts4r::e_charts(time_point_label) |>
        echarts4r::e_line(cfu_log10, symbol_size = 10) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_y_axis(name = "log10 CFU", scale = TRUE) |>
        echarts4r::e_x_axis(name = "Time point") |>
        echarts4r::e_legend(bottom = 0)
    })

    output$table_title <- shiny::renderText({
      paste0("Co-collected specimens · ", selected_participant() %||% "—", " · ", selected_timepoint() %||% "—")
    })

    output$specimen_table <- DT::renderDT({
      dat <- participant_data()
      tp <- selected_timepoint()
      if (!is.null(tp)) dat <- dplyr::filter(dat, .data$time_point_label == tp)
      dat <- dat |>
        dplyr::transmute(
          Specimen = .data$specimen_label,
          Media = dplyr::coalesce(.data$custom_selective_media, "—"),
          Organism = dplyr::coalesce(.data$custom_organism, "—"),
          MDRO = dplyr::coalesce(.data$custom_mdro, "—"),
          `CFU raw` = dplyr::coalesce(.data$cfu_raw, "—"),
          `CFU normalized` = dplyr::if_else(is.na(.data$cfu_value), "—", .culture_fmt_sci(.data$cfu_value)),
          Unit = dplyr::coalesce(.data$cfu_unit, "—"),
          Method = dplyr::coalesce(.data$growth_method, "—"),
          Status = .data$cfu_status
        )
      DT::datatable(dat, rownames = FALSE, options = list(pageLength = 8, dom = "tip", scrollX = TRUE))
    })

    output$participant_rail <- shiny::renderUI({
      dat <- filtered_data() |>
        dplyr::filter(.data$has_quant, !is.na(.data$participant_id)) |>
        dplyr::count(.data$participant_id, .data$cp_short_title, name = "n_quant") |>
        dplyr::arrange(dplyr::desc(.data$n_quant), .data$participant_id)
      if (nrow(dat) == 0) return(shiny::div(class = "cul-muted", "No quantitative culture rows loaded."))
      shiny::div(
        class = "cul-rail-list",
        purrr::pmap(dat, function(participant_id, cp_short_title, n_quant) {
          cls <- paste("cul-rail-btn", if (identical(participant_id, selected_participant())) "active" else "")
          shiny::actionButton(
            ns(paste0("pick_", make.names(participant_id))),
            label = shiny::tagList(
              shiny::div(participant_id),
              shiny::div(class = "cul-rail-meta", paste(cp_short_title, "·", n_quant, "quant rows"))
            ),
            class = cls,
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})",
                              ns("participant_pick"), htmltools::htmlEscape(participant_id))
          )
        })
      )
    })

    output$timepoint_rail <- shiny::renderUI({
      dat <- participant_data() |>
        dplyr::filter(.data$has_quant) |>
        dplyr::count(.data$time_point_label, .data$time_point_date, name = "n_quant") |>
        dplyr::arrange(.data$time_point_date, .data$time_point_label)
      if (nrow(dat) == 0) return(shiny::div(class = "cul-muted", "Select a participant with CFU data."))
      shiny::div(
        class = "cul-rail-list",
        purrr::pmap(dat, function(time_point_label, time_point_date, n_quant) {
          cls <- paste("cul-rail-btn", if (identical(time_point_label, selected_timepoint())) "active" else "")
          shiny::actionButton(
            ns(paste0("tp_", make.names(time_point_label))),
            label = shiny::tagList(
              shiny::div(time_point_label),
              shiny::div(class = "cul-rail-meta", paste(as.character(time_point_date), "·", n_quant, "quant rows"))
            ),
            class = cls,
            onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})",
                              ns("timepoint_pick"), htmltools::htmlEscape(time_point_label))
          )
        })
      )
    })
  })
}
