# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/mod_inventory.R  — Inventory dashboard
# Prompt D of HANDOFF.md
#
# Layout:
#   Filter chip bar  (Study, Site, Range, Specimen, Species, Status)
#   Hero card:
#     - 4 KPI tiles with sparklines (echarts4r)
#     - Stacked-area accrual chart   (echarts4r, MDRO_COLORS)
#   Bottom row:
#     - Sankey: site → study → parent specimen → MDRO → species  (echarts4r)
#     - Sites map                       (leaflet)
#
# Data source: build_cleaned() from R/data_clean.R
# ─────────────────────────────────────────────────────────────────────────────

# ── Known site dictionary ────────────────────────────────────────────────────
# Override via options("axis.site_dictionary") with the same columns.
.INV_SITE_DICTIONARY <- tibble::tibble(
  site_code = c("ARRRRG_ATL", "SENTINEL_REACT_DECATUR", "EM", "AG", "ML", "SP"),
  site_label = c(
    "Emory University Hospital",
    "Emory Long Term Acute Care Hospital",
    "Emory Long Term Acute Care Hospital",
    "A.G. Rhodes Wesley Woods",
    "RML Specialty Hospital",
    "Good Shepherd Penn Partners"
  ),
  city = c("Atlanta", "Decatur", "Decatur", "Atlanta", "Hinsdale", "Allentown"),
  state = c("GA", "GA", "GA", "GA", "IL", "PA"),
  address = c(
    "Clifton Rd, Atlanta, GA",
    "Decatur, GA",
    "Decatur, GA",
    "Atlanta, GA",
    "Hinsdale, IL",
    "Allentown, PA"
  ),
  lat = c(33.7923, 33.7748, 33.7748, 33.7999, 41.8009, 40.6084),
  lon = c(-84.3196, -84.2963, -84.2963, -84.3236, -87.9289, -75.3927)
)

# ── UI ────────────────────────────────────────────────────────────────────────

inventoryUI <- function(id) {
  ns <- shiny::NS(id)

  inline_css <- "
    /* ─── Inventory module ─── */
    .inv-outer          { display:flex; flex-direction:column; gap:16px;
                          padding:20px; min-height:0; overflow:visible; }
    .inv-chip-bar       { display:flex; gap:8px; flex-wrap:wrap; align-items:center;
                          position:relative; z-index:1000; overflow:visible; }
    .inv-chip-wrap      { display:inline-flex; align-items:center; gap:0;
                          border:1px solid #d1d5db; border-radius:18px;
                          background:#fff; overflow:visible; cursor:pointer;
                          position:relative; z-index:1001;
                          font-size:12px; font-weight:500; color:#374151; }
    .inv-chip-wrap:hover,
    .inv-chip-wrap:focus-within { z-index:1010; }
    .inv-chip-label     { padding:4px 10px; background:#f3f4f6;
                          color:#6b7280; font-size:11px; font-weight:600;
                          text-transform:uppercase; letter-spacing:.4px;
                          border-radius:18px 0 0 18px;
                          border-right:1px solid #d1d5db; }
    .inv-chip-wrap .form-group,
    .inv-chip-wrap .shiny-input-container { margin:0 !important; overflow:visible !important; }
    .inv-chip-wrap select,
    .inv-chip-wrap input { position:relative; z-index:1011; background:#fff; }
    .inv-chip-select    { border:none; background:transparent; padding:4px 10px 4px 6px;
                          font-size:12px; color:#1f3a5f; font-weight:500;
                          outline:none; cursor:pointer; min-width:80px;
                          -webkit-appearance:none; appearance:none; }
    .inv-hero           { background:#fff; border:1px solid #e8e6e0; border-radius:10px;
                          padding:20px; box-shadow:0 1px 3px rgba(0,0,0,.05); }
    .inv-hero-title     { font-size:13px; font-weight:700; text-transform:uppercase;
                          letter-spacing:.7px; color:#6b7280; margin-bottom:14px; }
    .inv-kpi-row        { display:grid; grid-template-columns:repeat(4,1fr);
                          gap:12px; margin-bottom:20px; }
    .inv-kpi-tile       { background:#f7f7f5; border:1px solid #e8e6e0; border-radius:8px;
                          padding:12px 14px; display:flex; flex-direction:column; gap:2px; }
    .inv-kpi-label      { font-size:11px; font-weight:600; text-transform:uppercase;
                          letter-spacing:.5px; color:#6b7280; }
    .inv-kpi-value      { font-size:24px; font-weight:700; color:#1f3a5f;
                          font-family:'IBM Plex Mono',monospace; line-height:1.1; }
    .inv-kpi-sub        { font-size:11px; color:#9ca3af; }
    .inv-kpi-spark      { height:32px; margin-top:4px; }
    .inv-card           { background:#fff; border:1px solid #e8e6e0; border-radius:10px;
                          padding:18px; box-shadow:0 1px 3px rgba(0,0,0,.05); }
    .inv-card-title     { font-size:12px; font-weight:700; text-transform:uppercase;
                          letter-spacing:.6px; color:#6b7280; margin-bottom:12px; }
    .inv-empty          { display:flex; align-items:center; justify-content:center;
                          height:200px; color:#9ca3af; font-size:13px;
                          font-style:italic; }
  "

  shiny::tagList(
    shiny::tags$style(inline_css),

    shiny::div(
      class = "inv-outer",

      # ── Filter chip bar ──────────────────────────────────────────────────
      shiny::div(
        class = "inv-chip-bar",
        .inv_chip(ns("f_study"),    "Study"),
        .inv_chip(ns("f_site"),     "Site"),
        .inv_chip(ns("f_specimen"), "Specimen"),
        .inv_chip(ns("f_species"),  "Species"),
        .inv_chip(ns("f_status"),   "Status"),
        shiny::div(
          class = "inv-chip-wrap",
          shiny::span(class = "inv-chip-label", "Range"),
          shiny::dateRangeInput(
            ns("f_range"), label = NULL,
            start    = Sys.Date() - 365,
            end      = Sys.Date(),
            format   = "M d, yy",
            width    = "260px"
          )
        )
      ),

      # ── Hero card ──────────────────────────────────────────────────────────
      shiny::div(
        class = "inv-hero",
        shiny::div(class = "inv-hero-title", "Specimen Accrual Overview"),

        # KPI tiles row
        shiny::div(
          class = "inv-kpi-row",
          shiny::div(class = "inv-kpi-tile",
            shiny::div(class = "inv-kpi-label", "Total specimens"),
            shiny::div(class = "inv-kpi-value", shiny::textOutput(ns("kpi_total"), inline = TRUE)),
            shiny::div(class = "inv-kpi-sub",   shiny::textOutput(ns("kpi_total_sub"), inline = TRUE)),
            shiny::div(class = "inv-kpi-spark", echarts4r::echarts4rOutput(ns("spark_total"), height = "32px"))
          ),
          shiny::div(class = "inv-kpi-tile",
            shiny::div(class = "inv-kpi-label", "Participants"),
            shiny::div(class = "inv-kpi-value", shiny::textOutput(ns("kpi_pts"), inline = TRUE)),
            shiny::div(class = "inv-kpi-sub",   shiny::textOutput(ns("kpi_pts_sub"), inline = TRUE)),
            shiny::div(class = "inv-kpi-spark", echarts4r::echarts4rOutput(ns("spark_pts"), height = "32px"))
          ),
          shiny::div(class = "inv-kpi-tile",
            shiny::div(class = "inv-kpi-label", "Studies"),
            shiny::div(class = "inv-kpi-value", shiny::textOutput(ns("kpi_studies"), inline = TRUE)),
            shiny::div(class = "inv-kpi-sub",   shiny::textOutput(ns("kpi_studies_sub"), inline = TRUE)),
            shiny::div(class = "inv-kpi-spark", echarts4r::echarts4rOutput(ns("spark_studies"), height = "32px"))
          ),
          shiny::div(class = "inv-kpi-tile",
            shiny::div(class = "inv-kpi-label", "MDRO categories"),
            shiny::div(class = "inv-kpi-value", shiny::textOutput(ns("kpi_mdro"), inline = TRUE)),
            shiny::div(class = "inv-kpi-sub",   shiny::textOutput(ns("kpi_mdro_sub"), inline = TRUE)),
            shiny::div(class = "inv-kpi-spark", echarts4r::echarts4rOutput(ns("spark_mdro"), height = "32px"))
          )
        ),

        # Stacked-area accrual chart
        echarts4r::echarts4rOutput(ns("chart_accrual"), height = "260px")
      ),

      # ── Specimen flow ───────────────────────────────────────────────────────
      shiny::div(
        class = "inv-card",
        shiny::div(class = "inv-card-title", "Specimen flow: Site → Study → Parent specimen → MDRO → Species"),
        echarts4r::echarts4rOutput(ns("chart_sankey"), height = "360px")
      ),

      # ── Sites map ───────────────────────────────────────────────────────────
      shiny::div(
        class = "inv-card",
        shiny::div(class = "inv-card-title", "Sites map"),
        leaflet::leafletOutput(ns("map_sites"), height = "360px")
      )
    )
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

inventoryServer <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Build cleaned dataset ─────────────────────────────────────────────────
    cleaned_data <- shiny::reactive({
      cached <- prepare_inventory_cleaned(app_state$cleaned_links)
      if (nrow(cached) > 0) return(cached)

      lc  <- app_state$links_confirmed
      ov  <- app_state$cleaned_overrides
      vu  <- app_state$vitek_unique
      sp  <- app_state$specimens

      if (!is.null(lc) && nrow(lc) > 0 &&
          !is.null(vu) && nrow(vu) > 0 &&
          !is.null(sp) && nrow(sp) > 0) {
        built <- tryCatch(
          build_cleaned(lc, ov, vu, sp),
          error = function(e) {
            warning("build_cleaned() error: ", e$message)
            cleaned_empty()
          }
        )
        built <- prepare_inventory_cleaned(built)
        if (nrow(built) > 0) return(built)
      }

      conn <- app_state$db_conn
      if (is.null(conn)) return(cleaned_empty())

      from_db <- tryCatch(
        read_table(conn, "cleaned_links"),
        error = function(e) {
          warning("read cleaned_links for inventory failed: ", e$message)
          tibble::tibble()
        }
      )
      prepare_inventory_cleaned(from_db)
    })

    # ── Update filter chip selects when data changes ──────────────────────────
    shiny::observe({
      d <- cleaned_data()
      if (nrow(d) == 0) return()

      studies  <- sort(unique(na.omit(d$v_parsed_study)))
      d <- add_inventory_site_fields(d)
      sites    <- sort(unique(na.omit(d$inv_site_label)))
      specs    <- sort(unique(na.omit(d$clean_specimen_type)))
      species  <- sort(unique(na.omit(d$clean_organism)))
      statuses <- sort(unique(na.omit(d$state)))

      shiny::updateSelectInput(session, "f_study",
        choices = c("All", studies), selected = "All")
      shiny::updateSelectInput(session, "f_site",
        choices = c("All", sites),   selected = "All")
      shiny::updateSelectInput(session, "f_specimen",
        choices = c("All", specs),   selected = "All")
      shiny::updateSelectInput(session, "f_species",
        choices = c("All", species), selected = "All")
      shiny::updateSelectInput(session, "f_status",
        choices = c("All", statuses),selected = "All")

      # Date range defaults
      if ("v_testing_date" %in% names(d)) {
        dates <- d$v_testing_date
        dates <- dates[!is.na(dates)]
        if (length(dates) > 0) {
          shiny::updateDateRangeInput(session, "f_range",
            start = min(dates), end = max(dates))
        }
      }
    })

    # ── Filtered dataset ──────────────────────────────────────────────────────
    filtered <- shiny::reactive({
      d <- cleaned_data()
      if (nrow(d) == 0) return(d)
      d <- add_inventory_site_fields(d)

      # Study
      if (!is.null(input$f_study) && input$f_study != "All")
        d <- dplyr::filter(d, v_parsed_study == input$f_study)

      # Site
      if (!is.null(input$f_site) && input$f_site != "All")
        d <- dplyr::filter(d, inv_site_label == input$f_site)

      # Specimen
      if (!is.null(input$f_specimen) && input$f_specimen != "All")
        d <- dplyr::filter(d, clean_specimen_type == input$f_specimen)

      # Species
      if (!is.null(input$f_species) && input$f_species != "All")
        d <- dplyr::filter(d, clean_organism == input$f_species)

      # Status
      if (!is.null(input$f_status) && input$f_status != "All")
        d <- dplyr::filter(d, state == input$f_status)

      # Date range
      if (!is.null(input$f_range) && "v_testing_date" %in% names(d)) {
        d <- dplyr::filter(d,
          is.na(v_testing_date) |
          (v_testing_date >= input$f_range[1] & v_testing_date <= input$f_range[2]))
      }

      d
    })

    # ── KPI outputs ───────────────────────────────────────────────────────────
    output$kpi_total <- shiny::renderText({
      d <- filtered(); format(nrow(d), big.mark = ",")
    })
    output$kpi_total_sub <- shiny::renderText({
      d <- cleaned_data()
      if (nrow(filtered()) == nrow(d)) "all confirmed links"
      else sprintf("of %s total", format(nrow(d), big.mark = ","))
    })

    output$kpi_pts <- shiny::renderText({
      d <- filtered()
      n <- dplyr::n_distinct(na.omit(d$clean_participant_id))
      format(n, big.mark = ",")
    })
    output$kpi_pts_sub <- shiny::renderText("unique participants")

    output$kpi_studies <- shiny::renderText({
      d <- filtered()
      n <- dplyr::n_distinct(na.omit(d$v_parsed_study))
      format(n, big.mark = ",")
    })
    output$kpi_studies_sub <- shiny::renderText("active studies")

    output$kpi_mdro <- shiny::renderText({
      d <- filtered()
      n <- dplyr::n_distinct(na.omit(d$clean_mdro_category))
      format(n, big.mark = ",")
    })
    output$kpi_mdro_sub <- shiny::renderText("MDRO categories")

    # ── Sparklines ────────────────────────────────────────────────────────────
    .spark <- function(series_vec, color = "#1f3a5f") {
      if (length(series_vec) == 0 || all(is.na(series_vec)))
        return(echarts4r::e_charts() |> echarts4r::e_line(smooth = TRUE))

      df <- tibble::tibble(x = seq_along(series_vec), y = series_vec)
      df |>
        echarts4r::e_charts(x) |>
        echarts4r::e_line(y, smooth = TRUE, symbol = "none",
                          lineStyle = list(color = color, width = 2),
                          areaStyle = list(color = color, opacity = 0.12)) |>
        echarts4r::e_x_axis(show = FALSE) |>
        echarts4r::e_y_axis(show = FALSE) |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_tooltip(show = FALSE) |>
        echarts4r::e_grid(top = 0, bottom = 0, left = 0, right = 0)
    }

    # Daily cumulative counts for sparklines (last 90 days)
    .spark_series <- function(d, group_col = NULL) {
      if (nrow(d) == 0 || !"v_testing_date" %in% names(d))
        return(integer(0))

      d2 <- d |>
        dplyr::filter(!is.na(v_testing_date)) |>
        dplyr::mutate(wk = lubridate::floor_date(v_testing_date, "week"))

      if (!is.null(group_col))
        d2 <- dplyr::distinct(d2, wk, .data[[group_col]])

      d2 |>
        dplyr::count(wk) |>
        dplyr::arrange(wk) |>
        dplyr::pull(n)
    }

    output$spark_total   <- echarts4r::renderEcharts4r(
      .spark(.spark_series(filtered()), "#1f3a5f"))
    output$spark_pts     <- echarts4r::renderEcharts4r(
      .spark(.spark_series(filtered(), "clean_participant_id"), "#15803d"))
    output$spark_studies <- echarts4r::renderEcharts4r(
      .spark(.spark_series(filtered(), "v_parsed_study"), "#0891b2"))
    output$spark_mdro    <- echarts4r::renderEcharts4r(
      .spark(.spark_series(filtered(), "clean_mdro_category"), "#7c3aed"))

    # ── Stacked-area accrual chart ────────────────────────────────────────────
    output$chart_accrual <- echarts4r::renderEcharts4r({
      d <- filtered()
      if (nrow(d) == 0 || !"v_testing_date" %in% names(d)) {
        return(echarts4r::e_charts() |>
          echarts4r::e_title(subtext = "No data — load Vitek files and link specimens"))
      }

      # Daily counts by MDRO category
      daily <- d |>
        dplyr::filter(!is.na(v_testing_date), !is.na(clean_mdro_category)) |>
        dplyr::mutate(date = v_testing_date) |>
        dplyr::count(date, mdro = clean_mdro_category) |>
        dplyr::arrange(date)

      if (nrow(daily) == 0) {
        return(echarts4r::e_charts() |>
          echarts4r::e_title(subtext = "No dated records"))
      }

      # Widen: one column per MDRO
      all_mdros <- sort(unique(daily$mdro))
      date_seq  <- seq(min(daily$date), max(daily$date), by = "day")

      wide <- tibble::tibble(date = date_seq) |>
        dplyr::left_join(
          tidyr::pivot_wider(daily, names_from = mdro, values_from = n, values_fill = 0L),
          by = "date"
        ) |>
        dplyr::mutate(dplyr::across(where(is.integer), ~ tidyr::replace_na(., 0L)))

      # Build chart iteratively — add one area series per MDRO
      chart <- wide |>
        echarts4r::e_charts(date) |>
        echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "cross")) |>
        echarts4r::e_x_axis(type = "time") |>
        echarts4r::e_y_axis(name = "specimens / day", nameTextStyle = list(fontSize = 11)) |>
        echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_grid(top = 10, bottom = 60, left = 50, right = 20)

      for (mdro_cat in all_mdros) {
        if (!mdro_cat %in% names(wide)) next
        col <- MDRO_COLORS[mdro_cat]
        if (is.na(col)) col <- "#9ca3af"
        chart <- chart |>
          echarts4r::e_area_(
            serie      = mdro_cat,
            stack      = "total",
            smooth     = TRUE,
            symbol     = "none",
            itemStyle  = list(color = col),
            areaStyle  = list(color = col, opacity = 0.75),
            lineStyle  = list(color = col, width = 0)
          )
      }

      chart
    })

    # ── Sankey: site → study → parent specimen → MDRO → species ─────────────
    output$chart_sankey <- echarts4r::renderEcharts4r({
      d <- filtered()
      if (nrow(d) == 0) {
        return(echarts4r::e_charts() |>
          echarts4r::e_title(subtext = "No data"))
      }

      d_flow <- d |>
        dplyr::mutate(
          flow_site = display_flow_site(inv_site_label, project_id, cp_short_title),
          flow_study = purrr::pmap_chr(
            list(clean_participant_id, v_parsed_subject, lab_id,
                 v_parsed_study, clean_cp_title, cp_short_title, project_id),
            display_flow_study
          ),
          flow_parent = display_flow_parent(clean_parent_specimen_type),
          flow_mdro = normalize_flow_mdro(
            clean_mdro_category,
            os_mdro = o_custom_mdro,
            disagree = mdro_disagree
          ),
          flow_species = clean_organism
        ) |>
        dplyr::mutate(
          flow_site = dplyr::na_if(trimws(as.character(flow_site)), ""),
          flow_study = dplyr::na_if(trimws(as.character(flow_study)), ""),
          flow_parent = dplyr::coalesce(
            dplyr::na_if(trimws(as.character(flow_parent)), ""),
            "Parent specimen unspecified"
          ),
          flow_mdro = dplyr::coalesce(
            dplyr::na_if(trimws(as.character(flow_mdro)), ""),
            "MDRO unspecified"
          ),
          flow_species = dplyr::coalesce(
            dplyr::na_if(trimws(as.character(flow_species)), ""),
            "Species unspecified"
          )
        ) |>
        dplyr::filter(!is.na(flow_site), !is.na(flow_study))

      edges <- dplyr::bind_rows(
        d_flow |>
          dplyr::transmute(source = paste0("site:", flow_site),
                           target = paste0("study:", flow_study)),
        d_flow |>
          dplyr::transmute(source = paste0("study:", flow_study),
                           target = paste0("parent:", flow_parent)),
        d_flow |>
          dplyr::transmute(source = paste0("parent:", flow_parent),
                           target = paste0("mdro:", flow_mdro)),
        d_flow |>
          dplyr::transmute(source = paste0("mdro:", flow_mdro),
                           target = paste0("species:", flow_species))
      ) |>
        dplyr::count(source, target, name = "value")

      if (nrow(edges) == 0) {
        return(echarts4r::e_charts() |>
          echarts4r::e_title(subtext = "Insufficient data for Sankey"))
      }

      # Keep layer prefixes in node IDs so identical labels in different columns
      # do not collapse into one ECharts node. Strip prefixes only for display.
      strip_prefix_js <- htmlwidgets::JS(
        "function(params) {",
        "  var name = params.name || '';",
        "  var label = name.replace(/^(site:|study:|parent:|mdro:|species:)/, '');",
        "  if (params.value !== undefined && params.value !== null) {",
        "    return label + '\\n' + params.value;",
        "  }",
        "  return label;",
        "}"
      )
      tooltip_js <- htmlwidgets::JS(
        "function(params) {",
        "  function clean(x) { return String(x || '').replace(/^(site:|study:|parent:|mdro:|species:)/, ''); }",
        "  if (params.data && params.data.source && params.data.target) {",
        "    return clean(params.data.source) + ' → ' + clean(params.data.target) + '<br/>' + params.data.value + ' specimens';",
        "  }",
        "  return clean(params.name);",
        "}"
      )

      chart <- edges |>
        echarts4r::e_charts() |>
        echarts4r::e_sankey(
          source, target, value,
          layout      = "horizontal",
          nodeWidth   = 12,
          nodePadding = 10,
          layoutIterations = 64,
          emphasis    = list(focus = "adjacency"),
          label       = list(fontSize = 11, color = "#374151", formatter = strip_prefix_js),
          itemStyle   = list(borderWidth = 0),
          lineStyle   = list(color = "source", opacity = 0.68, curveness = 0.55)
        ) |>
        echarts4r::e_tooltip(trigger = "item", formatter = tooltip_js) |>
        echarts4r::e_grid(top = 10, bottom = 10, left = 20, right = 20)

      # echarts4r builds Sankey links via apply(), which coerces value to
      # character. ECharts needs numeric values for visible ribbon widths.
      chart$x$opts$series[[1]]$links <- purrr::map(
        chart$x$opts$series[[1]]$links,
        function(link) {
          link$value <- as.numeric(link$value)
          link
        }
      )
      node_values <- dplyr::full_join(
        edges |>
          dplyr::group_by(name = source) |>
          dplyr::summarise(out_value = sum(value), .groups = "drop"),
        edges |>
          dplyr::group_by(name = target) |>
          dplyr::summarise(in_value = sum(value), .groups = "drop"),
        by = "name"
      ) |>
        dplyr::mutate(value = pmax(
          tidyr::replace_na(out_value, 0L),
          tidyr::replace_na(in_value, 0L)
        ))
      chart$x$opts$series[[1]]$data <- purrr::map(
        chart$x$opts$series[[1]]$data,
        function(node) {
          node$value <- node_values$value[match(node$name, node_values$name)]
          node
        }
      )
      chart
    })

    # ── Sites map ─────────────────────────────────────────────────────────────
    output$map_sites <- leaflet::renderLeaflet({
      d <- filtered()
      d <- add_inventory_site_fields(d)

      # Build site summary
      site_counts <- d |>
        dplyr::filter(!is.na(inv_site_code)) |>
        dplyr::count(inv_site_code, inv_site_label, inv_site_city, inv_site_state,
                     inv_site_address, inv_site_lat, inv_site_lon,
                     name = "n_specimens") |>
        dplyr::mutate(
          n_pts = purrr::map_int(inv_site_code, function(site) {
            rows <- d[d$inv_site_code == site & !is.na(d$clean_participant_id), ]
            dplyr::n_distinct(rows$clean_participant_id)
          })
        )

      map <- leaflet::leaflet() |>
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.Positron,
          options = leaflet::providerTileOptions(minZoom = 3, maxZoom = 14)
        )

      if (!is.null(site_counts) && nrow(site_counts) > 0) {
        map <- map |>
          leaflet::addCircleMarkers(
            data        = site_counts,
            lat         = ~inv_site_lat, lng = ~inv_site_lon,
            radius      = ~pmax(6, log(n_specimens + 1) * 4),
            color       = "#1f3a5f", fillColor = "#1f3a5f",
            fillOpacity = 0.65, weight = 1.5, opacity = 0.9,
            popup = ~sprintf(
              "<b>%s</b><br>%s, %s<br>%s<br>%s specimens<br>%s participants",
              inv_site_label, inv_site_city, inv_site_state, inv_site_address,
              format(n_specimens, big.mark=","),
              format(n_pts, big.mark=",")
            ),
            label = ~inv_site_label
          ) |>
          leaflet::fitBounds(
            lng1 = min(site_counts$inv_site_lon) - 0.5,
            lat1 = min(site_counts$inv_site_lat) - 0.5,
            lng2 = max(site_counts$inv_site_lon) + 0.5,
            lat2 = max(site_counts$inv_site_lat) + 0.5
          )
      } else {
        map <- map |>
          leaflet::setView(lng = -84.4, lat = 33.75, zoom = 7)
      }

      map
    })

  })
}

# ── Private helpers ───────────────────────────────────────────────────────────

#' Filter chip: label + select input wrapped in pill styling.
.inv_chip <- function(input_id, label_text, choices = c("All")) {
  shiny::div(
    class = "inv-chip-wrap",
    shiny::span(class = "inv-chip-label", label_text),
    shiny::selectInput(
      inputId  = input_id,
      label    = NULL,
      choices  = choices,
      selected = "All",
      width    = "auto"
    )
  )
}

inventory_site_dictionary <- function() {
  getOption("axis.site_dictionary", .INV_SITE_DICTIONARY)
}

prepare_inventory_cleaned <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(cleaned_empty())

  d <- tibble::as_tibble(d)
  empty <- cleaned_empty()

  for (col in setdiff(names(empty), names(d))) {
    d[[col]] <- empty[[col]][NA_integer_]
  }

  # Older cleaned exports were written before parent specimen was first-class.
  if (all(is.na(d$clean_parent_specimen_type)) || all(trimws(as.character(d$clean_parent_specimen_type)) == "", na.rm = TRUE)) {
    d$clean_parent_specimen_type <- dplyr::coalesce(
      dplyr::na_if(as.character(d$o_parent_specimen_type), ""),
      dplyr::na_if(as.character(d$clean_specimen_type), "")
    )
  }

  d |>
    dplyr::mutate(
      v_testing_date = as.Date(v_testing_date),
      v_collection_date = as.Date(v_collection_date),
      clean_testing_date = as.Date(clean_testing_date),
      o_custom_collection_date = as.Date(o_custom_collection_date)
    )
}

add_inventory_site_fields <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(d)

  d <- d |>
    dplyr::mutate(
      .site_prefix = purrr::pmap_chr(
        list(clean_participant_id, v_parsed_subject, lab_id, v_parsed_study, cp_short_title),
        derive_site_code
      )
    )

  site_dict <- inventory_site_dictionary() |>
    dplyr::rename(
      dict_site_label = site_label,
      dict_city = city,
      dict_state = state,
      dict_address = address,
      dict_lat = lat,
      dict_lon = lon
    )

  d |>
    dplyr::left_join(site_dict, by = c(".site_prefix" = "site_code")) |>
    dplyr::mutate(
      inv_site_code = .site_prefix,
      inv_site_label = dplyr::coalesce(dict_site_label, project_id, cp_short_title),
      inv_site_city = dict_city,
      inv_site_state = dict_state,
      inv_site_address = dict_address,
      inv_site_lat = dict_lat,
      inv_site_lon = dict_lon
    ) |>
    dplyr::select(-dplyr::any_of(c(".site_prefix", "dict_site_label", "dict_city",
                                   "dict_state", "dict_address", "dict_lat",
                                   "dict_lon")))
}

derive_site_code <- function(participant_id, parsed_subject, lab_id,
                             parsed_study, cp_short_title) {
  ids <- c(participant_id, parsed_subject, lab_id)
  ids <- ids[!is.na(ids) & ids != ""]
  id_upper <- toupper(ids)

  for (id in id_upper) {
    prefix <- site_prefix_from_id(id)
    if (!is.na(prefix)) return(prefix)
  }

  study <- toupper(paste(na.omit(c(parsed_study, cp_short_title)), collapse = " "))
  if (grepl("ARRRRG", study)) return("ARRRRG_ATL")
  if (grepl("SNT|SENTINEL|APPS|REACT", study)) return("SENTINEL_REACT_DECATUR")
  NA_character_
}

site_prefix_from_id <- function(id) {
  id <- toupper(trimws(as.character(id)))
  if (is.na(id) || id == "") return(NA_character_)

  # REACT/APPS IDs often carry the true site in character positions 2-3,
  # e.g. aEM037, rML012, bAG014, cSP009.
  if (nchar(id) >= 3) {
    mid <- substr(id, 2, 3)
    if (mid %in% c("EM", "AG", "ML", "SP")) return(mid)
  }

  # Also allow IDs that begin directly with the two-letter site code.
  first <- substr(id, 1, 2)
  if (first %in% c("EM", "AG", "ML", "SP")) return(first)

  NA_character_
}

display_flow_site <- function(site_label, project_id = NULL, cp_short_title = NULL) {
  n <- length(site_label)
  if (is.null(project_id)) project_id <- rep(NA_character_, n)
  if (is.null(cp_short_title)) cp_short_title <- rep(NA_character_, n)

  raw <- dplyr::coalesce(
    dplyr::na_if(trimws(as.character(site_label)), ""),
    dplyr::na_if(trimws(as.character(project_id)), ""),
    dplyr::na_if(trimws(as.character(cp_short_title)), "")
  )
  upper <- toupper(raw)

  out <- raw
  out[!is.na(upper) & grepl("^FAIR(_OUTPUT)?$|^FAIR\\b|FAIR618", upper)] <- "FAIR"
  out[!is.na(upper) & grepl("EMORY LONG TERM|LONG TERM ACUTE|ELTAC", upper)] <- "ELTAC"
  out[!is.na(upper) & grepl("A\\.?G\\.? RHODES|AG RHODES|A G RHODES", upper)] <- "A.G. Rhodes"
  out <- sub("_output$", "", out, ignore.case = TRUE)
  out
}

display_flow_study <- function(participant_id, parsed_subject, lab_id,
                               parsed_study, clean_cp_title,
                               cp_short_title, project_id) {
  ids <- toupper(trimws(as.character(c(participant_id, parsed_subject, lab_id))))
  ids <- ids[!is.na(ids) & ids != ""]
  text <- toupper(paste(na.omit(c(parsed_study, clean_cp_title,
                                  cp_short_title, project_id)), collapse = " "))

  if (grepl("FAIR618|\\bFAIR\\b|FAIR_OUTPUT", text)) return("FAIR")

  if (length(ids) > 0) {
    if (any(grepl("^R(EM|AG|ML|SP)", ids))) return("REACT")
    if (any(grepl("^[A-QS-Z](EM|AG|ML|SP)", ids))) return("APPS")
    if (any(grepl("^APPS", ids))) return("APPS")
    if (any(grepl("^SNT", ids))) return("Sentinel REACT")
  }

  if (grepl("\\bAPPS\\b", text)) return("APPS")
  if (grepl("\\bSNT\\b|SENTINEL", text)) return("Sentinel REACT")
  if (grepl("\\bREACT\\b", text)) return("REACT")

  fallback <- dplyr::coalesce(
    dplyr::na_if(trimws(as.character(parsed_study)), ""),
    dplyr::na_if(trimws(as.character(clean_cp_title)), ""),
    dplyr::na_if(trimws(as.character(cp_short_title)), ""),
    dplyr::na_if(trimws(as.character(project_id)), ""),
    "Study unspecified"
  )
  sub("_output$", "", fallback, ignore.case = TRUE)
}

display_flow_parent <- function(parent_type) {
  x <- trimws(as.character(parent_type))
  x[x %in% c("", "NA", "N/A", "na", "n/a")] <- NA_character_
  x[!is.na(x) & toupper(x) == "CRYOPRESERVED CELLS"] <- "Isolates"
  x
}

normalize_flow_mdro <- function(x, os_mdro = NULL, disagree = NULL) {
  clean <- canonical_flow_mdro(x)
  os <- if (is.null(os_mdro)) rep(NA_character_, length(clean)) else canonical_flow_mdro(os_mdro)
  if (is.null(disagree)) disagree <- rep(FALSE, length(clean))
  disagree <- !is.na(disagree) & disagree

  mismatch <- disagree & !is.na(clean) & !is.na(os) & clean != os
  clean[mismatch] <- paste0("MDRO mismatch: ", clean[mismatch], " vs ", os[mismatch])
  clean
}

canonical_flow_mdro <- function(x) {
  raw <- trimws(as.character(x))
  raw[raw %in% c("", "NA", "N/A", "na", "n/a")] <- NA_character_
  upper <- toupper(raw)

  result <- rep(NA_character_, length(raw))
  result[!is.na(upper) & grepl("^(NON[- ]?MDRO|NONE|NO MDRO|NEGATIVE)$", upper)] <- "Non-MDRO"
  result[!is.na(upper) & grepl("^(POSITIVE|MDRO POSITIVE)$", upper)] <- "MDRO positive (unspecified)"

  token_map <- c(
    ESBL = "ESBL",
    CRE = "CRE",
    CRKP = "CRE",
    CREC = "CRE",
    VRE = "VRE",
    MRSA = "MRSA",
    CRAB = "CRAB",
    CRPA = "CRPA",
    MDRP = "MDRP",
    MDRA = "MDRA"
  )

  remaining <- which(is.na(result) & !is.na(upper))
  for (i in remaining) {
    for (tok in names(token_map)) {
      if (grepl(tok, upper[[i]], fixed = TRUE)) {
        result[[i]] <- token_map[[tok]]
        break
      }
    }
    if (is.na(result[[i]]) &&
        grepl("MULTIDRUG[- ]RESISTANT PSEUDOMONAS|MDR PSEUDOMONAS", upper[[i]])) {
      result[[i]] <- "MDRP"
    }
  }

  dplyr::coalesce(result, raw)
}
