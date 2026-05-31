# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/data_export_cfu.R — Culture / CFU CSV exports
# ─────────────────────────────────────────────────────────────────────────────

.cfu_export_slug <- function(x) {
  x <- if (is.null(x) || !nzchar(as.character(x))) {
    format(Sys.time(), "%Y%m%d%H%M%S")
  } else {
    as.character(x)
  }
  gsub("[^A-Za-z0-9_-]+", "_", x)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.cfu_exportable_df <- function(df) {
  df <- tibble::as_tibble(df)
  list_cols <- vapply(df, is.list, logical(1))
  for (nm in names(df)[list_cols]) {
    df[[nm]] <- vapply(df[[nm]], function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      paste(as.character(x), collapse = "; ")
    }, character(1))
  }
  df
}

.cfu_export_blank <- function(x) {
  x <- trimws(as.character(x))
  is.na(x) | x %in% c("", "NA", "na", "N/A", "n/a", "NULL", "null")
}

.cfu_export_status <- function(df) {
  is_pseudo <- dplyr::coalesce(as.logical(df$is_pseudocount), FALSE)
  censored <- dplyr::coalesce(as.logical(df$cfu_censored), FALSE)
  flag <- as.character(df$cfu_flag)
  dplyr::case_when(
    is_pseudo ~ "pseudocount",
    flag == "unparseable" ~ "unparseable",
    flag == "renormalized" | censored ~ "normalized",
    !is.na(flag) & nzchar(flag) ~ "review",
    TRUE ~ "parsed"
  )
}

.cfu_export_issue <- function(df) {
  status <- .cfu_export_status(df)
  flag <- as.character(df$cfu_flag)
  unit <- as.character(df$cfu_unit)
  censored <- dplyr::coalesce(as.logical(df$cfu_censored), FALSE)
  is_pseudo <- dplyr::coalesce(as.logical(df$is_pseudocount), FALSE)

  dplyr::case_when(
    is_pseudo ~ "Growth flag without quantitative value; cohort-floor pseudocount",
    flag == "unparseable" ~ "Could not parse CFU value",
    flag == "renormalized" ~ "Mantissa >= 10 renormalized",
    flag == "ambiguous_bare" ~ "Bare value; unit and scale assumed",
    flag == "below_floor" ~ "Value below plausible floor",
    censored ~ "Right-censored value",
    !is.na(unit) & unit != "CFU/mL" ~ "Non-mL unit retained separately",
    status == "parsed" ~ "Parsed cleanly",
    TRUE ~ "Review recommended"
  )
}

.cfu_export_base <- function(specimens) {
  if (is.null(specimens) || nrow(specimens) == 0) {
    return(tibble::tibble())
  }
  df <- tibble::as_tibble(specimens)
  required <- c(
    "source_file", "source_row", "project_id", "cp_short_title",
    "os_identifier", "specimen_label", "participant_id", "custom_day",
    "type", "lineage", "parent_label", "location_container", "location_row",
    "location_col", "location_pos",
    "custom_collection_date", "collection_dt", "custom_selective_media",
    "custom_site", "anatomic_site", "custom_organism", "custom_mdro", "cfu_raw", "cfu_log10", "cfu_value",
    "cfu_unit", "cfu_censored", "growth_method", "is_pseudocount",
    "cfu_flag", "has_quant"
  )
  for (col in setdiff(required, names(df))) df[[col]] <- NA
  out <- df |>
    dplyr::mutate(
      custom_day = dplyr::na_if(trimws(as.character(.data$custom_day)), ""),
      custom_site = dplyr::na_if(trimws(as.character(.data$custom_site)), ""),
      anatomic_site = dplyr::na_if(trimws(as.character(.data$anatomic_site)), ""),
      time_point_date = dplyr::coalesce(as.Date(.data$custom_collection_date), as.Date(.data$collection_dt)),
      time_point_label = dplyr::coalesce(.data$custom_day, as.character(.data$time_point_date), "—"),
      site = dplyr::coalesce(.data$custom_site, .data$anatomic_site, .data$cp_short_title, .data$project_id),
      has_quant = dplyr::coalesce(as.logical(.data$has_quant), FALSE),
      is_pseudocount = dplyr::coalesce(as.logical(.data$is_pseudocount), FALSE),
      cfu_censored = dplyr::coalesce(as.logical(.data$cfu_censored), FALSE)
    )
  out$cfu_status <- .cfu_export_status(out)
  out$cfu_issue <- .cfu_export_issue(out)
  out
}

.cfu_mdro_flags <- function(x) {
  raw <- toupper(trimws(as.character(x)))
  raw[raw %in% c("", "NA", "N/A", "NULL", "NONE", "NO", "NEGATIVE", "NON-MDRO", "NON MDRO", "NO MDRO")] <- NA_character_
  cre <- !is.na(raw) & grepl("CRE|CRKP|CREC", raw)
  esbl <- !is.na(raw) & grepl("ESBL|ESCRE", raw)
  vre <- !is.na(raw) & grepl("VRE", raw)
  mdrp <- !is.na(raw) & grepl("MDRP|CRPA|MULTIDRUG[- ]RESISTANT PSEUDOMONAS|MDR PSEUDOMONAS", raw)
  mdro <- !is.na(raw) & (
    cre | esbl | vre | mdrp |
      grepl("MDRA|CRAB|MRSA|MDRO|POSITIVE|YES", raw)
  )
  tibble::tibble(
    mdro_positive = as.integer(mdro),
    cre_positive = as.integer(cre),
    esbl_positive = as.integer(esbl),
    vre_positive = as.integer(vre),
    mdrp_positive = as.integer(mdrp)
  )
}

prepare_cfu_review_export <- function(specimens, batch_id = NULL) {
  df <- .cfu_export_base(specimens)
  if (nrow(df) == 0) return(tibble::tibble())

  df |>
    dplyr::filter(
      .data$has_quant |
        !.cfu_export_blank(.data$cfu_raw) |
        !is.na(.data$cfu_flag) |
        .data$cfu_censored |
        .data$is_pseudocount
    ) |>
    dplyr::filter(.data$cfu_status != "parsed" | .data$cfu_unit != "CFU/mL") |>
    dplyr::transmute(
      batch_id = batch_id %||% NA_character_,
      raw = .data$cfu_raw,
      project = dplyr::coalesce(.data$cp_short_title, .data$project_id),
      source_file = .data$source_file,
      source_row = .data$source_row,
      os_identifier = .data$os_identifier,
      specimen = .data$specimen_label,
      participant_id = .data$participant_id,
      time_point = .data$time_point_label,
      time_point_date = .data$time_point_date,
      media = .data$custom_selective_media,
      organism = .data$custom_organism,
      mdro = .data$custom_mdro,
      parsed = .data$cfu_value,
      log10_cfu = .data$cfu_log10,
      unit = .data$cfu_unit,
      censored = .data$cfu_censored,
      method = .data$growth_method,
      is_pseudocount = .data$is_pseudocount,
      issue = .data$cfu_issue,
      status = .data$cfu_status
    ) |>
    dplyr::arrange(.data$project, .data$participant_id, .data$time_point_date, .data$specimen)
}

prepare_cfu_summary_export <- function(specimens, batch_id = NULL) {
  df <- .cfu_export_base(specimens)
  if (nrow(df) == 0) return(tibble::tibble())

  quant <- df |>
    dplyr::filter(.data$has_quant)
  if (nrow(quant) == 0) return(tibble::tibble())

  quant |>
    dplyr::group_by(
      project = dplyr::coalesce(.data$cp_short_title, .data$project_id),
      participant_id = .data$participant_id,
      time_point = .data$time_point_label,
      time_point_date = .data$time_point_date
    ) |>
    dplyr::summarise(
      batch_id = batch_id %||% NA_character_,
      n_quant_rows = dplyr::n(),
      n_specimens = dplyr::n_distinct(.data$os_identifier, na.rm = TRUE),
      n_dp = sum(.data$growth_method == "DP", na.rm = TRUE),
      n_eb = sum(.data$growth_method == "EB" | .data$is_pseudocount, na.rm = TRUE),
      n_censored = sum(.data$cfu_censored, na.rm = TRUE),
      n_flagged = sum(!is.na(.data$cfu_flag), na.rm = TRUE),
      median_log10_cfu_ml_dp = {
        vals <- .data$cfu_log10[
          .data$cfu_unit == "CFU/mL" &
            .data$growth_method != "EB" &
            !.data$is_pseudocount
        ]
        if (length(stats::na.omit(vals)) == 0) NA_real_ else stats::median(vals, na.rm = TRUE)
      },
      units = paste(sort(unique(stats::na.omit(.data$cfu_unit))), collapse = "; "),
      media = paste(sort(unique(stats::na.omit(.data$custom_selective_media))), collapse = "; "),
      organisms = paste(sort(unique(stats::na.omit(.data$custom_organism))), collapse = "; "),
      mdro = paste(sort(unique(stats::na.omit(.data$custom_mdro))), collapse = "; "),
      specimens = paste(sort(unique(stats::na.omit(.data$specimen_label))), collapse = "; "),
      .groups = "drop"
    ) |>
    dplyr::select(
      batch_id, project, participant_id, time_point, time_point_date,
      dplyr::everything()
    ) |>
    dplyr::mutate(
      median_cfu_ml_dp = dplyr::if_else(
        is.na(.data$median_log10_cfu_ml_dp),
        NA_real_,
        10^.data$median_log10_cfu_ml_dp
      ),
      .after = "median_log10_cfu_ml_dp"
    ) |>
    dplyr::arrange(.data$project, .data$participant_id, .data$time_point_date, .data$time_point)
}

prepare_cfu_specimen_export <- function(specimens, batch_id = NULL) {
  df <- .cfu_export_base(specimens)
  if (nrow(df) == 0) return(tibble::tibble())

  flags <- .cfu_mdro_flags(df$custom_mdro)
  dplyr::bind_cols(df, flags) |>
    dplyr::mutate(
      .axis_row_order = dplyr::row_number(),
      has_cfu_value = !is.na(.data$cfu_log10) | .data$is_pseudocount
    ) |>
    dplyr::arrange(.data$os_identifier, dplyr::desc(.data$has_cfu_value), .data$.axis_row_order) |>
    dplyr::distinct(.data$os_identifier, .keep_all = TRUE) |>
    dplyr::transmute(
      batch_id = batch_id %||% NA_character_,
      project = dplyr::coalesce(.data$cp_short_title, .data$project_id),
      source_file = .data$source_file,
      source_row = .data$source_row,
      os_identifier = .data$os_identifier,
      specimen_label = .data$specimen_label,
      participant_id = .data$participant_id,
      visit_day = .data$time_point_label,
      visit_date = .data$time_point_date,
      site = .data$site,
      anatomic_site = .data$anatomic_site,
      storage_container = .data$location_container,
      storage_row = .data$location_row,
      storage_column = .data$location_col,
      storage_position = .data$location_pos,
      specimen_type = .data$type,
      lineage = .data$lineage,
      parent_specimen_label = .data$parent_label,
      organism = .data$custom_organism,
      mdro_category = .data$custom_mdro,
      mdro_positive = .data$mdro_positive,
      cre_positive = .data$cre_positive,
      esbl_positive = .data$esbl_positive,
      vre_positive = .data$vre_positive,
      mdrp_positive = .data$mdrp_positive,
      selective_media = .data$custom_selective_media,
      cfu_raw = .data$cfu_raw,
      cfu_value = .data$cfu_value,
      cfu_log10 = .data$cfu_log10,
      cfu_unit = .data$cfu_unit,
      cfu_censored = .data$cfu_censored,
      growth_method = .data$growth_method,
      is_pseudocount = .data$is_pseudocount,
      cfu_flag = .data$cfu_flag,
      has_quant = .data$has_quant
    ) |>
    dplyr::arrange(.data$project, .data$participant_id, .data$visit_date, .data$specimen_label)
}

write_cfu_csv_exports <- function(specimens, batch_id = NULL,
                                  output_dir = file.path("data", "exports")) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  slug <- .cfu_export_slug(batch_id)
  review <- prepare_cfu_review_export(specimens, batch_id = slug)
  summary <- prepare_cfu_summary_export(specimens, batch_id = slug)
  specimen <- prepare_cfu_specimen_export(specimens, batch_id = slug)
  review_path <- file.path(output_dir, paste0("cfu_review_", slug, ".csv"))
  summary_path <- file.path(output_dir, paste0("cfu_summary_", slug, ".csv"))
  specimen_path <- file.path(output_dir, paste0("cfu_specimen_", slug, ".csv"))

  readr::write_csv(.cfu_exportable_df(review), review_path, na = "")
  readr::write_csv(.cfu_exportable_df(summary), summary_path, na = "")
  readr::write_csv(.cfu_exportable_df(specimen), specimen_path, na = "")

  list(
    review_path = review_path,
    summary_path = summary_path,
    specimen_path = specimen_path,
    n_review = nrow(review),
    n_summary = nrow(summary),
    n_specimen = nrow(specimen)
  )
}

write_cfu_review_csv <- function(specimens, batch_id = NULL,
                                 output_dir = file.path("data", "exports")) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  slug <- .cfu_export_slug(batch_id)
  review <- prepare_cfu_review_export(specimens, batch_id = slug)
  path <- file.path(output_dir, paste0("cfu_review_", slug, ".csv"))
  readr::write_csv(.cfu_exportable_df(review), path, na = "")
  list(path = path, n = nrow(review))
}

write_cfu_summary_csv <- function(specimens, batch_id = NULL,
                                  output_dir = file.path("data", "exports")) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  slug <- .cfu_export_slug(batch_id)
  summary <- prepare_cfu_summary_export(specimens, batch_id = slug)
  path <- file.path(output_dir, paste0("cfu_summary_", slug, ".csv"))
  readr::write_csv(.cfu_exportable_df(summary), path, na = "")
  list(path = path, n = nrow(summary))
}

write_cfu_specimen_csv <- function(specimens, batch_id = NULL,
                                   output_dir = file.path("data", "exports")) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  slug <- .cfu_export_slug(batch_id)
  specimen <- prepare_cfu_specimen_export(specimens, batch_id = slug)
  path <- file.path(output_dir, paste0("cfu_specimen_", slug, ".csv"))
  readr::write_csv(.cfu_exportable_df(specimen), path, na = "")
  list(path = path, n = nrow(specimen))
}
