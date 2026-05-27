# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/data_clean.R  — Cleaned dataset builder
# HANDOFF.md §4 build_cleaned() definition
#
# Pipeline:
#   1. Join links_confirmed × vitek_unique on (lab_id, isolate_number)
#   2. Join ×specimens on os_identifier
#   3. Pivot cleaned_overrides → wide (latest win per link_id × field)
#   4. Coalesce: override → vitek_value → os_value → NA
#
# Output schema (one row per confirmed link):
#   link_id, lab_id, isolate_number, os_identifier, project_id, specimen_label,
#   cp_short_title, confidence, match_method, state, batch_id,
#   -- Vitek fields (prefixed v_) --
#   v_organism, v_specimen_type, v_specimen_source, v_collection_date,
#   v_testing_date, v_parsed_study, v_parsed_subject, v_parsed_target,
#   v_cp_hint, v_n_drugs, v_file_name,
#   -- OS fields (prefixed o_) --
#   o_participant_id, o_custom_collection_date, o_custom_organism, o_custom_mdro,
#   o_parent_specimen_type, o_class, o_type, o_lineage,
#   -- Cleaned (authoritative) --
#   clean_lab_id, clean_organism, clean_specimen_type, clean_mdro_category,
#   clean_testing_date, clean_participant_id, clean_cp_title,
#   clean_parent_specimen_type, clean_ast_notes,
#   -- Flags --
#   has_edit, n_edits, mdro_disagree
# ─────────────────────────────────────────────────────────────────────────────

#' Build the cleaned dataset from confirmed links, Vitek data, OS specimens,
#' and field-level overrides.
#'
#' @param links     links_confirmed tibble  (from read_table(conn,"links_confirmed"))
#' @param overrides cleaned_overrides tibble (from read_table(conn,"cleaned_overrides"))
#' @param vitek     vitek_unique tibble      (from dedup_vitek())
#' @param specimens specimens tibble         (from parse_os_specimens_multi())
#' @return Cleaned tibble (one row per confirmed link). Returns cleaned_empty()
#'         if any of links/vitek/specimens is NULL or empty.
build_cleaned <- function(links, overrides, vitek, specimens) {

  # ── Guard ─────────────────────────────────────────────────────────────────
  if (is.null(links)     || nrow(links)     == 0 ||
      is.null(vitek)     || nrow(vitek)     == 0 ||
      is.null(specimens) || nrow(specimens) == 0) {
    return(cleaned_empty())
  }

  # ── Step 1: links × vitek ─────────────────────────────────────────────────
  joined <- links |>
    dplyr::left_join(
      vitek |>
        dplyr::select(
          lab_id, isolate_number,
          v_organism       = organism_name,
          v_specimen_type  = specimen_type,
          v_specimen_source= specimen_source,
          v_collection_date= collection_date,
          v_testing_date   = testing_date,
          v_parsed_study   = parsed_study,
          v_parsed_subject = parsed_subject,
          v_parsed_target  = parsed_target,
          v_cp_hint        = cp_hint,
          v_n_drugs        = n_drugs,
          v_file_name      = file_name
        ),
      by = c("lab_id", "isolate_number")
    )

  # ── Step 2: × specimens ───────────────────────────────────────────────────
  joined <- joined |>
    dplyr::left_join(
      specimens |>
        dplyr::select(
          os_identifier,
          o_participant_id          = participant_id,
          o_custom_collection_date  = custom_collection_date,
          o_custom_organism         = custom_organism,
          o_custom_mdro             = custom_mdro,
          o_parent_specimen_type     = dplyr::any_of("custom_parent_specimen_type"),
          o_class                   = dplyr::any_of("class"),
          o_type                    = dplyr::any_of("type"),
          o_lineage                 = dplyr::any_of("lineage")
        ),
      by = "os_identifier"
    )

  # ── Step 3: pivot overrides → wide (latest per link_id × field) ───────────
  ov_wide <- if (!is.null(overrides) && nrow(overrides) > 0) {
    overrides |>
      dplyr::arrange(dplyr::desc(edited_at)) |>
      dplyr::distinct(link_id, field, .keep_all = TRUE) |>
      tidyr::pivot_wider(
        id_cols     = link_id,
        names_from  = field,
        values_from = cleaned_value,
        names_prefix= "ov_"
      )
  } else {
    tibble::tibble(link_id = character())
  }

  joined <- joined |>
    dplyr::left_join(ov_wide, by = "link_id")

  # ── Step 4: coalesce override → vitek → os for each cleaned field ─────────
  .coalesce_field <- function(df, ov_col, v_col, o_col) {
    ov <- if (ov_col %in% names(df)) df[[ov_col]] else NA_character_
    v  <- if (v_col  %in% names(df)) as.character(df[[v_col]])  else NA_character_
    o  <- if (o_col  %in% names(df)) as.character(df[[o_col]])  else NA_character_
    dplyr::coalesce(
      dplyr::na_if(as.character(ov), ""),
      dplyr::na_if(v, ""),
      dplyr::na_if(o, "")
    )
  }

  cleaned <- joined |>
    dplyr::mutate(
      clean_lab_id        = .coalesce_field(joined, "ov_lab_id",
                              "lab_id",         "specimen_label"),
      clean_organism      = .coalesce_field(joined, "ov_organism",
                              "v_organism",     "o_custom_organism"),
      clean_specimen_type = .coalesce_field(joined, "ov_specimen_type",
                              "v_specimen_type","o_type"),
      clean_mdro_category = .coalesce_field(joined, "ov_mdro_category",
                              "v_parsed_target","o_custom_mdro"),
      clean_testing_date  = .coalesce_field(joined, "ov_testing_date",
                              "v_testing_date", "o_custom_collection_date"),
      clean_participant_id= .coalesce_field(joined, "ov_participant_id",
                              "v_parsed_subject","o_participant_id"),
      clean_cp_title      = .coalesce_field(joined, "ov_cp_title",
                              "v_cp_hint",      "cp_short_title"),
      clean_parent_specimen_type = .coalesce_field(joined, "ov_parent_specimen_type",
                              "o_parent_specimen_type", "o_type"),
      clean_ast_notes     = if ("ov_ast_notes" %in% names(joined))
                              as.character(joined$ov_ast_notes)
                            else NA_character_
    )

  # ── Step 5: MDRO disagree flag ────────────────────────────────────────────
  cleaned <- cleaned |>
    dplyr::mutate(
      .v_mdro_norm = toupper(trimws(as.character(v_parsed_target))),
      .o_mdro_norm = toupper(trimws(as.character(o_custom_mdro))),
      mdro_disagree = (!is.na(.v_mdro_norm) & .v_mdro_norm != "" &
                       !is.na(.o_mdro_norm) & .o_mdro_norm != "" &
                       .v_mdro_norm != .o_mdro_norm),
      has_edit = FALSE, n_edits = 0L
    ) |>
    dplyr::select(-.v_mdro_norm, -.o_mdro_norm)

  # ── Step 6: compute has_edit / n_edits from overrides ────────────────────
  if (!is.null(overrides) && nrow(overrides) > 0) {
    edit_counts <- overrides |>
      dplyr::group_by(link_id) |>
      dplyr::summarise(n_edits = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(has_edit = TRUE)

    cleaned <- cleaned |>
      dplyr::select(-has_edit, -n_edits) |>
      dplyr::left_join(edit_counts, by = "link_id") |>
      dplyr::mutate(
        has_edit = tidyr::replace_na(has_edit, FALSE),
        n_edits  = tidyr::replace_na(n_edits,  0L)
      )
  }

  # ── Step 7: drop override staging columns, keep canonical output ──────────
  ov_cols <- grep("^ov_", names(cleaned), value = TRUE)
  cleaned |>
    dplyr::select(-dplyr::any_of(ov_cols)) |>
    dplyr::arrange(project_id, lab_id, isolate_number)
}

#' Build linked AST rows for the cleaned export.
#'
#' @param cleaned Cleaned link-level tibble from build_cleaned().
#' @param vitek_ast Long AST tibble from parse_vitek_files().
#' @return One row per confirmed link × antibiotic result.
build_cleaned_ast <- function(cleaned, vitek_ast) {
  if (is.null(cleaned) || nrow(cleaned) == 0 ||
      is.null(vitek_ast) || nrow(vitek_ast) == 0) {
    return(cleaned_ast_empty())
  }

  ast <- vitek_ast |>
    .ensure_export_col("mic") |>
    .ensure_export_col("call_instr") |>
    .ensure_export_col("call_expert") |>
    .ensure_export_col("result_mic") |>
    .ensure_export_col("result_instrument") |>
    .ensure_export_col("result_expertized") |>
    dplyr::mutate(
      mic = dplyr::coalesce(
        dplyr::na_if(as.character(.data$mic), ""),
        dplyr::na_if(as.character(.data$result_mic), "")
      ),
      call_instr = dplyr::coalesce(
        dplyr::na_if(as.character(.data$call_instr), ""),
        dplyr::na_if(as.character(.data$result_instrument), "")
      ),
      call_expert = dplyr::coalesce(
        dplyr::na_if(as.character(.data$call_expert), ""),
        dplyr::na_if(as.character(.data$result_expertized), "")
      )
    )

  cleaned |>
    dplyr::select(
      link_id, batch_id, lab_id, isolate_number, os_identifier,
      specimen_label, project_id, cp_short_title,
      clean_lab_id, clean_organism, clean_mdro_category,
      clean_testing_date, clean_participant_id
    ) |>
    dplyr::left_join(
      ast |>
        dplyr::select(
          dplyr::any_of(c("source_file", "source_row")),
          lab_id, isolate_number,
          drug_code, drug_name, mic, call_instr, call_expert,
          dplyr::any_of("ingested_at")
        ),
      by = c("lab_id", "isolate_number")
    ) |>
    dplyr::arrange(project_id, lab_id, isolate_number, drug_code)
}

#' Write cleaned export artifacts for a batch.
#'
#' @param cleaned Link-level cleaned tibble.
#' @param cleaned_ast Linked AST tibble from build_cleaned_ast().
#' @param batch_id Batch id used in output names.
#' @param output_dir Directory for CSV/XLSX outputs.
#' @param formats Any of "csv", "xlsx", "duckdb".
#' @param conn Optional DBI connection for "duckdb" exports.
#' @return Named list containing output paths and row counts.
export_cleaned_dataset <- function(cleaned, cleaned_ast, batch_id,
                                   output_dir = file.path("data", "exports"),
                                   formats = c("csv", "xlsx", "duckdb"),
                                   conn = NULL) {
  formats <- unique(tolower(formats))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  slug <- gsub("[^A-Za-z0-9_-]+", "_", batch_id)
  base <- file.path(output_dir, paste0("AXIS_clean_", slug))
  outputs <- list(
    csv = character(),
    xlsx = character(),
    duckdb = character(),
    n_cleaned = if (is.null(cleaned)) 0L else nrow(cleaned),
    n_ast = if (is.null(cleaned_ast)) 0L else nrow(cleaned_ast)
  )

  if ("csv" %in% formats) {
    cleaned_path <- paste0(base, "_links.csv")
    ast_path <- paste0(base, "_ast.csv")
    readr::write_csv(.exportable_df(cleaned), cleaned_path, na = "")
    readr::write_csv(.exportable_df(cleaned_ast), ast_path, na = "")
    outputs$csv <- c(cleaned_links = cleaned_path, cleaned_ast = ast_path)
  }

  if ("xlsx" %in% formats) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      warning("openxlsx is not installed; skipping XLSX export.")
    } else {
      xlsx_path <- paste0(base, ".xlsx")
      openxlsx::write.xlsx(
        list(
          cleaned_links = .exportable_df(cleaned),
          cleaned_ast = .exportable_df(cleaned_ast)
        ),
        file = xlsx_path,
        overwrite = TRUE,
        asTable = TRUE
      )
      outputs$xlsx <- xlsx_path
    }
  }

  if ("duckdb" %in% formats) {
    if (is.null(conn)) {
      warning("No DuckDB connection supplied; skipping DuckDB cleaned export.")
    } else {
      write_cleaned_export_tables(conn, cleaned, cleaned_ast, batch_id)
      outputs$duckdb <- c("cleaned_links", "cleaned_ast")
    }
  }

  outputs
}

#' Empty cleaned tibble (matches build_cleaned() output schema).
cleaned_empty <- function() {
  tibble::tibble(
    link_id                  = character(),
    lab_id                   = character(),
    isolate_number           = character(),
    os_identifier            = character(),
    project_id               = character(),
    specimen_label           = character(),
    cp_short_title           = character(),
    confidence               = double(),
    match_method             = character(),
    state                    = character(),
    batch_id                 = character(),
    # Vitek
    v_organism               = character(),
    v_specimen_type          = character(),
    v_specimen_source        = character(),
    v_collection_date        = as.Date(character()),
    v_testing_date           = as.Date(character()),
    v_parsed_study           = character(),
    v_parsed_subject         = character(),
    v_parsed_target          = character(),
    v_cp_hint                = character(),
    v_n_drugs                = integer(),
    v_file_name              = character(),
    # OS
    o_participant_id         = character(),
    o_custom_collection_date = as.Date(character()),
    o_custom_organism        = character(),
    o_custom_mdro            = character(),
    o_parent_specimen_type   = character(),
    # Cleaned
    clean_lab_id             = character(),
    clean_organism           = character(),
    clean_specimen_type      = character(),
    clean_mdro_category      = character(),
    clean_testing_date       = character(),
    clean_participant_id     = character(),
    clean_cp_title           = character(),
    clean_parent_specimen_type = character(),
    clean_ast_notes          = character(),
    # Flags
    has_edit                 = logical(),
    n_edits                  = integer(),
    mdro_disagree            = logical()
  )
}

cleaned_ast_empty <- function() {
  tibble::tibble(
    link_id = character(),
    batch_id = character(),
    lab_id = character(),
    isolate_number = character(),
    os_identifier = character(),
    specimen_label = character(),
    project_id = character(),
    cp_short_title = character(),
    clean_lab_id = character(),
    clean_organism = character(),
    clean_mdro_category = character(),
    clean_testing_date = character(),
    clean_participant_id = character(),
    source_file = character(),
    source_row = integer(),
    drug_code = character(),
    drug_name = character(),
    mic = character(),
    call_instr = character(),
    call_expert = character(),
    ingested_at = as.POSIXct(character())
  )
}

.exportable_df <- function(df) {
  if (is.null(df)) return(tibble::tibble())
  df <- tibble::as_tibble(df)
  list_cols <- vapply(df, is.list, logical(1))
  for (nm in names(df)[list_cols]) {
    df[[nm]] <- vapply(df[[nm]], function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      paste(utils::capture.output(str(x, give.attr = FALSE)), collapse = "\n")
    }, character(1))
  }
  df
}

.ensure_export_col <- function(df, col, default = NA_character_) {
  if (!col %in% names(df)) df[[col]] <- rep(default, nrow(df))
  df
}
