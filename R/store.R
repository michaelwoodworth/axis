# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/store.R  — DuckDB persistence layer
# DB lives at axis/data/axis.duckdb (created on first run).
# All write functions are append-safe and create tables if missing.
# ─────────────────────────────────────────────────────────────────────────────

# DB_PATH is relative to the *app* working directory (axis/).
# Override by setting options(axis.db_path = "/your/path/axis.duckdb") before
# launching the app.
.db_path <- function() {
  getOption("axis.db_path", default = file.path("data", "axis.duckdb"))
}

#' Open (or create) the AXIS DuckDB connection.
#'
#' Creates data/ directory and initialises schema tables if the DB is new.
#' Call once per Shiny session; store the result in session$userData.
#'
#' @param path  Character. Path to the .duckdb file.
#' @return A DBI connection object.
open_db <- function(path = .db_path()) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = FALSE)
  init_db_schema(conn)
  conn
}

#' Initialise schema tables (no-op if tables already exist).
init_db_schema <- function(conn) {
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS links_confirmed (
      link_id        VARCHAR PRIMARY KEY,
      lab_id         VARCHAR,
      isolate_number VARCHAR,
      os_identifier  VARCHAR,
      project_id     VARCHAR,
      specimen_label VARCHAR,
      cp_short_title VARCHAR,
      confidence     DOUBLE,
      match_method   VARCHAR,
      state          VARCHAR,
      batch_id       VARCHAR,
      created_at     TIMESTAMP,
      created_by     VARCHAR
    )
  ")
  .ensure_db_columns(conn, "links_confirmed", c(
    link_id        = "VARCHAR",
    lab_id         = "VARCHAR",
    isolate_number = "VARCHAR",
    os_identifier  = "VARCHAR",
    project_id     = "VARCHAR",
    specimen_label = "VARCHAR",
    cp_short_title = "VARCHAR",
    confidence     = "DOUBLE",
    match_method   = "VARCHAR",
    state          = "VARCHAR",
    batch_id       = "VARCHAR",
    created_at     = "TIMESTAMP",
    created_by     = "VARCHAR"
  ))

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS cleaned_overrides (
      link_id       VARCHAR,
      field         VARCHAR,
      cleaned_value VARCHAR,
      source_hint   VARCHAR,
      rationale     VARCHAR,
      edited_at     TIMESTAMP,
      edited_by     VARCHAR
    )
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS edit_log (
      event_id   VARCHAR PRIMARY KEY,
      link_id    VARCHAR,
      event_type VARCHAR,
      field      VARCHAR,
      from_value VARCHAR,
      to_value   VARCHAR,
      who        VARCHAR,
      when_ts    TIMESTAMP
    )
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS vitek_raw (
      batch_id         VARCHAR,
      source_file      VARCHAR,
      source_row       INTEGER,
      lab_id           VARCHAR,
      isolate_number   VARCHAR,
      patient_id       VARCHAR,
      specimen_type    VARCHAR,
      specimen_source  VARCHAR,
      collection_date  DATE,
      testing_date     DATE,
      organism_name    VARCHAR,
      organism_code    VARCHAR,
      bio_number       VARCHAR,
      pct_probability  VARCHAR,
      id_confidence    VARCHAR,
      selected_bp_site VARCHAR,
      parsed_study     VARCHAR,
      parsed_subject   VARCHAR,
      parsed_target    VARCHAR,
      cp_hint          VARCHAR,
      n_drugs          INTEGER,
      ingested_at      TIMESTAMP,
      file_name        VARCHAR
    )
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS vitek_ast (
      batch_id          VARCHAR,
      source_file       VARCHAR,
      source_row        INTEGER,
      lab_id            VARCHAR,
      isolate_number    VARCHAR,
      drug_code         VARCHAR,
      drug_name         VARCHAR,
      mic               VARCHAR,
      call_instr        VARCHAR,
      call_expert       VARCHAR,
      result_mic        VARCHAR,
      result_instrument VARCHAR,
      result_expertized VARCHAR,
      ingested_at       TIMESTAMP
    )
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS specimens (
      batch_id               VARCHAR,
      source_file            VARCHAR,
      source_row             INTEGER,
      project_id             VARCHAR,
      os_identifier          VARCHAR,
      specimen_label_raw     VARCHAR,
      specimen_label         VARCHAR,
      cp_short_title         VARCHAR,
      class                  VARCHAR,
      type                   VARCHAR,
      lineage                VARCHAR,
      parent_label           VARCHAR,
      collection_dt          TIMESTAMP,
      available_qty          DOUBLE,
      activity_status        VARCHAR,
      location_container     VARCHAR,
      location_row           VARCHAR,
      location_col           VARCHAR,
      location_pos           VARCHAR,
      participant_id         VARCHAR,
      custom_collection_date DATE,
      custom_organism        VARCHAR,
      custom_parent_specimen_type VARCHAR,
      custom_day             VARCHAR,
      custom_selective_media VARCHAR,
      custom_growth_blob     VARCHAR,
      custom_mdro            VARCHAR,
      cfu_raw                VARCHAR,
      cfu_log10              DOUBLE,
      cfu_value              DOUBLE,
      cfu_unit               VARCHAR,
      cfu_censored           BOOLEAN,
      growth_method          VARCHAR,
      is_pseudocount         BOOLEAN,
      cfu_flag               VARCHAR,
      has_quant              BOOLEAN,
      custom_form_blob       VARCHAR
    )
  ")

  .ensure_db_columns(conn, "specimens", c(
    batch_id               = "VARCHAR",
    source_file            = "VARCHAR",
    source_row             = "INTEGER",
    project_id             = "VARCHAR",
    os_identifier          = "VARCHAR",
    specimen_label_raw     = "VARCHAR",
    specimen_label         = "VARCHAR",
    cp_short_title         = "VARCHAR",
    class                  = "VARCHAR",
    type                   = "VARCHAR",
    lineage                = "VARCHAR",
    parent_label           = "VARCHAR",
    collection_dt          = "TIMESTAMP",
    available_qty          = "DOUBLE",
    activity_status        = "VARCHAR",
    location_container     = "VARCHAR",
    location_row           = "VARCHAR",
    location_col           = "VARCHAR",
    location_pos           = "VARCHAR",
    participant_id         = "VARCHAR",
    custom_collection_date = "DATE",
    custom_organism        = "VARCHAR",
    custom_parent_specimen_type = "VARCHAR",
    custom_day             = "VARCHAR",
    custom_selective_media = "VARCHAR",
    custom_growth_blob     = "VARCHAR",
    custom_mdro            = "VARCHAR",
    cfu_raw                = "VARCHAR",
    cfu_log10              = "DOUBLE",
    cfu_value              = "DOUBLE",
    cfu_unit               = "VARCHAR",
    cfu_censored           = "BOOLEAN",
    growth_method          = "VARCHAR",
    is_pseudocount         = "BOOLEAN",
    cfu_flag               = "VARCHAR",
    has_quant              = "BOOLEAN",
    custom_form_blob       = "VARCHAR"
  ))

  invisible(conn)
}

#' Write matched links to links_confirmed (append, skip duplicate logical links).
#'
#' @param conn   DBI connection from open_db().
#' @param links  tibble matching links_confirmed schema (HANDOFF.md §4).
#' @return Number of rows written.
write_links <- function(conn, links) {
  if (is.null(links) || nrow(links) == 0) return(invisible(0L))

  # Ensure link_id column exists
  if (!"link_id" %in% names(links)) {
    links <- links |> dplyr::mutate(
      link_id = purrr::map_chr(seq_len(dplyr::n()), ~ uuid::UUIDgenerate())
    )
  }

  # Skip rows whose logical link has already been confirmed. link_id is generated
  # per commit, so it cannot by itself prevent repeated commits of the same link.
  existing <- tryCatch(
    DBI::dbGetQuery(conn, "
      SELECT link_id, lab_id, isolate_number, os_identifier
      FROM links_confirmed
    "),
    error = function(e) tibble::tibble()
  )
  new_links <- links
  if (nrow(existing) > 0) {
    existing_keys <- .add_logical_link_key(existing) |>
      dplyr::select(.axis_lab_key, .axis_isolate_key, .axis_os_key)

    new_links <- .add_logical_link_key(new_links) |>
      dplyr::anti_join(
        existing_keys,
        by = c(".axis_lab_key", ".axis_isolate_key", ".axis_os_key")
      ) |>
      dplyr::select(-dplyr::starts_with(".axis_"))
  }

  if (nrow(new_links) == 0) return(invisible(0L))

  .append_table_aligned(conn, "links_confirmed", new_links)
  invisible(nrow(new_links))
}

#' Write field-level overrides and append to the edit log.
#'
#' @param conn      DBI connection.
#' @param overrides tibble matching cleaned_overrides schema.
#' @param edit_log  tibble matching edit_log schema (optional).
write_overrides <- function(conn, overrides, edit_log = NULL) {
  if (!is.null(overrides) && nrow(overrides) > 0)
    .append_table_aligned(conn, "cleaned_overrides", overrides)

  if (!is.null(edit_log) && nrow(edit_log) > 0)
    .append_table_aligned(conn, "edit_log", edit_log)

  invisible(NULL)
}

#' Write parsed source data for an ingestion batch.
#'
#' These tables are append-only landing tables. Source files remain untouched;
#' each row is tagged with the AXIS batch id that produced it.
write_ingested_tables <- function(conn, batch_id,
                                  vitek_raw = NULL,
                                  vitek_ast = NULL,
                                  specimens = NULL) {
  written <- c(vitek_raw = 0L, vitek_ast = 0L, specimens = 0L)

  if (!is.null(vitek_raw) && nrow(vitek_raw) > 0) {
    tbl <- .with_batch(vitek_raw, batch_id)
    .append_table_aligned(conn, "vitek_raw", tbl)
    written[["vitek_raw"]] <- nrow(tbl)
  }

  if (!is.null(vitek_ast) && nrow(vitek_ast) > 0) {
    tbl <- .with_batch(vitek_ast, batch_id)
    .append_table_aligned(conn, "vitek_ast", tbl)
    written[["vitek_ast"]] <- nrow(tbl)
  }

  if (!is.null(specimens) && nrow(specimens) > 0) {
    tbl <- .with_batch(specimens, batch_id)
    tbl <- .stringify_list_cols(tbl)
    .append_table_aligned(conn, "specimens", tbl)
    written[["specimens"]] <- nrow(tbl)
  }

  written
}

#' Write the current cleaned export to DuckDB.
#'
#' The tables are kept as canonical append-friendly tables. Re-exporting the
#' same batch replaces that batch's rows before appending fresh data.
write_cleaned_export_tables <- function(conn, cleaned, cleaned_ast, batch_id) {
  written <- c(cleaned_links = 0L, cleaned_ast = 0L)

  if (!is.null(cleaned) && nrow(cleaned) > 0) {
    tbl <- .with_batch(.stringify_list_cols(cleaned), batch_id)
    .replace_batch_rows(conn, "cleaned_links", tbl, batch_id)
    written[["cleaned_links"]] <- nrow(tbl)
  }

  if (!is.null(cleaned_ast) && nrow(cleaned_ast) > 0) {
    tbl <- .with_batch(.stringify_list_cols(cleaned_ast), batch_id)
    .replace_batch_rows(conn, "cleaned_ast", tbl, batch_id)
    written[["cleaned_ast"]] <- nrow(tbl)
  }

  written
}

#' Read a table from DuckDB into a tibble.
#'
#' @param conn        DBI connection.
#' @param table_name  Character. One of "links_confirmed", "cleaned_overrides", "edit_log".
#' @return tibble.
read_table <- function(conn, table_name) {
  tryCatch(
    dplyr::tbl(conn, table_name) |> dplyr::collect(),
    error = function(e) {
      warning(paste0("read_table('", table_name, "') failed: ", e$message))
      tibble::tibble()
    }
  )
}

#' Gracefully close the DuckDB connection.
close_db <- function(conn) {
  if (!is.null(conn)) {
    tryCatch(DBI::dbDisconnect(conn, shutdown = TRUE), error = function(e) NULL)
  }
  invisible(NULL)
}

#' Construct a links_confirmed tibble from match buckets (for commit step).
#'
#' @param matched_tbl  $matched from bucket_results().
#' @param batch_id     Character. Current batch identifier.
#' @param created_by   Character. Username / session ID.
#' @return tibble matching links_confirmed schema.
build_links_from_matches <- function(matched_tbl, batch_id, created_by = "analyst") {
  if (is.null(matched_tbl) || nrow(matched_tbl) == 0)
    return(tibble::tibble(
      link_id        = character(), lab_id = character(), isolate_number = character(),
      os_identifier  = character(), project_id = character(), specimen_label = character(),
      cp_short_title = character(), confidence = double(), match_method = character(),
      state = character(), batch_id = character(),
      created_at = lubridate::ymd_hms(character()), created_by = character()
    ))

  matched_tbl |>
    dplyr::transmute(
      link_id        = purrr::map_chr(seq_len(dplyr::n()), ~ uuid::UUIDgenerate()),
      lab_id         = lab_id,
      isolate_number = isolate_number,
      os_identifier  = os_identifier,
      project_id     = project_id,
      specimen_label = specimen_label,
      cp_short_title = cp_short_title,
      confidence     = score / 100,
      match_method   = "auto",
      state          = "confirmed",
      batch_id       = batch_id,
      created_at     = lubridate::now(),
      created_by     = created_by
    )
}

#' Commit auto-matched links and refresh cleaned exports.
#'
#' Shared by the Ingestion and Linking modules so the "Commit matched only"
#' action behaves the same from either tab. Source tables are append-only landing
#' tables; cleaned exports are rebuilt from confirmed links plus overrides.
commit_matched_links <- function(conn, matched, batch_id,
                                 vitek_raw = NULL,
                                 vitek_ast = NULL,
                                 vitek_unique = NULL,
                                 specimens = NULL,
                                 cleaned_overrides = NULL,
                                 csv_path = NULL,
                                 output_dir = file.path("data", "exports"),
                                 formats = c("csv", "xlsx", "duckdb")) {
  if (is.null(conn)) stop("DuckDB connection is not available.")
  if (is.null(batch_id) || !nzchar(as.character(batch_id))) {
    stop("Batch id is not available.")
  }
  if (is.null(matched) || nrow(matched) == 0) {
    stop("No auto-matched records are staged to commit.")
  }
  if (is.null(vitek_unique) || nrow(vitek_unique) == 0 ||
      is.null(specimens) || nrow(specimens) == 0) {
    stop("Parsed Vitek and OpenSpecimen data are not available. Run ingestion/automerge first.")
  }

  links <- build_links_from_matches(matched, batch_id)
  n <- write_links(conn, links)
  written_sources <- c(vitek_raw = 0L, vitek_ast = 0L, specimens = 0L)
  if (n > 0L) {
    written_sources <- write_ingested_tables(
      conn,
      batch_id  = batch_id,
      vitek_raw = vitek_raw,
      vitek_ast = vitek_ast,
      specimens = specimens
    )
  }

  links_confirmed <- tryCatch(
    read_table(conn, "links_confirmed"),
    error = function(e) tibble::tibble()
  )
  overrides <- cleaned_overrides
  if (is.null(overrides)) {
    overrides <- tryCatch(
      read_table(conn, "cleaned_overrides"),
      error = function(e) tibble::tibble()
    )
  }

  cleaned_links <- build_cleaned(
    links     = links_confirmed,
    overrides = overrides,
    vitek     = vitek_unique,
    specimens = specimens
  )
  cleaned_ast <- build_cleaned_ast(cleaned_links, vitek_ast)
  export_info <- export_cleaned_dataset(
    cleaned     = cleaned_links,
    cleaned_ast = cleaned_ast,
    batch_id    = batch_id,
    output_dir  = output_dir,
    csv_path    = csv_path,
    formats     = formats,
    conn        = conn
  )

  list(
    n_committed = n,
    written_sources = written_sources,
    links_confirmed = links_confirmed,
    cleaned_overrides = overrides,
    cleaned_links = cleaned_links,
    cleaned_ast = cleaned_ast,
    export_info = export_info
  )
}

.with_batch <- function(df, batch_id) {
  df <- tibble::as_tibble(df)
  if ("batch_id" %in% names(df)) df$batch_id <- batch_id
  else df <- dplyr::mutate(df, batch_id = batch_id, .before = 1)
  df
}

.add_logical_link_key <- function(df) {
  df |>
    dplyr::mutate(
      .axis_lab_key = toupper(trimws(as.character(.data$lab_id))),
      .axis_isolate_key = toupper(trimws(as.character(.data$isolate_number))),
      .axis_os_key = toupper(trimws(as.character(.data$os_identifier)))
    )
}

.stringify_list_cols <- function(df) {
  list_cols <- vapply(df, is.list, logical(1))
  for (nm in names(df)[list_cols]) {
    df[[nm]] <- vapply(df[[nm]], function(x) {
      if (is.null(x) || length(x) == 0) return(NA_character_)
      paste(utils::capture.output(str(x, give.attr = FALSE)), collapse = "\n")
    }, character(1))
  }
  df
}

.replace_batch_rows <- function(conn, table_name, df, batch_id) {
  df <- tibble::as_tibble(df)
  exists <- table_name %in% DBI::dbListTables(conn)
  if (exists) {
    .ensure_db_columns(conn, table_name, .infer_db_columns(df))
    DBI::dbExecute(
      conn,
      paste0("DELETE FROM ", DBI::dbQuoteIdentifier(conn, table_name), " WHERE batch_id = ?"),
      params = list(batch_id)
    )
    .append_table_aligned(conn, table_name, df)
  } else {
    DBI::dbWriteTable(conn, table_name, df, overwrite = TRUE)
  }
  invisible(nrow(df))
}

.infer_db_columns <- function(df) {
  vapply(df, function(x) {
    if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return("TIMESTAMP")
    if (inherits(x, "Date")) return("DATE")
    if (is.integer(x)) return("INTEGER")
    if (is.numeric(x)) return("DOUBLE")
    if (is.logical(x)) return("BOOLEAN")
    "VARCHAR"
  }, character(1), USE.NAMES = TRUE)
}

.table_columns <- function(conn, table_name) {
  if (!table_name %in% DBI::dbListTables(conn)) return(character())
  DBI::dbGetQuery(
    conn,
    paste0("DESCRIBE ", DBI::dbQuoteIdentifier(conn, table_name))
  )[["column_name"]]
}

.ensure_db_columns <- function(conn, table_name, columns) {
  existing <- .table_columns(conn, table_name)
  missing <- setdiff(names(columns), existing)
  for (col in missing) {
    DBI::dbExecute(
      conn,
      paste(
        "ALTER TABLE",
        DBI::dbQuoteIdentifier(conn, table_name),
        "ADD COLUMN",
        DBI::dbQuoteIdentifier(conn, col),
        columns[[col]]
      )
    )
  }
  invisible(NULL)
}

.append_table_aligned <- function(conn, table_name, df) {
  df <- tibble::as_tibble(df)
  db_cols <- .table_columns(conn, table_name)
  if (length(db_cols) == 0) {
    DBI::dbWriteTable(conn, table_name, df, append = TRUE)
    return(invisible(nrow(df)))
  }

  for (col in setdiff(db_cols, names(df))) {
    df[[col]] <- NA
  }
  df <- df[, db_cols, drop = FALSE]
  DBI::dbWriteTable(conn, table_name, df, append = TRUE)
  invisible(nrow(df))
}
