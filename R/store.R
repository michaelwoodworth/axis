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

  # Ledger of which source tables have already been persisted for an ingestion
  # batch. Source persistence happens once per batch (Phase A); manual
  # confirmations must never append source rows again.
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS source_batches (
      batch_id     VARCHAR,
      table_name   VARCHAR,
      n_rows       INTEGER,
      content_hash VARCHAR,
      written_at   TIMESTAMP
    )
  ")
  .ensure_db_columns(conn, "source_batches", c(
    batch_id     = "VARCHAR",
    table_name   = "VARCHAR",
    n_rows       = "INTEGER",
    content_hash = "VARCHAR",
    written_at   = "TIMESTAMP"
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
      anatomic_site          VARCHAR,
      participant_id         VARCHAR,
      custom_collection_date DATE,
      custom_organism        VARCHAR,
      custom_parent_specimen_type VARCHAR,
      custom_day             VARCHAR,
      custom_selective_media VARCHAR,
      custom_site            VARCHAR,
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
    anatomic_site          = "VARCHAR",
    participant_id         = "VARCHAR",
    custom_collection_date = "DATE",
    custom_organism        = "VARCHAR",
    custom_parent_specimen_type = "VARCHAR",
    custom_day             = "VARCHAR",
    custom_selective_media = "VARCHAR",
    custom_site            = "VARCHAR",
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

#' Append only the logical links that are not already confirmed.
#'
#' Returns the rows that were actually inserted, so callers can build audit
#' events for new links only. A repeated confirmation of the same
#' lab_id/isolate_number/os_identifier inserts nothing and returns zero rows.
#'
#' @param conn   DBI connection from open_db().
#' @param links  tibble matching links_confirmed schema.
#' @return tibble of inserted rows (zero rows if everything was a duplicate).
insert_new_links <- function(conn, links) {
  if (is.null(links) || nrow(links) == 0) return(links_confirmed_empty())

  # Ensure link_id column exists
  if (!"link_id" %in% names(links)) {
    links <- links |> dplyr::mutate(
      link_id = purrr::map_chr(seq_len(dplyr::n()), ~ uuid::UUIDgenerate())
    )
  }

  # Collapse duplicates inside the incoming batch first, so a single call can
  # never insert the same logical link twice.
  new_links <- .add_logical_link_key(links) |>
    dplyr::distinct(.axis_lab_key, .axis_isolate_key, .axis_os_key,
                    .keep_all = TRUE) |>
    dplyr::select(-dplyr::starts_with(".axis_"))

  # Skip rows whose logical link has already been confirmed. link_id is generated
  # per commit, so it cannot by itself prevent repeated commits of the same link.
  existing <- tryCatch(
    DBI::dbGetQuery(conn, "
      SELECT link_id, lab_id, isolate_number, os_identifier
      FROM links_confirmed
    "),
    error = function(e) tibble::tibble()
  )
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

  if (nrow(new_links) == 0) return(new_links)

  .append_table_aligned(conn, "links_confirmed", new_links)
  new_links
}

#' Write matched links to links_confirmed (append, skip duplicate logical links).
#'
#' Thin count-returning wrapper around insert_new_links().
#'
#' @param conn   DBI connection from open_db().
#' @param links  tibble matching links_confirmed schema (HANDOFF.md §4).
#' @return Number of rows written.
write_links <- function(conn, links) {
  invisible(nrow(insert_new_links(conn, links)))
}

#' Empty links_confirmed tibble with the canonical column types.
links_confirmed_empty <- function() {
  tibble::tibble(
    link_id        = character(), lab_id = character(), isolate_number = character(),
    os_identifier  = character(), project_id = character(), specimen_label = character(),
    cp_short_title = character(), confidence = double(), match_method = character(),
    state = character(), batch_id = character(),
    created_at = lubridate::ymd_hms(character()), created_by = character()
  )
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
write_cleaned_export_tables <- function(conn, cleaned, cleaned_ast, batch_id,
                                        specimen_dataset = NULL) {
  written <- c(cleaned_links = 0L, cleaned_ast = 0L, specimen_dataset = 0L)

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

  if (!is.null(specimen_dataset) && nrow(specimen_dataset) > 0) {
    tbl <- .with_batch(.stringify_list_cols(specimen_dataset), batch_id)
    .replace_batch_rows(conn, "specimen_dataset", tbl, batch_id)
    written[["specimen_dataset"]] <- nrow(tbl)
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
#' @param match_method Character. auto, manual_confirmed, or manual_selected.
#' @param state        Character link state.
#' @return tibble matching links_confirmed schema.
build_links_from_matches <- function(matched_tbl, batch_id, created_by = "analyst",
                                     match_method = "auto", state = "confirmed") {
  if (is.null(matched_tbl) || nrow(matched_tbl) == 0)
    return(links_confirmed_empty())

  matched_tbl |>
    dplyr::transmute(
      link_id        = purrr::map_chr(seq_len(dplyr::n()), ~ uuid::UUIDgenerate()),
      lab_id         = lab_id,
      isolate_number = isolate_number,
      os_identifier  = os_identifier,
      project_id     = project_id,
      specimen_label = specimen_label,
      cp_short_title = cp_short_title,
      confidence     = dplyr::coalesce(as.numeric(score) / 100, 0),
      match_method   = match_method,
      state          = state,
      batch_id       = batch_id,
      created_at     = lubridate::now(),
      created_by     = created_by
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# Confirmation / export workflow
#
# The work that used to sit in one commit function is split into four
# operations so each can be tested — and paid for — on its own:
#
#   Phase A  persist_source_batch()        once per ingestion batch
#   Phase B  confirm_links()               once per analyst decision (cheap)
#   Phase C  rebuild_cleaned()             in-memory rebuild, no writes
#            write_cleaned_outputs()       CSV / XLSX / DuckDB, on request
#            rebuild_and_export_cleaned()  the two above, as one action
#
# Confirmations deliberately do not touch source tables or cleaned outputs.
# ─────────────────────────────────────────────────────────────────────────────

#' Phase A — persist an ingestion batch's parsed source tables exactly once.
#'
#' Source tables are landing tables for the parsed files. They belong to the
#' ingestion batch, not to any individual linking decision, so they are written
#' once when the batch is prepared and never again while the analyst reviews.
#'
#' A table is written only when what is in the database does not already match
#' what is in memory. The ledger in `source_batches` records the row count and a
#' content fingerprint per (batch_id, table); a write is skipped only when the
#' fingerprint, the recorded row count, and the row count actually present in
#' the table all agree. Anything else — a first load, a re-parse after loading
#' more files, a corrected export that happens to have the same number of rows,
#' or a batch left half-written by an interrupted attempt — replaces that
#' batch's rows rather than appending a second copy.
#'
#' Correctness therefore does not depend on the caller knowing whether the
#' parsed content changed.
#'
#' @param conn      DBI connection from open_db().
#' @param batch_id  Character. Current batch identifier.
#' @param vitek_raw,vitek_ast,specimens Parsed source tibbles (may be NULL).
#' @param force     Logical. TRUE re-writes even when the fingerprint matches.
#'                  Not needed for correctness; an escape hatch for callers that
#'                  want to rewrite unconditionally.
#' @return Named integer vector of rows persisted per table (0 where skipped).
persist_source_batch <- function(conn, batch_id,
                                 vitek_raw = NULL,
                                 vitek_ast = NULL,
                                 specimens = NULL,
                                 force = FALSE) {
  if (is.null(conn)) stop("DuckDB connection is not available.")
  if (is.null(batch_id) || !nzchar(as.character(batch_id))) {
    stop("Batch id is not available.")
  }
  batch_id <- as.character(batch_id)

  inputs <- list(vitek_raw = vitek_raw, vitek_ast = vitek_ast,
                 specimens = specimens)
  written <- c(vitek_raw = 0L, vitek_ast = 0L, specimens = 0L)
  ledger <- .source_batch_ledger(conn, batch_id)

  for (nm in names(inputs)) {
    tbl <- inputs[[nm]]
    if (is.null(tbl) || nrow(tbl) == 0) next

    # Fingerprint the parsed table before staging it. Staging specimens means
    # serialising the OpenSpecimen blob columns, which is the expensive part of
    # this function; there is no reason to pay it just to discover a no-op.
    fingerprint <- .source_fingerprint(tbl)
    if (!isTRUE(force) &&
        .source_batch_is_current(conn, ledger, nm, fingerprint, nrow(tbl), batch_id)) {
      next
    }

    staged <- .with_batch(tbl, batch_id)
    if (identical(nm, "specimens")) staged <- .stringify_list_cols(staged)

    # Replace rather than append, in one transaction with its ledger entry, so
    # an interrupted attempt leaves neither half-written rows nor a ledger
    # entry claiming rows that are not there.
    DBI::dbWithTransaction(conn, {
      .replace_batch_rows(conn, nm, staged, batch_id)
      .record_source_batch(conn, batch_id, nm, nrow(staged), fingerprint)
    })
    written[[nm]] <- nrow(staged)
  }

  written
}

#' Content fingerprint for a parsed source table.
.source_fingerprint <- function(tbl) {
  tryCatch(rlang::hash(tibble::as_tibble(tbl)), error = function(e) NA_character_)
}

#' Ledger rows recorded for this batch, keyed by table name.
.source_batch_ledger <- function(conn, batch_id) {
  if (!"source_batches" %in% DBI::dbListTables(conn)) return(tibble::tibble())
  tryCatch(
    DBI::dbGetQuery(
      conn,
      "SELECT table_name, n_rows, content_hash FROM source_batches WHERE batch_id = ?",
      params = list(as.character(batch_id))
    ),
    error = function(e) tibble::tibble()
  )
}

#' Is this table already persisted for this batch, with this exact content?
.source_batch_is_current <- function(conn, ledger, table_name, fingerprint,
                                     n_rows, batch_id) {
  if (is.na(fingerprint)) return(FALSE)
  if (is.null(ledger) || nrow(ledger) == 0) return(FALSE)
  row <- ledger[ledger$table_name == table_name, , drop = FALSE]
  if (nrow(row) != 1L) return(FALSE)
  if (!identical(as.character(row$content_hash[[1]]), fingerprint)) return(FALSE)
  if (!identical(as.integer(row$n_rows[[1]]), as.integer(n_rows))) return(FALSE)

  # Guard against a ledger entry that outlived its rows.
  actual <- tryCatch(
    DBI::dbGetQuery(
      conn,
      paste0("SELECT COUNT(*) AS n FROM ", DBI::dbQuoteIdentifier(conn, table_name),
             " WHERE batch_id = ?"),
      params = list(as.character(batch_id))
    )$n[[1]],
    error = function(e) NA_real_
  )
  isTRUE(as.numeric(actual) == as.numeric(n_rows))
}

.record_source_batch <- function(conn, batch_id, table_name, n_rows,
                                 content_hash = NA_character_) {
  DBI::dbExecute(
    conn,
    "DELETE FROM source_batches WHERE batch_id = ? AND table_name = ?",
    params = list(as.character(batch_id), as.character(table_name))
  )
  .append_table_aligned(conn, "source_batches", tibble::tibble(
    batch_id     = as.character(batch_id),
    table_name   = as.character(table_name),
    n_rows       = as.integer(n_rows),
    content_hash = as.character(content_hash),
    written_at   = lubridate::now()
  ))
  invisible(NULL)
}

#' Phase B — record one or more confirmed links and their audit events.
#'
#' This is the whole cost of an analyst confirmation. It appends the new
#' logical links, appends one manual-confirmation audit event per *newly
#' inserted* link, and returns. It does not write source tables, rebuild the
#' cleaned dataset, or touch CSV/XLSX/DuckDB cleaned outputs.
#'
#' Repeating the same confirmation inserts nothing and logs nothing.
#'
#' @param conn         DBI connection from open_db().
#' @param matched      Candidate rows to confirm (as produced by matching, or
#'                     assembled by the Manual Link dialog).
#' @param batch_id     Character. Current batch identifier.
#' @param match_method "auto", "manual_confirmed", or "manual_selected".
#'                     Audit events are written for the manual methods only.
#' @param created_by   Character. Analyst / session identifier.
#' @param rationale    Optional free text stored with the audit event.
#' @param state        Link state written to links_confirmed.
#' @return list(n_committed, inserted, links_confirmed)
confirm_links <- function(conn, matched, batch_id,
                          match_method = "auto",
                          created_by = "analyst",
                          rationale = "",
                          state = "confirmed") {
  if (is.null(conn)) stop("DuckDB connection is not available.")
  if (is.null(batch_id) || !nzchar(as.character(batch_id))) {
    stop("Batch id is not available.")
  }
  if (is.null(matched) || nrow(matched) == 0) {
    stop("No link records are staged to commit.")
  }

  links <- build_links_from_matches(
    matched, batch_id,
    created_by   = created_by,
    match_method = match_method,
    state        = state
  )
  # The link and its audit event are one fact about one analyst decision, so
  # they are written together. Without the transaction an interruption between
  # the two writes leaves a confirmed link with no record of who confirmed it.
  inserted <- NULL
  audit <- NULL
  DBI::dbWithTransaction(conn, {
    inserted <- insert_new_links(conn, links)

    if (nrow(inserted) > 0L && !identical(match_method, "auto")) {
      rationale_text <- if (is.null(rationale) || length(rationale) == 0L ||
                            is.na(rationale[[1]])) "" else trimws(as.character(rationale[[1]]))
      audit <- inserted |>
        dplyr::transmute(
          event_id = purrr::map_chr(seq_len(dplyr::n()), ~ uuid::UUIDgenerate()),
          link_id = .data$link_id,
          event_type = "link.manually_confirmed",
          field = "link",
          from_value = "unconfirmed",
          to_value = paste0(
            .data$os_identifier,
            if (nzchar(rationale_text)) paste0(" | ", rationale_text) else ""
          ),
          who = created_by,
          when_ts = lubridate::now()
        )
      write_overrides(conn, overrides = NULL, edit_log = audit)
    }
  })

  list(
    n_committed = nrow(inserted),
    inserted    = inserted,
    audit       = audit
  )
}

#' Phase C (part 1) — rebuild the cleaned datasets in memory.
#'
#' Reads the complete set of confirmed links and the latest overrides, then
#' rebuilds the cleaned link, AST, and specimen datasets. Writes nothing.
#'
#' @param conn         DBI connection from open_db().
#' @param vitek_unique Deduplicated Vitek isolates for the loaded batch.
#' @param vitek_ast    Long AST rows for the loaded batch.
#' @param specimens    Parsed OpenSpecimen records for the loaded batch.
#' @param links_confirmed,cleaned_overrides Optional pre-read tables; read from
#'        the database when NULL.
#' @return list(links_confirmed, cleaned_overrides, cleaned_links, cleaned_ast,
#'         specimen_dataset)
rebuild_cleaned <- function(conn,
                            vitek_unique = NULL,
                            vitek_ast = NULL,
                            specimens = NULL,
                            links_confirmed = NULL,
                            cleaned_overrides = NULL) {
  if (is.null(conn)) stop("DuckDB connection is not available.")

  if (is.null(links_confirmed)) {
    links_confirmed <- tryCatch(
      read_table(conn, "links_confirmed"),
      error = function(e) tibble::tibble()
    )
  }
  if (is.null(cleaned_overrides)) {
    cleaned_overrides <- tryCatch(
      read_table(conn, "cleaned_overrides"),
      error = function(e) tibble::tibble()
    )
  }

  cleaned_links <- build_cleaned(
    links     = links_confirmed,
    overrides = cleaned_overrides,
    vitek     = vitek_unique,
    specimens = specimens
  )
  cleaned_ast <- build_cleaned_ast(cleaned_links, vitek_ast)
  specimen_dataset <- build_specimen_dataset(cleaned_links, specimens)

  list(
    links_confirmed   = links_confirmed,
    cleaned_overrides = cleaned_overrides,
    cleaned_links     = cleaned_links,
    cleaned_ast       = cleaned_ast,
    specimen_dataset  = specimen_dataset
  )
}

#' Phase C (part 2) — write the rebuilt cleaned datasets to the output formats.
#'
#' @param conn     DBI connection (required for the "duckdb" format).
#' @param rebuilt  The list returned by rebuild_cleaned().
#' @param batch_id Character. Current batch identifier.
#' @param specimens Parsed OpenSpecimen records, used for specimen hierarchy.
#' @param output_dir,csv_path,formats As for export_cleaned_dataset().
#' @return The export_cleaned_dataset() output list.
write_cleaned_outputs <- function(conn, rebuilt, batch_id,
                                  specimens = NULL,
                                  output_dir = file.path("data", "exports"),
                                  csv_path = NULL,
                                  formats = c("csv", "xlsx", "duckdb")) {
  if (is.null(batch_id) || !nzchar(as.character(batch_id))) {
    stop("Batch id is not available.")
  }
  export_cleaned_dataset(
    cleaned          = rebuilt$cleaned_links,
    cleaned_ast      = rebuilt$cleaned_ast,
    batch_id         = batch_id,
    specimens        = specimens,
    specimen_dataset = rebuilt$specimen_dataset,
    output_dir       = output_dir,
    csv_path         = csv_path,
    formats          = formats,
    conn             = conn
  )
}

#' Phase C — rebuild the cleaned datasets and write every requested output.
#'
#' This is the explicit "Rebuild and export cleaned data" action. It is the only
#' operation that writes CSV, XLSX, or the cleaned DuckDB tables.
#'
#' If the export step fails, the error propagates: confirmed links and audit
#' events written earlier stay in the database, so the analyst can retry without
#' reconfirming anything.
#'
#' @inheritParams rebuild_cleaned
#' @param batch_id Character. Current batch identifier.
#' @param output_dir,csv_path,formats As for export_cleaned_dataset().
#' @return The rebuild_cleaned() list plus `export_info`.
rebuild_and_export_cleaned <- function(conn, batch_id,
                                       vitek_unique = NULL,
                                       vitek_ast = NULL,
                                       specimens = NULL,
                                       links_confirmed = NULL,
                                       cleaned_overrides = NULL,
                                       output_dir = file.path("data", "exports"),
                                       csv_path = NULL,
                                       formats = c("csv", "xlsx", "duckdb")) {
  rebuilt <- rebuild_cleaned(
    conn,
    vitek_unique      = vitek_unique,
    vitek_ast         = vitek_ast,
    specimens         = specimens,
    links_confirmed   = links_confirmed,
    cleaned_overrides = cleaned_overrides
  )
  export_info <- write_cleaned_outputs(
    conn, rebuilt,
    batch_id   = batch_id,
    specimens  = specimens,
    output_dir = output_dir,
    csv_path   = csv_path,
    formats    = formats
  )
  c(rebuilt, list(export_info = export_info))
}

#' Commit matched links, then rebuild and export in one call.
#'
#' Superseded by persist_source_batch() / confirm_links() /
#' rebuild_and_export_cleaned(), which the application now calls directly so
#' that a confirmation does not pay for a full export. Retained as a composite
#' for callers and scripts that still want the old one-shot behaviour; source
#' persistence is idempotent per batch, so repeated calls no longer multiply
#' source rows.
commit_matched_links <- function(conn, matched, batch_id,
                                 vitek_raw = NULL,
                                 vitek_ast = NULL,
                                 vitek_unique = NULL,
                                 specimens = NULL,
                                 cleaned_overrides = NULL,
                                 csv_path = NULL,
                                 output_dir = file.path("data", "exports"),
                                 formats = c("csv", "xlsx", "duckdb"),
                                 match_method = "auto",
                                 created_by = "analyst",
                                 rationale = "") {
  if (is.null(vitek_unique) || nrow(vitek_unique) == 0 ||
      is.null(specimens) || nrow(specimens) == 0) {
    stop("Parsed Vitek and OpenSpecimen data are not available. Run ingestion/automerge first.")
  }

  confirmed <- confirm_links(
    conn, matched, batch_id,
    match_method = match_method,
    created_by   = created_by,
    rationale    = rationale
  )

  written_sources <- c(vitek_raw = 0L, vitek_ast = 0L, specimens = 0L)
  if (confirmed$n_committed > 0L) {
    written_sources <- persist_source_batch(
      conn, batch_id = batch_id,
      vitek_raw = vitek_raw, vitek_ast = vitek_ast, specimens = specimens
    )
  }

  result <- rebuild_and_export_cleaned(
    conn, batch_id = batch_id,
    vitek_unique      = vitek_unique,
    vitek_ast         = vitek_ast,
    specimens         = specimens,
    cleaned_overrides = cleaned_overrides,
    output_dir        = output_dir,
    csv_path          = csv_path,
    formats           = formats
  )

  list(
    n_committed = confirmed$n_committed,
    written_sources = written_sources,
    links_confirmed = result$links_confirmed,
    cleaned_overrides = result$cleaned_overrides,
    cleaned_links = result$cleaned_links,
    cleaned_ast = result$cleaned_ast,
    specimen_dataset = result$specimen_dataset,
    export_info = result$export_info
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
