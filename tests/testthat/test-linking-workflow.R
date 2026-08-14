library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(lubridate)
library(purrr)

source("../../R/data_dedup.R")
source("../../R/data_clean.R")
source("../../R/data_export_cfu.R")
source("../../R/data_match.R")
source("../../R/mdro_categories.R")
source("../../R/store.R")
source("../../R/mod_linking.R")

# End-to-end checks of the Linking module's confirmation and export actions,
# driven through shiny::testServer. Synthetic records only.

.lk_fixture <- function(n = 4L) {
  lab_ids <- sprintf("SYN%03dCRE1of1", seq_len(n))
  vitek_raw <- tibble::tibble(
    source_file = "synthetic.xlsx", source_row = seq_len(n),
    lab_id = lab_ids, isolate_number = "1",
    specimen_type = "Isolate", specimen_source = "Rectal",
    collection_date = as.Date("2026-01-01"), testing_date = as.Date("2026-01-03"),
    organism_name = "Klebsiella pneumoniae", parsed_study = "SYNTH",
    parsed_subject = sprintf("SYN%03d", seq_len(n)), parsed_target = "CRE",
    cp_hint = "Synthetic Protocol", n_drugs = 2L,
    ingested_at = as.POSIXct("2026-01-04 09:00:00", tz = "UTC"),
    file_name = "synthetic.xlsx"
  )
  vitek_ast <- tidyr::expand_grid(i = seq_len(n), d = seq_len(2)) |>
    dplyr::transmute(
      source_file = "synthetic.xlsx", source_row = .data$i,
      lab_id = lab_ids[.data$i], isolate_number = "1",
      drug_code = sprintf("D%d", .data$d), drug_name = sprintf("Drug %d", .data$d),
      mic = "<=1", call_instr = "S", call_expert = "S",
      result_mic = "<=1", result_instrument = "S", result_expertized = "S",
      ingested_at = as.POSIXct("2026-01-04 09:00:00", tz = "UTC")
    )
  specimens <- tibble::tibble(
    source_file = "synthetic_os.csv", source_row = seq_len(n),
    project_id = "SYNTH", os_identifier = sprintf("OS-%03d", seq_len(n)),
    specimen_label_raw = lab_ids, specimen_label = lab_ids,
    cp_short_title = "Synthetic Protocol", class = "Fluid",
    type = "Cryopreserved Cells", lineage = "Aliquot", parent_label = NA_character_,
    collection_dt = as.POSIXct("2026-01-01", tz = "UTC"),
    available_qty = 1, activity_status = "Active", anatomic_site = "Rectal",
    participant_id = sprintf("SYN%03d", seq_len(n)),
    custom_collection_date = as.Date("2026-01-01"),
    custom_organism = "Klebsiella pneumoniae",
    custom_parent_specimen_type = "Stool", custom_mdro = "CRE", has_quant = FALSE
  )
  candidates <- tibble::tibble(
    lab_id = lab_ids, isolate_number = "1",
    os_identifier = sprintf("OS-%03d", seq_len(n)), project_id = "SYNTH",
    specimen_label = lab_ids, cp_short_title = "Synthetic Protocol",
    score = 95,
    mdro_disagree = FALSE, organism_disagree = FALSE
  )
  list(vitek_raw = vitek_raw, vitek_ast = vitek_ast,
       vitek_unique = dedup_vitek(vitek_raw, "latest"),
       specimens = specimens, candidates = candidates)
}

.lk_state <- function(fx, conn) {
  shiny::reactiveValues(
    vitek_raw = fx$vitek_raw, vitek_ast = fx$vitek_ast,
    vitek_unique = fx$vitek_unique, specimens = fx$specimens,
    match_candidates = fx$candidates,
    match_buckets = list(matched = fx$candidates[1:2, ],
                         review = fx$candidates[3, , drop = FALSE],
                         none = fx$candidates[4, , drop = FALSE]),
    links_confirmed = NULL, cleaned_links = NULL, cleaned_ast = NULL,
    cleaned_overrides = NULL, edit_log = NULL, specimen_dataset = NULL,
    batch_id = "B-lk", needs_export = FALSE, pending_confirmations = 0L,
    last_export_failed = FALSE, cleaned_csv_path = NULL,
    db_conn = conn
  )
}

.rows <- function(conn, table_name) {
  if (!table_name %in% DBI::dbListTables(conn)) return(0L)
  as.integer(DBI::dbGetQuery(
    conn, paste0("SELECT COUNT(*) n FROM ", DBI::dbQuoteIdentifier(conn, table_name))
  )$n[[1]])
}

test_that("committing matched rows saves links without exporting", {
  fx <- .lk_fixture()
  conn <- open_db(tempfile(fileext = ".duckdb"))
  withr::defer(close_db(conn))
  persist_source_batch(conn, "B-lk", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  app_state <- .lk_state(fx, conn)

  shiny::testServer(linkingServer, args = list(app_state = app_state), {
    session$setInputs(commit_matched = 1)

    expect_equal(nrow(app_state$links_confirmed), 2L)
    expect_true(app_state$needs_export)
    expect_equal(app_state$pending_confirmations, 2L)
    expect_null(app_state$cleaned_links)
  })

  expect_equal(.rows(conn, "links_confirmed"), 2L)
  expect_false("cleaned_links" %in% DBI::dbListTables(conn))
  expect_equal(.rows(conn, "vitek_ast"), nrow(fx$vitek_ast))
})

test_that("the rebuild and export action clears the needs-export state", {
  fx <- .lk_fixture()
  conn <- open_db(tempfile(fileext = ".duckdb"))
  withr::defer(close_db(conn))
  out_dir <- withr::local_tempdir()
  persist_source_batch(conn, "B-lk", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  app_state <- .lk_state(fx, conn)
  app_state$cleaned_csv_path <- file.path(out_dir, "cleaned.csv")

  shiny::testServer(linkingServer, args = list(app_state = app_state), {
    session$setInputs(commit_matched = 1)
    expect_true(app_state$needs_export)

    session$setInputs(rebuild_export = 1)

    expect_false(app_state$needs_export)
    expect_equal(app_state$pending_confirmations, 0L)
    expect_false(app_state$last_export_failed)
    expect_equal(nrow(app_state$cleaned_links), 2L)
  })

  expect_true(file.exists(file.path(out_dir, "cleaned.csv")))
  expect_true("cleaned_links" %in% DBI::dbListTables(conn))
  # Source rows were written once, by the ingestion batch, and not again.
  expect_equal(.rows(conn, "vitek_ast"), nrow(fx$vitek_ast))
})

test_that("a failed export keeps the confirmations and the needs-export state", {
  fx <- .lk_fixture()
  conn <- open_db(tempfile(fileext = ".duckdb"))
  withr::defer(close_db(conn))
  persist_source_batch(conn, "B-lk", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  app_state <- .lk_state(fx, conn)

  # Point the CSV destination at a path whose parent cannot be created.
  blocked_parent <- tempfile()
  writeLines("not a directory", blocked_parent)
  app_state$cleaned_csv_path <- file.path(blocked_parent, "cleaned.csv")

  shiny::testServer(linkingServer, args = list(app_state = app_state), {
    session$setInputs(commit_matched = 1)
    expect_warning(session$setInputs(rebuild_export = 1),
                   "rebuild_export failed")

    expect_true(app_state$needs_export)
    expect_true(app_state$last_export_failed)
    expect_equal(app_state$pending_confirmations, 2L)
  })

  expect_equal(.rows(conn, "links_confirmed"), 2L)
})

test_that("confirming the same manual link twice saves one link and one audit event", {
  fx <- .lk_fixture()
  conn <- open_db(tempfile(fileext = ".duckdb"))
  withr::defer(close_db(conn))
  persist_source_batch(conn, "B-lk", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  app_state <- .lk_state(fx, conn)

  shiny::testServer(linkingServer, args = list(app_state = app_state), {
    session$setInputs(manual_vitek_key = "SYN004CRE1of1||1",
                      manual_os_identifier = "OS-004",
                      manual_link_reason = "legacy label")
    session$setInputs(save_manual_link = 1)
    expect_equal(app_state$pending_confirmations, 1L)

    session$setInputs(save_manual_link = 2)
    expect_equal(app_state$pending_confirmations, 1L)
  })

  expect_equal(.rows(conn, "links_confirmed"), 1L)
  expect_equal(.rows(conn, "edit_log"), 1L)
})
