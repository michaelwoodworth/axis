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

# ── Removing a confirmed link ────────────────────────────────────────────────

test_that("the remove-link action deletes the link and restores the isolate", {
  fx <- .lk_fixture()
  conn <- open_db(tempfile(fileext = ".duckdb"))
  withr::defer(close_db(conn))
  persist_source_batch(conn, "B-lk", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  app_state <- .lk_state(fx, conn)

  shiny::testServer(linkingServer, args = list(app_state = app_state), {
    session$setInputs(commit_matched = 1)
    expect_equal(nrow(app_state$links_confirmed), 2L)

    doomed <- app_state$links_confirmed[1, ]
    rv$selected_id <- doomed$link_id
    session$setInputs(unlink_reason = "wrong OpenSpecimen record",
                      btn_unlink_confirm = 1)

    expect_equal(nrow(app_state$links_confirmed), 1L)
    expect_false(doomed$link_id %in% app_state$links_confirmed$link_id)
    expect_null(rv$selected_id)

    # The isolate has to be actionable again, not just absent.
    expect_true(doomed$lab_id %in% app_state$match_buckets$none$lab_id)

    # And the export is stale until it is rebuilt.
    expect_true(app_state$needs_export)
  })

  expect_equal(.rows(conn, "links_confirmed"), 1L)

  audit <- tibble::as_tibble(read_table(conn, "edit_log")) |>
    dplyr::filter(event_type == "link.retracted")
  expect_equal(nrow(audit), 1L)
  expect_match(audit$to_value[[1]], "wrong OpenSpecimen record")
})

test_that("removing a link lets the isolate be linked to another specimen", {
  fx <- .lk_fixture()
  conn <- open_db(tempfile(fileext = ".duckdb"))
  withr::defer(close_db(conn))
  persist_source_batch(conn, "B-lk", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  app_state <- .lk_state(fx, conn)

  shiny::testServer(linkingServer, args = list(app_state = app_state), {
    session$setInputs(commit_matched = 1)
    doomed <- app_state$links_confirmed[1, ]

    # While the link stands, a different specimen for that isolate is refused.
    rival <- tibble::tibble(
      lab_id = doomed$lab_id, isolate_number = doomed$isolate_number,
      os_identifier = "OS-OTHER", project_id = doomed$project_id,
      specimen_label = doomed$specimen_label,
      cp_short_title = doomed$cp_short_title, score = 92
    )
    blocked <- confirm_links(conn, rival, batch_id = "B-lk",
                             match_method = "manual_selected")
    expect_equal(blocked$n_committed, 0L)
    expect_equal(blocked$n_conflicted, 1L)

    rv$selected_id <- doomed$link_id
    session$setInputs(btn_unlink_confirm = 1)

    # Once it is removed, the corrected link goes in.
    fixed <- confirm_links(conn, rival, batch_id = "B-lk",
                           match_method = "manual_selected")
    expect_equal(fixed$n_committed, 1L)
    expect_equal(fixed$n_conflicted, 0L)
  })

  links <- tibble::as_tibble(read_table(conn, "links_confirmed"))
  expect_equal(nrow(links), 2L)
  expect_true("OS-OTHER" %in% links$os_identifier)
})

test_that("removing an already-removed link changes nothing", {
  fx <- .lk_fixture()
  conn <- open_db(tempfile(fileext = ".duckdb"))
  withr::defer(close_db(conn))
  persist_source_batch(conn, "B-lk", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  app_state <- .lk_state(fx, conn)

  shiny::testServer(linkingServer, args = list(app_state = app_state), {
    session$setInputs(commit_matched = 1)
    doomed_id <- app_state$links_confirmed$link_id[[1]]

    rv$selected_id <- doomed_id
    session$setInputs(btn_unlink_confirm = 1)
    expect_equal(nrow(app_state$links_confirmed), 1L)

    rv$selected_id <- doomed_id
    session$setInputs(btn_unlink_confirm = 2)
    expect_equal(nrow(app_state$links_confirmed), 1L)
  })

  expect_equal(.rows(conn, "links_confirmed"), 1L)
})

test_that("the discarded field edits path still only clears pending edits", {
  fx <- .lk_fixture()
  conn <- open_db(tempfile(fileext = ".duckdb"))
  withr::defer(close_db(conn))
  persist_source_batch(conn, "B-lk", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  app_state <- .lk_state(fx, conn)

  shiny::testServer(linkingServer, args = list(app_state = app_state), {
    session$setInputs(commit_matched = 1)
    before <- nrow(app_state$links_confirmed)

    rv$pending_edits <- list(organism = "Escherichia coli")
    session$setInputs(btn_revert = 1)

    expect_equal(length(rv$pending_edits), 0L)
    # Discarding edits must never remove a link — that is what the separate
    # remove-link action is for.
    expect_equal(nrow(app_state$links_confirmed), before)
  })
})

# ── One click must produce exactly one Shiny event ───────────────────────────
# A raw tags$button carrying BOTH the action-button class and an inline
# Shiny.setInputValue onclick sends two events for one click. For an idempotent
# handler that is invisible; for one that calls showModal() it opens two
# dialogs, and only the first is bound, so the visible confirm button is inert.
# That is exactly how the remove-link action failed in the field.

.lk_ns <- function(id) paste0("linking-", id)

.lk_button_tags <- function(html) {
  regmatches(html, gregexpr("<button[^>]*>", html))[[1]]
}

test_that("no detail-rail button carries both an action-button class and an onclick", {
  for (edit_mode in c(FALSE, TRUE)) {
    html <- as.character(link_action_buttons(.lk_ns, "L-1", edit_mode))
    offenders <- Filter(
      function(tag) grepl("action-button", tag, fixed = TRUE) &&
                    grepl("onclick=", tag, fixed = TRUE),
      .lk_button_tags(html)
    )
    expect_equal(
      as.character(offenders), character(),
      info = sprintf("edit_mode = %s: these buttons fire twice per click", edit_mode)
    )
  }
})

test_that("the remove-link button is present outside edit mode and bound once", {
  html <- as.character(link_action_buttons(.lk_ns, "L-1", edit_mode = FALSE))
  tags <- .lk_button_tags(html)

  unlink_tags <- Filter(function(t) grepl('id="linking-btn_unlink"', t, fixed = TRUE), tags)
  expect_length(unlink_tags, 1L)
  expect_true(grepl("action-button", unlink_tags[[1]], fixed = TRUE))
  expect_false(grepl("onclick=", unlink_tags[[1]], fixed = TRUE))
  expect_match(html, "Remove link")

  # Save and Discard belong to edit mode only.
  expect_false(grepl('id="linking-btn_save"', html, fixed = TRUE))
})

test_that("edit mode adds save and discard, still one binding each", {
  html <- as.character(link_action_buttons(.lk_ns, "L-1", edit_mode = TRUE))
  tags <- .lk_button_tags(html)

  for (id in c("linking-btn_save", "linking-btn_revert", "linking-btn_unlink")) {
    hits <- Filter(function(t) grepl(sprintf('id="%s"', id), t, fixed = TRUE), tags)
    expect_length(hits, 1L)
    expect_false(grepl("onclick=", hits[[1]], fixed = TRUE))
  }
  expect_match(html, "Discard edits", fixed = TRUE)
  # The old label promised something the button never did.
  expect_false(grepl(">Revert<", html, fixed = TRUE))
})

test_that("a staged candidate gets no action buttons", {
  expect_equal(.lk_button_tags(as.character(link_action_buttons(.lk_ns, "staged::abc", TRUE))),
               character())
  expect_equal(.lk_button_tags(as.character(link_action_buttons(.lk_ns, NULL, TRUE))),
               character())
})
