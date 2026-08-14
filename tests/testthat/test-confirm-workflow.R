library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(lubridate)
library(purrr)

source("../../R/data_dedup.R")
source("../../R/data_clean.R")
source("../../R/data_export_cfu.R")
source("../../R/store.R")

# ── Synthetic fixture ─────────────────────────────────────────────────────────
# Everything below is generated. No analyst export, specimen identifier, or
# local DuckDB content is used.

.fixture <- function(n_isolates = 6L, drugs = 3L) {
  lab_ids <- sprintf("SYN%03dCRE1of1", seq_len(n_isolates))
  vitek_raw <- tibble::tibble(
    source_file = "synthetic.xlsx",
    source_row = seq_len(n_isolates),
    lab_id = lab_ids,
    isolate_number = "1",
    specimen_type = "Isolate",
    specimen_source = "Rectal",
    collection_date = as.Date("2026-01-01"),
    testing_date = as.Date("2026-01-03"),
    organism_name = "Klebsiella pneumoniae",
    parsed_study = "SYNTH",
    parsed_subject = sprintf("SYN%03d", seq_len(n_isolates)),
    parsed_target = "CRE",
    cp_hint = "Synthetic Protocol",
    n_drugs = drugs,
    ingested_at = as.POSIXct("2026-01-04 09:00:00", tz = "UTC"),
    file_name = "synthetic.xlsx"
  )
  vitek_ast <- tidyr::expand_grid(i = seq_len(n_isolates), d = seq_len(drugs)) |>
    dplyr::transmute(
      source_file = "synthetic.xlsx",
      source_row = .data$i,
      lab_id = lab_ids[.data$i],
      isolate_number = "1",
      drug_code = sprintf("D%d", .data$d),
      drug_name = sprintf("Drug %d", .data$d),
      mic = "<=1",
      call_instr = "S",
      call_expert = "S",
      result_mic = "<=1",
      result_instrument = "S",
      result_expertized = "S",
      ingested_at = as.POSIXct("2026-01-04 09:00:00", tz = "UTC")
    )
  specimens <- tibble::tibble(
    source_file = "synthetic_os.csv",
    source_row = seq_len(n_isolates),
    project_id = "SYNTH",
    os_identifier = sprintf("OS-%03d", seq_len(n_isolates)),
    specimen_label_raw = lab_ids,
    specimen_label = lab_ids,
    cp_short_title = "Synthetic Protocol",
    class = "Fluid",
    type = "Cryopreserved Cells",
    lineage = "Aliquot",
    parent_label = NA_character_,
    collection_dt = as.POSIXct("2026-01-01", tz = "UTC"),
    available_qty = 1,
    activity_status = "Active",
    anatomic_site = "Rectal",
    participant_id = sprintf("SYN%03d", seq_len(n_isolates)),
    custom_collection_date = as.Date("2026-01-01"),
    custom_organism = "Klebsiella pneumoniae",
    custom_parent_specimen_type = "Stool",
    custom_mdro = "CRE",
    has_quant = FALSE
  )
  list(
    vitek_raw = vitek_raw,
    vitek_ast = vitek_ast,
    vitek_unique = dedup_vitek(vitek_raw, "latest"),
    specimens = specimens,
    candidates = tibble::tibble(
      lab_id = lab_ids,
      isolate_number = "1",
      os_identifier = sprintf("OS-%03d", seq_len(n_isolates)),
      project_id = "SYNTH",
      specimen_label = lab_ids,
      cp_short_title = "Synthetic Protocol",
      score = 95
    )
  )
}

.tmp_db <- function(env = parent.frame()) {
  path <- tempfile(fileext = ".duckdb")
  conn <- open_db(path)
  withr::defer(close_db(conn), envir = env)
  list(path = path, conn = conn)
}

.count_rows <- function(conn, table_name) {
  if (!table_name %in% DBI::dbListTables(conn)) return(0L)
  as.integer(DBI::dbGetQuery(
    conn,
    paste0("SELECT COUNT(*) n FROM ", DBI::dbQuoteIdentifier(conn, table_name))
  )$n[[1]])
}

.source_counts <- function(conn) {
  vapply(c("vitek_raw", "vitek_ast", "specimens"),
         function(tb) .count_rows(conn, tb), integer(1))
}

# Replace a function in the environment store.R was sourced into, so we can see
# whether the confirmation path reaches the export code at all.
.spy_on <- function(name, env = environment(confirm_links), frame = parent.frame()) {
  original <- get(name, envir = env)
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  assign(name, function(...) {
    calls$n <- calls$n + 1L
    original(...)
  }, envir = env)
  withr::defer(assign(name, original, envir = env), envir = frame)
  calls
}

# ── Phase A: source persistence happens once per batch ────────────────────────

test_that("source tables are persisted once per batch and retries do not duplicate", {
  fx <- .fixture()
  db <- .tmp_db()

  first <- persist_source_batch(db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  expect_equal(unname(first), c(nrow(fx$vitek_raw), nrow(fx$vitek_ast), nrow(fx$specimens)))
  after_first <- .source_counts(db$conn)

  # A retry of the same batch is a no-op.
  second <- persist_source_batch(db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  expect_equal(unname(second), c(0L, 0L, 0L))
  expect_equal(.source_counts(db$conn), after_first)

  # Forcing a rewrite replaces this batch's rows rather than appending a copy.
  persist_source_batch(db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens,
                       force = TRUE)
  expect_equal(.source_counts(db$conn), after_first)

  # A different batch adds its own rows.
  persist_source_batch(db$conn, "B-2", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  expect_equal(.count_rows(db$conn, "vitek_raw"), 2L * nrow(fx$vitek_raw))
})

test_that("a re-parsed batch replaces its rows without needing force", {
  fx <- .fixture()
  db <- .tmp_db()
  persist_source_batch(db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)

  # The analyst loads another file, so the batch parses to more rows.
  bigger <- .fixture(n_isolates = 9L)
  persist_source_batch(db$conn, "B-1", bigger$vitek_raw, bigger$vitek_ast,
                       bigger$specimens)

  expect_equal(.count_rows(db$conn, "vitek_raw"), nrow(bigger$vitek_raw))
  expect_equal(.count_rows(db$conn, "vitek_ast"), nrow(bigger$vitek_ast))
  expect_equal(.count_rows(db$conn, "specimens"), nrow(bigger$specimens))
})

test_that("a corrected re-parse with the same row count still replaces the batch", {
  fx <- .fixture()
  db <- .tmp_db()
  persist_source_batch(db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)

  # Same shape, different content: the kind of change a row-count check misses.
  edited <- fx
  edited$vitek_raw$organism_name <- "Escherichia coli"
  edited$specimens$custom_organism <- "Escherichia coli"
  persist_source_batch(db$conn, "B-1", edited$vitek_raw, edited$vitek_ast,
                       edited$specimens)

  stored <- read_table(db$conn, "vitek_raw")
  expect_equal(nrow(stored), nrow(fx$vitek_raw))
  expect_true(all(stored$organism_name == "Escherichia coli"))
  expect_equal(.count_rows(db$conn, "specimens"), nrow(fx$specimens))
})

test_that("a ledger entry whose rows were removed forces a rewrite", {
  fx <- .fixture()
  db <- .tmp_db()
  persist_source_batch(db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)

  DBI::dbExecute(db$conn, "DELETE FROM vitek_raw WHERE batch_id = 'B-1'")
  expect_equal(.count_rows(db$conn, "vitek_raw"), 0L)

  persist_source_batch(db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  expect_equal(.count_rows(db$conn, "vitek_raw"), nrow(fx$vitek_raw))
})

test_that("a partially persisted batch can be retried without duplicating rows", {
  fx <- .fixture()
  db <- .tmp_db()

  # Simulate a failure after vitek_raw was written but before it was recorded.
  .append_table_aligned(db$conn, "vitek_raw", .with_batch(fx$vitek_raw, "B-1"))
  expect_equal(.count_rows(db$conn, "vitek_raw"), nrow(fx$vitek_raw))

  persist_source_batch(db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  expect_equal(.count_rows(db$conn, "vitek_raw"), nrow(fx$vitek_raw))
  expect_equal(.count_rows(db$conn, "vitek_ast"), nrow(fx$vitek_ast))
})

# ── Phase B: confirmations are cheap and idempotent ───────────────────────────

test_that("one confirmation inserts one logical link", {
  fx <- .fixture()
  db <- .tmp_db()
  persist_source_batch(db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)

  result <- confirm_links(db$conn, fx$candidates[1, ], "B-1",
                          match_method = "manual_selected")

  expect_equal(result$n_committed, 1L)
  expect_equal(.count_rows(db$conn, "links_confirmed"), 1L)
})

test_that("repeating the same confirmation does not insert a duplicate logical link", {
  fx <- .fixture()
  db <- .tmp_db()

  confirm_links(db$conn, fx$candidates[1, ], "B-1", match_method = "manual_selected")
  again <- confirm_links(db$conn, fx$candidates[1, ], "B-1", match_method = "manual_selected")

  expect_equal(again$n_committed, 0L)
  expect_equal(.count_rows(db$conn, "links_confirmed"), 1L)
})

test_that("a duplicated candidate inside one call still inserts a single link", {
  fx <- .fixture()
  db <- .tmp_db()

  result <- confirm_links(
    db$conn,
    dplyr::bind_rows(fx$candidates[1, ], fx$candidates[1, ]),
    "B-1", match_method = "manual_selected"
  )

  expect_equal(result$n_committed, 1L)
  expect_equal(.count_rows(db$conn, "links_confirmed"), 1L)
})

test_that("a manual confirmation logs one audit event only when a new link is saved", {
  fx <- .fixture()
  db <- .tmp_db()

  confirm_links(db$conn, fx$candidates[1, ], "B-1",
                match_method = "manual_selected", rationale = "legacy label")
  expect_equal(.count_rows(db$conn, "edit_log"), 1L)

  confirm_links(db$conn, fx$candidates[1, ], "B-1",
                match_method = "manual_selected", rationale = "legacy label")
  expect_equal(.count_rows(db$conn, "edit_log"), 1L)

  log <- read_table(db$conn, "edit_log")
  expect_equal(log$event_type, "link.manually_confirmed")
  expect_true(grepl("legacy label", log$to_value[[1]], fixed = TRUE))
})

test_that("automatic commits do not write manual-confirmation audit events", {
  fx <- .fixture()
  db <- .tmp_db()

  confirm_links(db$conn, fx$candidates, "B-1", match_method = "auto")

  expect_equal(.count_rows(db$conn, "links_confirmed"), nrow(fx$candidates))
  expect_equal(.count_rows(db$conn, "edit_log"), 0L)
})

test_that("confirming a link does not append source rows again", {
  fx <- .fixture()
  db <- .tmp_db()
  persist_source_batch(db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  before <- .source_counts(db$conn)

  for (i in seq_len(4)) {
    confirm_links(db$conn, fx$candidates[i, ], "B-1", match_method = "manual_selected")
  }

  expect_equal(.source_counts(db$conn), before)
  expect_equal(.count_rows(db$conn, "links_confirmed"), 4L)
})

test_that("confirming a link does not reach the CSV/XLSX export code", {
  fx <- .fixture()
  db <- .tmp_db()
  out_dir <- withr::local_tempdir()
  persist_source_batch(db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)

  spy <- .spy_on("export_cleaned_dataset")

  for (i in seq_len(3)) {
    confirm_links(db$conn, fx$candidates[i, ], "B-1", match_method = "manual_selected")
  }

  expect_equal(spy$n, 0L)
  expect_equal(length(list.files(out_dir, recursive = TRUE)), 0L)
  expect_false("cleaned_links" %in% DBI::dbListTables(db$conn))
  expect_false("cleaned_ast" %in% DBI::dbListTables(db$conn))
  expect_false("specimen_dataset" %in% DBI::dbListTables(db$conn))
})

test_that("several confirmations can be saved before one final rebuild", {
  fx <- .fixture()
  db <- .tmp_db()
  out_dir <- withr::local_tempdir()
  persist_source_batch(db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)

  spy <- .spy_on("export_cleaned_dataset")
  for (i in seq_len(5)) {
    confirm_links(db$conn, fx$candidates[i, ], "B-1", match_method = "manual_selected")
  }
  expect_equal(spy$n, 0L)

  result <- rebuild_and_export_cleaned(
    db$conn, batch_id = "B-1",
    vitek_unique = fx$vitek_unique, vitek_ast = fx$vitek_ast,
    specimens = fx$specimens, output_dir = out_dir
  )

  expect_equal(spy$n, 1L)
  expect_equal(nrow(result$cleaned_links), 5L)
  expect_equal(result$export_info$n_cleaned, 5L)
  expect_equal(.source_counts(db$conn),
               c(vitek_raw = nrow(fx$vitek_raw),
                 vitek_ast = nrow(fx$vitek_ast),
                 specimens = nrow(fx$specimens)))
})

test_that("confirmed links survive reopening the DuckDB connection before export", {
  fx <- .fixture()
  path <- tempfile(fileext = ".duckdb")

  conn <- open_db(path)
  persist_source_batch(conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  confirm_links(conn, fx$candidates[1:3, ], "B-1", match_method = "manual_selected")
  close_db(conn)

  conn2 <- open_db(path)
  withr::defer(close_db(conn2))

  expect_equal(.count_rows(conn2, "links_confirmed"), 3L)
  expect_equal(.count_rows(conn2, "edit_log"), 3L)

  rebuilt <- rebuild_cleaned(conn2, vitek_unique = fx$vitek_unique,
                             vitek_ast = fx$vitek_ast, specimens = fx$specimens)
  expect_equal(nrow(rebuilt$cleaned_links), 3L)
})

test_that("a failed export leaves confirmed links and audit records intact", {
  fx <- .fixture()
  db <- .tmp_db()
  persist_source_batch(db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  confirm_links(db$conn, fx$candidates[1:3, ], "B-1", match_method = "manual_selected")

  # An output directory that is actually a file: dir.create() cannot create it
  # and the CSV write fails.
  blocked <- tempfile()
  writeLines("not a directory", blocked)

  expect_error(rebuild_and_export_cleaned(
    db$conn, batch_id = "B-1",
    vitek_unique = fx$vitek_unique, vitek_ast = fx$vitek_ast,
    specimens = fx$specimens, output_dir = blocked
  ))

  expect_equal(.count_rows(db$conn, "links_confirmed"), 3L)
  expect_equal(.count_rows(db$conn, "edit_log"), 3L)

  # …and a retry to a writable location succeeds without reconfirming anything.
  out_dir <- withr::local_tempdir()
  retried <- rebuild_and_export_cleaned(
    db$conn, batch_id = "B-1",
    vitek_unique = fx$vitek_unique, vitek_ast = fx$vitek_ast,
    specimens = fx$specimens, output_dir = out_dir
  )
  expect_equal(nrow(retried$cleaned_links), 3L)
})

# ── Phase C: export equivalence with the pre-change pipeline ──────────────────

test_that("the split pipeline exports the same cleaned content as the pre-change pipeline", {
  fx <- .fixture()

  # -- New pipeline: persist once, confirm each link, export once -------------
  new_dir <- withr::local_tempdir()
  new_db <- .tmp_db()
  persist_source_batch(new_db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  for (i in seq_len(nrow(fx$candidates))) {
    confirm_links(new_db$conn, fx$candidates[i, ], "B-1",
                  match_method = "manual_selected", created_by = "analyst",
                  rationale = "equivalence")
  }
  write_overrides(
    new_db$conn,
    overrides = tibble::tibble(
      link_id = read_table(new_db$conn, "links_confirmed")$link_id[[1]],
      field = "lab_id", cleaned_value = "SYN001CRE1of1-fixed",
      source_hint = "manual", rationale = "typo",
      edited_at = as.POSIXct("2026-02-01 10:00:00", tz = "UTC"),
      edited_by = "analyst"
    ),
    edit_log = NULL
  )
  new_result <- rebuild_and_export_cleaned(
    new_db$conn, batch_id = "B-1",
    vitek_unique = fx$vitek_unique, vitek_ast = fx$vitek_ast,
    specimens = fx$specimens, output_dir = new_dir
  )

  # -- Pre-change pipeline, written out step by step as it used to run --------
  old_dir <- withr::local_tempdir()
  old_db <- .tmp_db()
  links <- read_table(new_db$conn, "links_confirmed")
  write_links(old_db$conn, links)
  write_ingested_tables(old_db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  old_links <- read_table(old_db$conn, "links_confirmed")
  old_overrides <- read_table(new_db$conn, "cleaned_overrides")
  old_cleaned <- build_cleaned(links = old_links, overrides = old_overrides,
                               vitek = fx$vitek_unique, specimens = fx$specimens)
  old_ast <- build_cleaned_ast(old_cleaned, fx$vitek_ast)
  old_export <- export_cleaned_dataset(
    cleaned = old_cleaned, cleaned_ast = old_ast, batch_id = "B-1",
    specimens = fx$specimens, output_dir = old_dir,
    formats = c("csv", "duckdb"), conn = old_db$conn
  )

  expect_equal(new_result$export_info$n_cleaned, old_export$n_cleaned)
  expect_equal(new_result$export_info$n_ast, old_export$n_ast)
  expect_equal(new_result$export_info$n_specimens, old_export$n_specimens)

  for (kind in c("_isolates.csv", "_isolates_ast.csv", "_isolates_specimens.csv")) {
    new_csv <- readr::read_csv(file.path(new_dir, paste0("AXIS_clean_B-1", kind)),
                               show_col_types = FALSE, progress = FALSE)
    old_csv <- readr::read_csv(file.path(old_dir, paste0("AXIS_clean_B-1", kind)),
                               show_col_types = FALSE, progress = FALSE)
    expect_equal(new_csv, old_csv, info = kind)
  }

  new_tbl <- read_table(new_db$conn, "cleaned_links") |> dplyr::arrange(link_id)
  old_tbl <- read_table(old_db$conn, "cleaned_links") |> dplyr::arrange(link_id)
  expect_equal(nrow(new_tbl), nrow(old_tbl))
  expect_equal(new_tbl$clean_lab_id, old_tbl$clean_lab_id)
})

# ── Typo correction and source preservation ──────────────────────────────────

test_that("an Edit mode override corrects the cleaned lab ID and survives rebuilds", {
  fx <- .fixture()
  db <- .tmp_db()
  out_dir <- withr::local_tempdir()

  persist_source_batch(db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)

  # The analyst manually links a typo'd Vitek record, then corrects the
  # cleaned ID in Edit mode.
  typo <- fx$candidates[1, ]
  confirm_links(db$conn, typo, "B-1", match_method = "manual_selected",
                rationale = "typo in Vitek lab ID")
  link_id <- read_table(db$conn, "links_confirmed")$link_id[[1]]

  before <- "SYN001CRE1of1"
  after <- "SYN001CRE1of1-corrected"
  write_overrides(
    db$conn,
    overrides = tibble::tibble(
      link_id = link_id, field = "lab_id", cleaned_value = after,
      source_hint = "manual", rationale = "1o1 typo",
      edited_at = as.POSIXct("2026-02-01 10:00:00", tz = "UTC"),
      edited_by = "analyst"
    ),
    edit_log = tibble::tibble(
      event_id = "E1", link_id = link_id, event_type = "field.edited",
      field = "lab_id", from_value = before, to_value = after,
      who = "analyst", when_ts = as.POSIXct("2026-02-01 10:00:00", tz = "UTC")
    )
  )

  rebuilt <- rebuild_cleaned(db$conn, vitek_unique = fx$vitek_unique,
                             vitek_ast = fx$vitek_ast, specimens = fx$specimens)

  # The correction is applied…
  expect_equal(rebuilt$cleaned_links$clean_lab_id[[1]], after)
  # …the original Vitek value is retained in the linked output…
  expect_equal(rebuilt$cleaned_links$lab_id[[1]], before)
  # …and the source table is untouched.
  raw <- read_table(db$conn, "vitek_raw")
  expect_true(before %in% raw$lab_id)
  expect_false(after %in% raw$lab_id)

  # The edit log carries before, after, analyst, and timestamp.
  log <- read_table(db$conn, "edit_log") |>
    dplyr::filter(.data$event_type == "field.edited")
  expect_equal(log$from_value, before)
  expect_equal(log$to_value, after)
  expect_equal(log$who, "analyst")
  expect_false(is.na(log$when_ts[[1]]))

  # Rebuilding and exporting again preserves the override.
  exported <- rebuild_and_export_cleaned(
    db$conn, batch_id = "B-1",
    vitek_unique = fx$vitek_unique, vitek_ast = fx$vitek_ast,
    specimens = fx$specimens, output_dir = out_dir
  )
  expect_equal(exported$cleaned_links$clean_lab_id[[1]], after)

  round_trip <- readr::read_csv(file.path(out_dir, "AXIS_clean_B-1_isolates.csv"),
                                show_col_types = FALSE, progress = FALSE)
  expect_true(after %in% round_trip$clean_lab_id)
  expect_equal(.count_rows(db$conn, "vitek_raw"), nrow(fx$vitek_raw))
})

test_that("a later override wins and the source rows are still unchanged", {
  fx <- .fixture()
  db <- .tmp_db()
  persist_source_batch(db$conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  confirm_links(db$conn, fx$candidates[1, ], "B-1", match_method = "manual_selected")
  link_id <- read_table(db$conn, "links_confirmed")$link_id[[1]]

  for (i in seq_len(2)) {
    write_overrides(db$conn, overrides = tibble::tibble(
      link_id = link_id, field = "lab_id",
      cleaned_value = paste0("SYN001CRE1of1-v", i),
      source_hint = "manual", rationale = "",
      edited_at = as.POSIXct("2026-02-01 10:00:00", tz = "UTC") + i * 60,
      edited_by = "analyst"
    ), edit_log = NULL)
  }

  rebuilt <- rebuild_cleaned(db$conn, vitek_unique = fx$vitek_unique,
                             vitek_ast = fx$vitek_ast, specimens = fx$specimens)
  expect_equal(rebuilt$cleaned_links$clean_lab_id[[1]], "SYN001CRE1of1-v2")
  expect_equal(.count_rows(db$conn, "vitek_raw"), nrow(fx$vitek_raw))
  expect_equal(.count_rows(db$conn, "specimens"), nrow(fx$specimens))
})

# ── Backwards compatibility ──────────────────────────────────────────────────

test_that("the retained commit_matched_links composite no longer multiplies source rows", {
  fx <- .fixture()
  db <- .tmp_db()
  out_dir <- withr::local_tempdir()

  for (i in seq_len(3)) {
    commit_matched_links(
      db$conn, fx$candidates[i, ], "B-1",
      vitek_raw = fx$vitek_raw, vitek_ast = fx$vitek_ast,
      vitek_unique = fx$vitek_unique, specimens = fx$specimens,
      output_dir = out_dir, formats = c("csv", "duckdb"),
      match_method = "manual_selected"
    )
  }

  expect_equal(.count_rows(db$conn, "links_confirmed"), 3L)
  expect_equal(.source_counts(db$conn),
               c(vitek_raw = nrow(fx$vitek_raw),
                 vitek_ast = nrow(fx$vitek_ast),
                 specimens = nrow(fx$specimens)))
})
