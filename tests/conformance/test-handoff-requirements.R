# ─────────────────────────────────────────────────────────────────────────────
# Neutral conformance harness for HANDOFF_CEDAR_LINKING_2026-08.md, Workstream B.
#
# One test file, run unchanged against both implementations. A thin adapter
# binds whichever persistence API the branch provides:
#
#   claude/link-confirm-performance   persist_source_batch / confirm_links /
#                                     rebuild_cleaned / rebuild_and_export_cleaned
#   PR #14 (codex)                    persist_source_batch_once / record_confirmed_links /
#                                     rebuild_cleaned_data / rebuild_and_export_cleaned_data
#
# Every assertion below traces to a bullet the handoff states explicitly, so
# neither branch's own naming or design choices decide the outcome.
# Synthetic records only.
# ─────────────────────────────────────────────────────────────────────────────

library(dplyr); library(tibble); library(tidyr)
library(readr); library(lubridate); library(purrr)

ROOT <- Sys.getenv("AXIS_XCHECK_ROOT", unset = "..")
for (f in c("data_dedup.R", "data_clean.R", "data_export_cfu.R", "store.R")) {
  source(file.path(ROOT, "R", f))
}

.has <- function(f) exists(f, mode = "function")

API <- if (.has("confirm_links")) "claude" else if (.has("record_confirmed_links")) "codex" else
  stop("Neither confirmation API is present.")
message("Conformance harness running against: ", API)

# ── Adapter ──────────────────────────────────────────────────────────────────

api_persist <- function(conn, batch_id, vr, va, sp) {
  if (API == "claude") persist_source_batch(conn, batch_id, vr, va, sp)
  else persist_source_batch_once(conn, batch_id, vr, va, sp)
}

api_confirm <- function(conn, matched, batch_id, match_method = "auto",
                        created_by = "analyst", rationale = "") {
  if (API == "claude") {
    r <- confirm_links(conn, matched, batch_id, match_method, created_by, rationale)
    list(n = r$n_committed, inserted = r$inserted)
  } else {
    r <- record_confirmed_links(conn, matched, batch_id, match_method, created_by, rationale)
    list(n = r$n_committed, inserted = r$inserted_links)
  }
}

api_rebuild <- function(conn, vu, va, sp) {
  if (API == "claude") rebuild_cleaned(conn, vu, va, sp)
  else rebuild_cleaned_data(conn, vu, va, sp)
}

api_export <- function(conn, batch_id, vu, va, sp, output_dir,
                       formats = c("csv", "xlsx", "duckdb")) {
  if (API == "claude") {
    rebuild_and_export_cleaned(conn, batch_id, vu, va, sp,
                               output_dir = output_dir, formats = formats)
  } else {
    rebuild_and_export_cleaned_data(conn, batch_id, vu, va, sp,
                                    output_dir = output_dir, formats = formats)
  }
}

# ── Fixture ──────────────────────────────────────────────────────────────────

.fixture <- function(n = 6L, drugs = 3L) {
  lab_ids <- sprintf("SYN%03dCRE1of1", seq_len(n))
  vitek_raw <- tibble::tibble(
    source_file = "synthetic.xlsx", source_row = seq_len(n),
    lab_id = lab_ids, isolate_number = "1",
    specimen_type = "Isolate", specimen_source = "Rectal",
    collection_date = as.Date("2026-01-01"), testing_date = as.Date("2026-01-03"),
    organism_name = "Klebsiella pneumoniae", parsed_study = "SYNTH",
    parsed_subject = sprintf("SYN%03d", seq_len(n)), parsed_target = "CRE",
    cp_hint = "Synthetic Protocol", n_drugs = drugs,
    ingested_at = as.POSIXct("2026-01-04 09:00:00", tz = "UTC"),
    file_name = "synthetic.xlsx"
  )
  vitek_ast <- tidyr::expand_grid(i = seq_len(n), d = seq_len(drugs)) |>
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
  list(
    vitek_raw = vitek_raw, vitek_ast = vitek_ast,
    vitek_unique = dedup_vitek(vitek_raw, "latest"), specimens = specimens,
    candidates = tibble::tibble(
      lab_id = lab_ids, isolate_number = "1",
      os_identifier = sprintf("OS-%03d", seq_len(n)), project_id = "SYNTH",
      specimen_label = lab_ids, cp_short_title = "Synthetic Protocol", score = 95
    )
  )
}

.db <- function(env = parent.frame()) {
  conn <- open_db(tempfile(fileext = ".duckdb"))
  withr::defer(close_db(conn), envir = env)
  conn
}

.n <- function(conn, tb) {
  if (!tb %in% DBI::dbListTables(conn)) return(0L)
  as.integer(DBI::dbGetQuery(
    conn, paste0("SELECT COUNT(*) n FROM ", DBI::dbQuoteIdentifier(conn, tb)))$n[[1]])
}

.src <- function(conn) vapply(c("vitek_raw", "vitek_ast", "specimens"),
                              function(tb) .n(conn, tb), integer(1))

.spy <- function(name, frame = parent.frame()) {
  env <- environment(get(if (API == "claude") "confirm_links" else "record_confirmed_links"))
  original <- get(name, envir = env)
  calls <- new.env(parent = emptyenv()); calls$n <- 0L
  assign(name, function(...) { calls$n <- calls$n + 1L; original(...) }, envir = env)
  withr::defer(assign(name, original, envir = env), envir = frame)
  calls
}

# ── Handoff §Confirmation persistence tests ──────────────────────────────────

test_that("R1 one confirmation inserts one logical link", {
  fx <- .fixture(); conn <- .db()
  api_persist(conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  r <- api_confirm(conn, fx$candidates[1, ], "B-1", "manual_selected")
  expect_equal(r$n, 1L)
  expect_equal(.n(conn, "links_confirmed"), 1L)
})

test_that("R2 repeating the same confirmation inserts no duplicate logical link", {
  fx <- .fixture(); conn <- .db()
  api_confirm(conn, fx$candidates[1, ], "B-1", "manual_selected")
  again <- api_confirm(conn, fx$candidates[1, ], "B-1", "manual_selected")
  expect_equal(again$n, 0L)
  expect_equal(.n(conn, "links_confirmed"), 1L)
})

test_that("R3 a manual confirmation creates one audit event only when a new link is saved", {
  fx <- .fixture(); conn <- .db()
  api_confirm(conn, fx$candidates[1, ], "B-1", "manual_selected", rationale = "legacy label")
  expect_equal(.n(conn, "edit_log"), 1L)
  api_confirm(conn, fx$candidates[1, ], "B-1", "manual_selected", rationale = "legacy label")
  expect_equal(.n(conn, "edit_log"), 1L)
})

test_that("R4 confirming a link does not append source rows again", {
  fx <- .fixture(); conn <- .db()
  api_persist(conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  before <- .src(conn)
  for (i in 1:4) api_confirm(conn, fx$candidates[i, ], "B-1", "manual_selected")
  expect_equal(.src(conn), before)
})

test_that("R5 confirming a link does not reach the CSV or XLSX export code", {
  fx <- .fixture(); conn <- .db()
  out <- withr::local_tempdir()
  api_persist(conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  spy <- .spy("export_cleaned_dataset")
  for (i in 1:3) api_confirm(conn, fx$candidates[i, ], "B-1", "manual_selected")
  expect_equal(spy$n, 0L)
  expect_equal(length(list.files(out, recursive = TRUE)), 0L)
  expect_false("cleaned_links" %in% DBI::dbListTables(conn))
})

test_that("R6 several confirmations can be saved before one final rebuild", {
  fx <- .fixture(); conn <- .db()
  out <- withr::local_tempdir()
  api_persist(conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  spy <- .spy("export_cleaned_dataset")
  for (i in 1:5) api_confirm(conn, fx$candidates[i, ], "B-1", "manual_selected")
  expect_equal(spy$n, 0L)
  res <- api_export(conn, "B-1", fx$vitek_unique, fx$vitek_ast, fx$specimens, out)
  expect_equal(spy$n, 1L)
  expect_equal(nrow(res$cleaned_links), 5L)
})

test_that("R7 confirmed links survive reopening the DuckDB connection before export", {
  fx <- .fixture()
  path <- tempfile(fileext = ".duckdb")
  conn <- open_db(path)
  api_persist(conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  api_confirm(conn, fx$candidates[1:3, ], "B-1", "manual_selected")
  close_db(conn)

  conn2 <- open_db(path); withr::defer(close_db(conn2))
  expect_equal(.n(conn2, "links_confirmed"), 3L)
  expect_equal(.n(conn2, "edit_log"), 3L)
  expect_equal(nrow(api_rebuild(conn2, fx$vitek_unique, fx$vitek_ast, fx$specimens)$cleaned_links), 3L)
})

test_that("R8 a failed export does not remove confirmed links or audit records", {
  fx <- .fixture(); conn <- .db()
  api_persist(conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  api_confirm(conn, fx$candidates[1:3, ], "B-1", "manual_selected")

  blocked <- tempfile(); writeLines("not a directory", blocked)
  expect_error(api_export(conn, "B-1", fx$vitek_unique, fx$vitek_ast, fx$specimens, blocked))

  expect_equal(.n(conn, "links_confirmed"), 3L)
  expect_equal(.n(conn, "edit_log"), 3L)

  out <- withr::local_tempdir()
  retried <- api_export(conn, "B-1", fx$vitek_unique, fx$vitek_ast, fx$specimens, out)
  expect_equal(nrow(retried$cleaned_links), 3L)
})

test_that("R9 a successful export matches the pre-change pipeline for the same links and overrides", {
  fx <- .fixture()
  new_dir <- withr::local_tempdir(); old_dir <- withr::local_tempdir()
  conn <- .db()
  api_persist(conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  for (i in seq_len(nrow(fx$candidates))) {
    api_confirm(conn, fx$candidates[i, ], "B-1", "manual_selected", rationale = "equivalence")
  }
  write_overrides(conn, overrides = tibble::tibble(
    link_id = read_table(conn, "links_confirmed")$link_id[[1]],
    field = "lab_id", cleaned_value = "SYN001CRE1of1-fixed",
    source_hint = "manual", rationale = "typo",
    edited_at = as.POSIXct("2026-02-01 10:00:00", tz = "UTC"), edited_by = "analyst"
  ), edit_log = NULL)

  new_res <- api_export(conn, "B-1", fx$vitek_unique, fx$vitek_ast, fx$specimens, new_dir)

  # The pre-change sequence, written out as main ran it.
  links <- read_table(conn, "links_confirmed")
  overrides <- read_table(conn, "cleaned_overrides")
  old_cleaned <- build_cleaned(links = links, overrides = overrides,
                               vitek = fx$vitek_unique, specimens = fx$specimens)
  old_ast <- build_cleaned_ast(old_cleaned, fx$vitek_ast)
  old_export <- export_cleaned_dataset(
    cleaned = old_cleaned, cleaned_ast = old_ast, batch_id = "B-1",
    specimens = fx$specimens, output_dir = old_dir, formats = "csv", conn = NULL)

  expect_equal(new_res$export_info$n_cleaned, old_export$n_cleaned)
  expect_equal(new_res$export_info$n_ast, old_export$n_ast)
  expect_equal(new_res$export_info$n_specimens, old_export$n_specimens)
  for (kind in c("_isolates.csv", "_isolates_ast.csv", "_isolates_specimens.csv")) {
    a <- readr::read_csv(file.path(new_dir, paste0("AXIS_clean_B-1", kind)),
                         show_col_types = FALSE, progress = FALSE)
    b <- readr::read_csv(file.path(old_dir, paste0("AXIS_clean_B-1", kind)),
                         show_col_types = FALSE, progress = FALSE)
    expect_equal(a, b, info = kind)
  }
})

# ── Handoff §Typo and provenance tests ───────────────────────────────────────

test_that("P1 a manually linked typo retains the original VITEK lab_id and the override wins", {
  fx <- .fixture(); conn <- .db()
  out <- withr::local_tempdir()
  api_persist(conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  api_confirm(conn, fx$candidates[1, ], "B-1", "manual_selected", rationale = "1o1 typo")
  link_id <- read_table(conn, "links_confirmed")$link_id[[1]]

  before <- "SYN001CRE1of1"; after <- "SYN001CRE1of1-corrected"
  write_overrides(conn,
    overrides = tibble::tibble(
      link_id = link_id, field = "lab_id", cleaned_value = after,
      source_hint = "manual", rationale = "1o1 typo",
      edited_at = as.POSIXct("2026-02-01 10:00:00", tz = "UTC"), edited_by = "analyst"),
    edit_log = tibble::tibble(
      event_id = "E1", link_id = link_id, event_type = "field.edited",
      field = "lab_id", from_value = before, to_value = after,
      who = "analyst", when_ts = as.POSIXct("2026-02-01 10:00:00", tz = "UTC")))

  rebuilt <- api_rebuild(conn, fx$vitek_unique, fx$vitek_ast, fx$specimens)
  expect_equal(rebuilt$cleaned_links$clean_lab_id[[1]], after)
  expect_equal(rebuilt$cleaned_links$lab_id[[1]], before)

  raw <- read_table(conn, "vitek_raw")
  expect_true(before %in% raw$lab_id)
  expect_false(after %in% raw$lab_id)

  log <- read_table(conn, "edit_log") |> dplyr::filter(.data$event_type == "field.edited")
  expect_equal(log$from_value, before); expect_equal(log$to_value, after)
  expect_equal(log$who, "analyst"); expect_false(is.na(log$when_ts[[1]]))

  exported <- api_export(conn, "B-1", fx$vitek_unique, fx$vitek_ast, fx$specimens, out)
  expect_equal(exported$cleaned_links$clean_lab_id[[1]], after)
  expect_equal(.n(conn, "vitek_raw"), nrow(fx$vitek_raw))
})

# ── Handoff §Phase A: source persistence is once-per-batch and retry-safe ────

test_that("A1 re-persisting an unchanged batch does not duplicate source rows", {
  fx <- .fixture(); conn <- .db()
  api_persist(conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  after_first <- .src(conn)
  api_persist(conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  expect_equal(.src(conn), after_first)
})

test_that("A2 a partially persisted batch can be retried without duplicating rows", {
  fx <- .fixture(); conn <- .db()
  .append_table_aligned(conn, "vitek_raw", .with_batch(fx$vitek_raw, "B-1"))
  api_persist(conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)
  expect_equal(.n(conn, "vitek_raw"), nrow(fx$vitek_raw))
  expect_equal(.n(conn, "vitek_ast"), nrow(fx$vitek_ast))
})

test_that("A3 re-persisting a batch whose parsed content changed replaces it, not appends", {
  fx <- .fixture(); conn <- .db()
  api_persist(conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)

  # The analyst loads one more file, so the batch now parses to more rows.
  bigger <- .fixture(n = 8L)
  api_persist(conn, "B-1", bigger$vitek_raw, bigger$vitek_ast, bigger$specimens)

  expect_equal(.n(conn, "vitek_raw"), nrow(bigger$vitek_raw))
  expect_equal(.n(conn, "specimens"), nrow(bigger$specimens))
})

test_that("A4 re-persisting a same-sized but different batch does not leave stale rows", {
  fx <- .fixture(); conn <- .db()
  api_persist(conn, "B-1", fx$vitek_raw, fx$vitek_ast, fx$specimens)

  # Same row count, different content: a re-parse of corrected source files.
  edited <- fx
  edited$vitek_raw$organism_name <- "Escherichia coli"
  edited$specimens$custom_organism <- "Escherichia coli"
  api_persist(conn, "B-1", edited$vitek_raw, edited$vitek_ast, edited$specimens)

  stored <- read_table(conn, "vitek_raw")
  expect_equal(.n(conn, "vitek_raw"), nrow(fx$vitek_raw))
  expect_true(all(stored$organism_name == "Escherichia coli"))
})
