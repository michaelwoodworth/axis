library(dplyr)
library(tibble)

source("../../R/data_clean.R")
source("../../R/store.R")

.perf_candidate <- function(id = "1") {
  tibble::tibble(
    lab_id = paste0("SYN", id, "ESBL1"),
    isolate_number = "1",
    os_identifier = paste0("OS", id),
    project_id = "SYNTHETIC",
    specimen_label = paste0("SYN", id, "ESBL1"),
    cp_short_title = "Synthetic CP",
    score = 95
  )
}

.perf_vitek <- function(ids = "1") {
  tibble::tibble(
    lab_id = paste0("SYN", ids, "ESBL1"), isolate_number = "1",
    organism_name = "Escherichia coli", specimen_type = "Isolate",
    specimen_source = "Synthetic", collection_date = as.Date("2026-01-01"),
    testing_date = as.Date("2026-01-02"), parsed_study = "SYNTHETIC",
    parsed_subject = paste0("SYN", ids), parsed_target = "ESBL",
    cp_hint = "Synthetic CP", n_drugs = 1L, file_name = "synthetic.xlsx"
  )
}

.perf_ast <- function(ids = "1") {
  tibble::tibble(
    source_file = "synthetic.xlsx", source_row = seq_along(ids),
    lab_id = paste0("SYN", ids, "ESBL1"), isolate_number = "1",
    drug_code = "AM", drug_name = "Ampicillin", mic = ">=32",
    call_instr = "R", call_expert = "R",
    result_mic = ">=32", result_instrument = "R", result_expertized = "R",
    ingested_at = as.POSIXct("2026-01-02 12:00:00", tz = "UTC")
  )
}

.perf_specimens <- function(ids = "1") {
  tibble::tibble(
    project_id = "SYNTHETIC", os_identifier = paste0("OS", ids),
    specimen_label_raw = paste0("SYN", ids, "ESBL1"),
    specimen_label = paste0("SYN", ids, "ESBL1"), cp_short_title = "Synthetic CP",
    parent_label = NA_character_, participant_id = paste0("SYN", ids),
    type = "Cryopreserved Cells", class = "Aliquot", lineage = "Derived",
    custom_collection_date = as.Date("2026-01-01"),
    custom_organism = "Escherichia coli", custom_mdro = "ESBL",
    custom_parent_specimen_type = "Stool"
  )
}

.with_perf_db <- function(code) {
  expr <- substitute(code)
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("duckdb")
  path <- tempfile(fileext = ".duckdb")
  conn <- open_db(path)
  on.exit({
    close_db(conn)
    unlink(path, force = TRUE)
  }, add = TRUE)
  eval(expr, envir = list2env(list(conn = conn), parent = parent.frame()))
}

test_that("confirmation is idempotent and audits only newly inserted manual links", {
  .with_perf_db({
    first <- record_confirmed_links(
      conn, .perf_candidate(), "B-test", match_method = "manual_confirmed"
    )
    retry <- record_confirmed_links(
      conn, .perf_candidate(), "B-test", match_method = "manual_confirmed"
    )

    expect_equal(first$n_committed, 1L)
    expect_equal(retry$n_committed, 0L)
    expect_equal(nrow(read_table(conn, "links_confirmed")), 1L)
    expect_equal(nrow(read_table(conn, "edit_log")), 1L)
  })
})

test_that("source persistence is retry-safe and confirmations do not append source rows", {
  .with_perf_db({
    raw <- .perf_vitek(c("1", "2"))
    ast <- .perf_ast(c("1", "2"))
    specimens <- .perf_specimens(c("1", "2"))

    first <- persist_source_batch_once(conn, "B-test", raw, ast, specimens)
    retry <- persist_source_batch_once(conn, "B-test", raw, ast, specimens)
    before <- vapply(c("vitek_raw", "vitek_ast", "specimens"), function(x) {
      DBI::dbGetQuery(conn, paste0("SELECT COUNT(*) n FROM ", x))$n[[1]]
    }, numeric(1))

    record_confirmed_links(conn, .perf_candidate("1"), "B-test", "manual_confirmed")
    record_confirmed_links(conn, .perf_candidate("2"), "B-test", "manual_confirmed")
    after <- vapply(c("vitek_raw", "vitek_ast", "specimens"), function(x) {
      DBI::dbGetQuery(conn, paste0("SELECT COUNT(*) n FROM ", x))$n[[1]]
    }, numeric(1))

    expect_equal(unname(first), c(2L, 2L, 2L))
    expect_equal(unname(retry), c(0L, 0L, 0L))
    expect_equal(before, after)
    expect_equal(nrow(read_table(conn, "links_confirmed")), 2L)

    .append_table_aligned(conn, "vitek_raw", .with_batch(raw[1, ], "B-partial"))
    persist_source_batch_once(conn, "B-partial", vitek_raw = raw)
    partial_n <- DBI::dbGetQuery(
      conn, "SELECT COUNT(*) n FROM vitek_raw WHERE batch_id = 'B-partial'"
    )$n[[1]]
    expect_equal(partial_n, 2)
  })
})

test_that("confirmation does not create cleaned export files", {
  .with_perf_db({
    work <- tempfile("axis-confirm-only-")
    dir.create(work)
    old <- setwd(work)
    on.exit({ setwd(old); unlink(work, recursive = TRUE, force = TRUE) }, add = TRUE)

    record_confirmed_links(conn, .perf_candidate(), "B-test", "manual_confirmed")

    expect_false(dir.exists(file.path(work, "data", "exports")))
    expect_length(list.files(work, recursive = TRUE), 0L)
  })
})

test_that("confirmed links survive reopening before export", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("duckdb")
  path <- tempfile(fileext = ".duckdb")
  conn <- open_db(path)
  record_confirmed_links(conn, .perf_candidate(), "B-test", "manual_confirmed")
  close_db(conn)

  conn <- open_db(path)
  on.exit({ close_db(conn); unlink(path, force = TRUE) }, add = TRUE)
  expect_equal(nrow(read_table(conn, "links_confirmed")), 1L)
  expect_equal(nrow(read_table(conn, "edit_log")), 1L)
})

test_that("failed export preserves durable confirmations and audit events", {
  .with_perf_db({
    record_confirmed_links(conn, .perf_candidate(), "B-test", "manual_confirmed")
    blocker <- tempfile()
    file.create(blocker)
    on.exit(unlink(blocker, force = TRUE), add = TRUE)

    expect_error(
      suppressWarnings(rebuild_and_export_cleaned_data(
        conn, "B-test", .perf_vitek(), .perf_ast(), .perf_specimens(),
        csv_path = file.path(blocker, "cannot-write.csv"), formats = "csv"
      ))
    )
    expect_equal(nrow(read_table(conn, "links_confirmed")), 1L)
    expect_equal(nrow(read_table(conn, "edit_log")), 1L)
  })
})

test_that("one final export matches the existing cleaned builders", {
  .with_perf_db({
    candidates <- bind_rows(.perf_candidate("1"), .perf_candidate("2"))
    record_confirmed_links(conn, candidates, "B-test", "manual_confirmed")
    vitek <- .perf_vitek(c("1", "2"))
    ast <- .perf_ast(c("1", "2"))
    specimens <- .perf_specimens(c("1", "2"))
    expected <- build_cleaned(read_table(conn, "links_confirmed"), tibble(), vitek, specimens)
    out <- tempfile("axis-final-")

    result <- rebuild_and_export_cleaned_data(
      conn, "B-test", vitek, ast, specimens,
      output_dir = out, formats = c("csv", "xlsx", "duckdb")
    )

    expect_equal(result$cleaned_links, expected)
    expect_equal(result$export_info$n_cleaned, 2L)
    expect_true(all(file.exists(result$export_info$csv)))
    expect_true(file.exists(result$export_info$xlsx))
    expect_equal(nrow(read_table(conn, "cleaned_links")), 2L)
  })
})
