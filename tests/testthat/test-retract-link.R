library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(lubridate)
library(purrr)

source("../../R/data_dedup.R")
source("../../R/data_match.R")
source("../../R/data_clean.R")
source("../../R/data_export_cfu.R")
source("../../R/store.R")

# ── Synthetic fixture ─────────────────────────────────────────────────────────
# Everything below is generated. No analyst export, specimen identifier, or
# local DuckDB content is used.

.rl_db <- function(env = parent.frame()) {
  conn <- open_db(tempfile(fileext = ".duckdb"))
  withr::defer(close_db(conn), envir = env)
  conn
}

.rl_candidate <- function(lab_id = "SYN001CRE1of1", os = "OS-001", score = 95) {
  tibble::tibble(
    lab_id = lab_id, isolate_number = "1", os_identifier = os,
    project_id = "SYNTH", specimen_label = lab_id,
    cp_short_title = "Synthetic Protocol", score = score
  )
}

.rl_confirm <- function(conn, candidate, method = "manual_selected") {
  confirm_links(conn, candidate, batch_id = "B-1", match_method = method,
                created_by = "analyst", rationale = "")
}

.rl_links <- function(conn) tibble::as_tibble(read_table(conn, "links_confirmed"))

# ── One isolate keeps one confirmed specimen ─────────────────────────────────

test_that("confirming a second specimen for a linked isolate is withheld, not appended", {
  conn <- .rl_db()

  first <- .rl_confirm(conn, .rl_candidate(os = "OS-WRONG"))
  expect_equal(first$n_committed, 1L)
  expect_equal(first$n_conflicted, 0L)

  second <- .rl_confirm(conn, .rl_candidate(os = "OS-RIGHT"))
  expect_equal(second$n_committed, 0L)
  expect_equal(second$n_conflicted, 1L)
  expect_equal(second$conflicted$os_identifier, "OS-RIGHT")

  # The isolate is still linked to exactly one specimen — the original.
  links <- .rl_links(conn)
  expect_equal(nrow(links), 1L)
  expect_equal(links$os_identifier, "OS-WRONG")
})

test_that("re-confirming the identical pairing is a no-op, not a conflict", {
  conn <- .rl_db()

  .rl_confirm(conn, .rl_candidate(os = "OS-001"))
  again <- .rl_confirm(conn, .rl_candidate(os = "OS-001"))

  expect_equal(again$n_committed, 0L)
  expect_equal(again$n_conflicted, 0L)
  expect_equal(nrow(.rl_links(conn)), 1L)
})

test_that("a different isolate is unaffected by another isolate's link", {
  conn <- .rl_db()

  .rl_confirm(conn, .rl_candidate(lab_id = "SYN001CRE1of1", os = "OS-001"))
  other <- .rl_confirm(conn, .rl_candidate(lab_id = "SYN002CRE1of1", os = "OS-002"))

  expect_equal(other$n_committed, 1L)
  expect_equal(other$n_conflicted, 0L)
  expect_equal(nrow(.rl_links(conn)), 2L)
})

# ── Retraction ────────────────────────────────────────────────────────────────

test_that("retract_links removes the link and records who removed it", {
  conn <- .rl_db()

  committed <- .rl_confirm(conn, .rl_candidate(os = "OS-WRONG"))
  link_id <- committed$inserted$link_id[[1]]

  out <- retract_links(conn, link_id, who = "cedar",
                       rationale = "selected the wrong record")

  expect_equal(out$n_retracted, 1L)
  expect_equal(nrow(.rl_links(conn)), 0L)

  audit <- tibble::as_tibble(read_table(conn, "edit_log")) |>
    dplyr::filter(event_type == "link.retracted")
  expect_equal(nrow(audit), 1L)
  expect_equal(audit$link_id[[1]], link_id)
  expect_equal(audit$from_value[[1]], "OS-WRONG")
  expect_equal(audit$who[[1]], "cedar")
  expect_match(audit$to_value[[1]], "selected the wrong record")
})

test_that("a retracted isolate can be linked to the correct specimen", {
  conn <- .rl_db()

  wrong <- .rl_confirm(conn, .rl_candidate(os = "OS-WRONG"))
  retract_links(conn, wrong$inserted$link_id[[1]], who = "cedar")

  right <- .rl_confirm(conn, .rl_candidate(os = "OS-RIGHT"))
  expect_equal(right$n_committed, 1L)
  expect_equal(right$n_conflicted, 0L)

  links <- .rl_links(conn)
  expect_equal(nrow(links), 1L)
  expect_equal(links$os_identifier, "OS-RIGHT")
})

test_that("the same pairing can be confirmed again after being retracted", {
  conn <- .rl_db()

  first <- .rl_confirm(conn, .rl_candidate(os = "OS-001"))
  retract_links(conn, first$inserted$link_id[[1]])

  again <- .rl_confirm(conn, .rl_candidate(os = "OS-001"))
  expect_equal(again$n_committed, 1L)
  expect_equal(nrow(.rl_links(conn)), 1L)
})

test_that("retracting a link removes its field overrides", {
  conn <- .rl_db()

  committed <- .rl_confirm(conn, .rl_candidate(os = "OS-001"))
  link_id <- committed$inserted$link_id[[1]]

  write_overrides(conn, tibble::tibble(
    link_id = link_id, field = "organism", cleaned_value = "Escherichia coli",
    source_hint = "analyst", rationale = "",
    edited_at = lubridate::now(tzone = "UTC"), edited_by = "cedar"
  ))
  expect_equal(nrow(read_table(conn, "cleaned_overrides")), 1L)

  retract_links(conn, link_id)
  expect_equal(nrow(read_table(conn, "cleaned_overrides")), 0L)
})

test_that("retracting an unknown link id is a no-op", {
  conn <- .rl_db()
  .rl_confirm(conn, .rl_candidate(os = "OS-001"))

  out <- retract_links(conn, "no-such-link")
  expect_equal(out$n_retracted, 0L)
  expect_equal(nrow(.rl_links(conn)), 1L)
})

test_that("retract_links accepts several link ids at once", {
  conn <- .rl_db()

  a <- .rl_confirm(conn, .rl_candidate(lab_id = "SYN001CRE1of1", os = "OS-001"))
  b <- .rl_confirm(conn, .rl_candidate(lab_id = "SYN002CRE1of1", os = "OS-002"))

  out <- retract_links(conn, c(a$inserted$link_id[[1]], b$inserted$link_id[[1]]))
  expect_equal(out$n_retracted, 2L)
  expect_equal(nrow(.rl_links(conn)), 0L)
})

test_that("retract_links on an empty id vector touches nothing", {
  conn <- .rl_db()
  .rl_confirm(conn, .rl_candidate(os = "OS-001"))

  out <- retract_links(conn, character())
  expect_equal(out$n_retracted, 0L)
  expect_equal(nrow(.rl_links(conn)), 1L)
})

# ── Legacy databases holding two specimens for one isolate ───────────────────

.rl_double_linked <- function() {
  tibble::tibble(
    link_id = c("L-old", "L-new"),
    lab_id = "SYN001CRE1of1", isolate_number = "1",
    os_identifier = c("OS-WRONG", "OS-RIGHT"),
    project_id = "SYNTH", specimen_label = "SYN001CRE1of1",
    cp_short_title = "Synthetic Protocol", confidence = c(0.9, 0.95),
    match_method = "manual_selected", state = "confirmed", batch_id = "B-1",
    created_at = as.POSIXct(c("2026-01-01 10:00:00", "2026-01-01 10:05:00"),
                            tz = "UTC"),
    created_by = "analyst"
  )
}

test_that("the export keeps only the most recent link for a double-linked isolate", {
  deduped <- dedup_confirmed_links(.rl_double_linked())

  expect_equal(nrow(deduped), 1L)
  expect_equal(deduped$os_identifier, "OS-RIGHT")
  expect_equal(deduped$link_id, "L-new")
})

test_that("superseded_isolate_links reports the older link so it can be cleared", {
  stale <- superseded_isolate_links(.rl_double_linked())

  expect_equal(nrow(stale), 1L)
  expect_equal(stale$link_id, "L-old")
  expect_equal(stale$os_identifier, "OS-WRONG")
})

test_that("superseded_isolate_links is empty when every isolate has one link", {
  clean <- .rl_double_linked()[2, ]
  expect_equal(nrow(superseded_isolate_links(clean)), 0L)
  expect_equal(nrow(superseded_isolate_links(NULL) %||% tibble::tibble()), 0L)
})

test_that("repeated commits of one pairing still collapse to a single row", {
  repeated <- .rl_double_linked()
  repeated$os_identifier <- "OS-RIGHT"

  expect_equal(nrow(dedup_confirmed_links(repeated)), 1L)
  expect_equal(nrow(superseded_isolate_links(repeated)), 0L)
})

# ── Confirmed isolates stay out of the review queue ──────────────────────────

.rl_bucket_fixture <- function() {
  lab_ids <- c("SYN001CRE1of1", "SYN002CRE1of1", "SYN003CRE1of1")
  vitek_unique <- tibble::tibble(
    lab_id = lab_ids, isolate_number = "1",
    organism_name = "Klebsiella pneumoniae", parsed_study = "SYNTH"
  )
  candidates <- tibble::tibble(
    lab_id = rep(lab_ids, each = 2L),
    isolate_number = "1",
    os_identifier = c("OS-001", "OS-001b", "OS-002", "OS-002b",
                      "OS-003", "OS-003b"),
    project_id = "SYNTH",
    specimen_label = rep(lab_ids, each = 2L),
    cp_short_title = "Synthetic Protocol",
    # First of each pair clears thresh_auto with no close runner-up.
    score = c(95, 40, 95, 40, 95, 40),
    mdro_disagree = FALSE, organism_disagree = FALSE,
    label_match_kind = "exact"
  )
  list(vitek_unique = vitek_unique, candidates = candidates, lab_ids = lab_ids)
}

test_that("an isolate with a confirmed link leaves every working bucket", {
  fx <- .rl_bucket_fixture()
  confirmed <- tibble::tibble(lab_id = "SYN002CRE1of1", isolate_number = "1")

  before <- bucket_results(fx$candidates, fx$vitek_unique)
  after  <- bucket_results(fx$candidates, fx$vitek_unique,
                           links_confirmed = confirmed)

  expect_true("SYN002CRE1of1" %in% before$matched$lab_id)
  expect_false("SYN002CRE1of1" %in% after$matched$lab_id)
  expect_false("SYN002CRE1of1" %in% after$review$lab_id)
  expect_false("SYN002CRE1of1" %in% after$none$lab_id)
  expect_true("SYN002CRE1of1" %in% after$confirmed$lab_id)
})

test_that("an unconfirmed isolate is bucketed exactly as before", {
  fx <- .rl_bucket_fixture()
  confirmed <- tibble::tibble(lab_id = "SYN002CRE1of1", isolate_number = "1")

  after <- bucket_results(fx$candidates, fx$vitek_unique,
                          links_confirmed = confirmed)

  expect_true("SYN001CRE1of1" %in% after$matched$lab_id)
  expect_true("SYN003CRE1of1" %in% after$matched$lab_id)
})

test_that("a no-match isolate is withheld once its link is confirmed manually", {
  fx <- .rl_bucket_fixture()
  # SYN003 has no candidates at all, so it lands in the no-match bucket.
  cands <- dplyr::filter(fx$candidates, lab_id != "SYN003CRE1of1")

  before <- bucket_results(cands, fx$vitek_unique)
  expect_true("SYN003CRE1of1" %in% before$none$lab_id)

  after <- bucket_results(
    cands, fx$vitek_unique,
    links_confirmed = tibble::tibble(lab_id = "SYN003CRE1of1", isolate_number = "1")
  )
  expect_false("SYN003CRE1of1" %in% after$none$lab_id)
})

test_that("confirmed-isolate keys compare case- and whitespace-insensitively", {
  fx <- .rl_bucket_fixture()
  confirmed <- tibble::tibble(lab_id = " syn002cre1of1 ", isolate_number = " 1 ")

  after <- bucket_results(fx$candidates, fx$vitek_unique,
                          links_confirmed = confirmed)
  expect_false("SYN002CRE1of1" %in% after$matched$lab_id)
})

test_that("passing no confirmed links leaves the buckets unchanged", {
  fx <- .rl_bucket_fixture()

  plain <- bucket_results(fx$candidates, fx$vitek_unique)
  nulled <- bucket_results(fx$candidates, fx$vitek_unique, links_confirmed = NULL)
  empty <- bucket_results(fx$candidates, fx$vitek_unique,
                          links_confirmed = links_confirmed_empty())

  expect_equal(nrow(nulled$matched), nrow(plain$matched))
  expect_equal(nrow(empty$matched), nrow(plain$matched))
  expect_equal(nrow(empty$none), nrow(plain$none))
  expect_equal(nrow(plain$confirmed), 0L)
})

test_that("bucket counts report confirmed isolates alongside the working buckets", {
  fx <- .rl_bucket_fixture()
  after <- bucket_results(
    fx$candidates, fx$vitek_unique,
    links_confirmed = tibble::tibble(lab_id = "SYN002CRE1of1", isolate_number = "1")
  )
  counts <- match_bucket_counts(after)

  expect_equal(counts$confirmed, 1L)
  expect_equal(counts$matched + counts$review + counts$none + counts$confirmed,
               nrow(fx$vitek_unique))
  expect_equal(match_bucket_counts(NULL)$confirmed, 0L)
})

test_that("the round trip settles: confirm, re-match, retract, re-match", {
  conn <- .rl_db()
  fx <- .rl_bucket_fixture()

  committed <- .rl_confirm(
    conn,
    .rl_candidate(lab_id = "SYN002CRE1of1", os = "OS-002")
  )

  # Re-running auto-match must not offer the confirmed isolate again.
  after_confirm <- bucket_results(fx$candidates, fx$vitek_unique,
                                  links_confirmed = .rl_links(conn))
  expect_false("SYN002CRE1of1" %in% after_confirm$review$lab_id)
  expect_false("SYN002CRE1of1" %in% after_confirm$matched$lab_id)

  retract_links(conn, committed$inserted$link_id[[1]], who = "cedar")

  # And once retracted it must come back.
  after_retract <- bucket_results(fx$candidates, fx$vitek_unique,
                                  links_confirmed = .rl_links(conn))
  expect_true("SYN002CRE1of1" %in% after_retract$matched$lab_id)
})
