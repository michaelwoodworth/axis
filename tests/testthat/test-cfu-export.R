library(dplyr)
library(readr)
library(tibble)

source("../../R/data_export_cfu.R")

.cfu_export_fixture <- function() {
  tibble::tibble(
    source_file = c("FAIR_output.csv", "FAIR_output.csv", "REACT_output.csv"),
    source_row = c(10L, 11L, 12L),
    project_id = c("FAIR_output", "FAIR_output", "REACT_output"),
    cp_short_title = c("FAIR 618", "FAIR 618", "REACT"),
    os_identifier = c("os1", "os2", "os3"),
    specimen_label = c("6180017CRE1", "6180017ESBL2", "R-0188-04"),
    participant_id = c("FR03", "FR03", "R0188"),
    custom_day = c("Day 7", "Day 7", "Day 3"),
    custom_collection_date = as.Date(c("2026-01-19", "2026-01-19", "2026-02-03")),
    collection_dt = as.POSIXct(c("2026-01-19", "2026-01-19", "2026-02-03"), tz = "UTC"),
    custom_selective_media = c("ESBL", "ESBL", "VRE"),
    custom_organism = c("K. pneumoniae", "E. cloacae complex", "E. faecalis"),
    custom_mdro = c("CRE", "ESBL", "VRE"),
    cfu_raw = c(">1x10^6", "11", NA_character_),
    cfu_log10 = c(6, log10(11), 0),
    cfu_value = c(1e6, 11, 1),
    cfu_unit = c("CFU/mL", "CFU/mL", "CFU/mL"),
    cfu_censored = c(TRUE, FALSE, FALSE),
    growth_method = c("DP", "DP", "EB"),
    is_pseudocount = c(FALSE, FALSE, TRUE),
    cfu_flag = c(NA_character_, "ambiguous_bare", NA_character_),
    has_quant = c(TRUE, TRUE, TRUE)
  )
}

test_that("CFU review export contains non-clean values with issue labels", {
  review <- prepare_cfu_review_export(.cfu_export_fixture(), batch_id = "B-test")

  expect_equal(nrow(review), 3)
  expect_setequal(review$status, c("normalized", "review", "pseudocount"))
  expect_true(all(c("raw", "project", "specimen", "parsed", "unit", "issue", "status") %in% names(review)))
  expect_true(any(grepl("Right-censored", review$issue)))
  expect_true(any(grepl("Bare value", review$issue)))
})

test_that("CFU summary export groups by participant and time point", {
  summary <- prepare_cfu_summary_export(.cfu_export_fixture(), batch_id = "B-test")

  expect_equal(nrow(summary), 2)
  fair <- summary |> dplyr::filter(project == "FAIR 618")
  expect_equal(fair$n_quant_rows, 2)
  expect_equal(fair$n_censored, 1)
  expect_equal(fair$n_flagged, 1)
  expect_true(grepl("K. pneumoniae", fair$organisms))
})

test_that("CFU CSV writers create separate review and summary files", {
  out_dir <- tempfile("cfu_exports_")
  info <- write_cfu_csv_exports(.cfu_export_fixture(), batch_id = "B-test", output_dir = out_dir)

  expect_true(file.exists(info$review_path))
  expect_true(file.exists(info$summary_path))
  expect_equal(info$n_review, 3)
  expect_equal(info$n_summary, 2)
  expect_equal(nrow(readr::read_csv(info$review_path, show_col_types = FALSE)), 3)
  expect_equal(nrow(readr::read_csv(info$summary_path, show_col_types = FALSE)), 2)
})
