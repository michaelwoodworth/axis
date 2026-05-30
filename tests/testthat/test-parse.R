library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(readxl)
library(tibble)

source("../../R/data_parse_labid.R")
source("../../R/data_parse_vitek.R")
source("../../R/data_parse_cfu.R")
source("../../R/data_parse_os.R")

.axis_private_data_dir <- function(subdir) {
  candidates <- c(
    Sys.getenv("AXIS_TEST_DATA_DIR", unset = NA_character_),
    file.path("..", "..", "01.data"),
    file.path("..", "..", "..", "01.data")
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  candidates <- file.path(candidates, subdir)
  hits <- candidates[dir.exists(candidates)]
  if (length(hits) == 0) return(NA_character_)
  normalizePath(hits[[1]], mustWork = FALSE)
}

test_that("Vitek parser reads sample XLSX files and preserves AST", {
  vitek_dir <- .axis_private_data_dir("02.vitek2_exports")
  testthat::skip_if(is.na(vitek_dir), "Private Vitek2 sample exports are not available")

  files <- list.files(vitek_dir, pattern = "\\.xlsx$",
                      full.names = TRUE)
  testthat::skip_if(length(files) == 0, "Private Vitek2 sample exports are not available")
  parsed <- parse_vitek_files(files)

  expect_gte(nrow(parsed$vitek_raw), 742)
  expect_gte(nrow(parsed$vitek_ast), 14616)
  expect_false("Patient Name" %in% names(parsed$vitek_raw))

  expect_true(all(c("source_file", "source_row", "ingested_at") %in% names(parsed$vitek_raw)))
  expect_true(all(c("mic", "call_instr", "call_expert") %in% names(parsed$vitek_ast)))
  expect_true(all(c("result_mic", "result_instrument", "result_expertized") %in% names(parsed$vitek_ast)))
  expect_true(any(parsed$vitek_ast$drug_code == "AM"))
})

test_that("OpenSpecimen scanner and parser handle CSV and ZIP exports", {
  os_dir <- .axis_private_data_dir("01.openspecimen_exports")
  testthat::skip_if(is.na(os_dir), "Private OpenSpecimen sample exports are not available")

  csv_path <- file.path(os_dir, "ARRRRGv2_output.csv")
  zip_path <- file.path(os_dir, "ExportJob_7291.zip")
  testthat::skip_if_not(file.exists(csv_path), "Private OpenSpecimen CSV sample export is not available")
  testthat::skip_if_not(file.exists(zip_path), "Private OpenSpecimen ZIP sample export is not available")

  projects <- scan_os_projects(os_dir)

  expect_equal(nrow(projects), 4)
  expect_setequal(
    projects$study_label,
    c("ARRRRG 2.0", "FAIR 618", "REACT", "SNT/APPS/React")
  )

  csv_rows <- parse_os_specimens(csv_path)
  zip_rows <- parse_os_specimens(zip_path)

  expect_equal(nrow(csv_rows), 668)
  expect_equal(nrow(zip_rows), 668)
  expect_equal(unique(zip_rows$cp_short_title), "ARRRRG 2.0")
  expect_true(all(c("source_file", "source_row", "available_qty") %in% names(zip_rows)))
  expect_type(zip_rows$available_qty, "double")
  expect_true("custom_parent_specimen_type" %in% names(zip_rows))
  expect_true(any(!is.na(zip_rows$custom_parent_specimen_type)))
  expect_true(all(c("custom_day", "custom_selective_media", "cfu_raw", "cfu_log10",
                    "growth_method", "is_pseudocount", "has_quant") %in% names(csv_rows)))
  expect_equal(sum(!is.na(csv_rows$cfu_raw) & trimws(csv_rows$cfu_raw) != ""), 39)
  expect_equal(sum(csv_rows$has_quant, na.rm = TRUE), 40)
})

test_that("OpenSpecimen parser strips glycerol suffix only for cryopreserved cells", {
  os_dir <- .axis_private_data_dir("01.openspecimen_exports")
  testthat::skip_if(is.na(os_dir), "Private OpenSpecimen sample exports are not available")
  csv_path <- file.path(os_dir, "ARRRRGv2_output.csv")
  testthat::skip_if_not(file.exists(csv_path), "Private OpenSpecimen CSV sample export is not available")

  rows <- parse_os_specimens(csv_path)
  cryo <- rows |>
    dplyr::filter(type == "Cryopreserved Cells", grepl("glycerol", specimen_label_raw, ignore.case = TRUE))

  expect_gt(nrow(cryo), 0)
  expect_false(any(grepl("glycerol", cryo$specimen_label, ignore.case = TRUE)))
  expect_true(any(cryo$specimen_label_raw != cryo$specimen_label))
})
