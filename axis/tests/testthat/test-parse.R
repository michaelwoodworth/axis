library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(readxl)
library(tibble)

source("../../R/data_parse_labid.R")
source("../../R/data_parse_vitek.R")
source("../../R/data_parse_os.R")

test_that("Vitek parser reads sample XLSX files and preserves AST", {
  files <- list.files("../../../01.data/02.vitek2_exports", pattern = "\\.xlsx$",
                      full.names = TRUE)
  parsed <- parse_vitek_files(files)

  expect_equal(nrow(parsed$vitek_raw), 742)
  expect_equal(nrow(parsed$vitek_ast), 14616)
  expect_false("Patient Name" %in% names(parsed$vitek_raw))

  expect_true(all(c("source_file", "source_row", "ingested_at") %in% names(parsed$vitek_raw)))
  expect_true(all(c("mic", "call_instr", "call_expert") %in% names(parsed$vitek_ast)))
  expect_true(all(c("result_mic", "result_instrument", "result_expertized") %in% names(parsed$vitek_ast)))
  expect_true(any(parsed$vitek_ast$drug_code == "AM"))
})

test_that("OpenSpecimen scanner and parser handle CSV and ZIP exports", {
  projects <- scan_os_projects("../../../01.data/01.openspecimen_exports")

  expect_equal(nrow(projects), 4)
  expect_setequal(
    projects$study_label,
    c("ARRRRG 2.0", "FAIR 618", "REACT", "SNT/APPS/React")
  )

  csv_rows <- parse_os_specimens("../../../01.data/01.openspecimen_exports/ARRRRGv2_output.csv")
  zip_rows <- parse_os_specimens("../../../01.data/01.openspecimen_exports/ExportJob_7291.zip")

  expect_equal(nrow(csv_rows), 668)
  expect_equal(nrow(zip_rows), 668)
  expect_equal(unique(zip_rows$cp_short_title), "ARRRRG 2.0")
  expect_true(all(c("source_file", "source_row", "available_qty") %in% names(zip_rows)))
  expect_type(zip_rows$available_qty, "double")
  expect_true("custom_parent_specimen_type" %in% names(zip_rows))
  expect_true(any(!is.na(zip_rows$custom_parent_specimen_type)))
})

test_that("OpenSpecimen parser strips glycerol suffix only for cryopreserved cells", {
  rows <- parse_os_specimens("../../../01.data/01.openspecimen_exports/ARRRRGv2_output.csv")
  cryo <- rows |>
    dplyr::filter(type == "Cryopreserved Cells", grepl("glycerol", specimen_label_raw, ignore.case = TRUE))

  expect_gt(nrow(cryo), 0)
  expect_false(any(grepl("glycerol", cryo$specimen_label, ignore.case = TRUE)))
  expect_true(any(cryo$specimen_label_raw != cryo$specimen_label))
})
