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

test_that("Pre-Alert style 161 lab IDs do not fall through to FAIR", {
  parsed <- parse_lab_ids(c(
    "1610123ESBL1", "Pre-Alert-1610456CRE2", "PREALERT-LEGACY001VRE1"
  ))

  expect_equal(parsed$parsed_study, rep("PRE_ALERT", 3))
  expect_equal(parsed$cp_hint, rep("Pre-Alert", 3))
  expect_equal(parsed$parsed_subject[1:2], c("1610123", "1610456"))
  expect_equal(parsed$parsed_target, c("ESBL", "CRE", "VRE"))
})

test_that("explicit SNT lab IDs use the SNT collection protocol hint", {
  parsed <- parse_lab_ids("SNT-LEGACY001ESBL1")

  expect_equal(parsed$parsed_study, "SNT")
  expect_equal(parsed$cp_hint, "SNT/APPS/React")
  expect_equal(parsed$parsed_target, "ESBL")
})

test_that("MEPSD lab IDs remain classified even without an OpenSpecimen link", {
  parsed <- parse_lab_ids(c("MEPSD001", "MEPSD-DONOR-02CRE1"))

  expect_equal(parsed$parsed_study, c("MEPSD", "MEPSD"))
  expect_equal(parsed$cp_hint, c("MEPSD", "MEPSD"))
  expect_equal(parsed$parsed_target, c(NA_character_, "CRE"))
})

test_that("OpenSpecimen parser accepts legacy Pre-Alert field aliases", {
  csv <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      Identifier = c("PA-P1", "PA-I1"),
      `Specimen Label` = c("1619001", "1619001ESBL1"),
      `CP Short Title` = "Pre Alert Legacy",
      Class = c("Specimen", "Aliquot"),
      Type = c("Stool", "Cryopreserved Cells"),
      Lineage = c("New", "Derived"),
      `Parent Specimen Label` = c(NA_character_, "1619001"),
      `Pre Alert#Participant ID` = "1619001",
      `Pre Alert#Collection Date` = "01/10/2025",
      `Pre Alert#MDRO` = c("ESBL", "ESBL"),
      `Pre Alert#Genus Species` = c(NA_character_, "Escherichia coli"),
      `Pre Alert#Parent Specimen Type` = "Stool"
    ),
    csv,
    na = ""
  )

  parsed <- parse_os_specimens(csv, project_id = "PRE_ALERT")
  expect_equal(nrow(parsed), 2L)
  expect_equal(parsed$participant_id, c("1619001", "1619001"))
  expect_equal(parsed$custom_mdro, c("ESBL", "ESBL"))
  expect_equal(parsed$parent_label[[2]], "1619001")
})

test_that("OpenSpecimen parser accepts XLSX exports", {
  testthat::skip_if_not_installed("openxlsx")
  xlsx <- tempfile(fileext = ".xlsx")
  fixture <- data.frame(
    Identifier = c("PA-P1", "PA-I1"),
    `Specimen Label` = c("1619001", "1619001ESBL1"),
    `CP Short Title` = "Pre-Alert",
    Class = c("Specimen", "Aliquot"),
    Type = c("Stool", "Cryopreserved Cells"),
    Lineage = c("New", "Derived"),
    `Parent Specimen Label` = c(NA_character_, "1619001"),
    `FAIR#Participant ID` = "1619001",
    `FAIR#MDRO Category` = c("ESBL", "ESBL"),
    check.names = FALSE
  )
  openxlsx::write.xlsx(fixture, xlsx, overwrite = TRUE)

  parsed <- parse_os_specimens(xlsx, project_id = "PRE_ALERT")
  expect_equal(nrow(parsed), 2L)
  expect_equal(parsed$cp_short_title, c("Pre-Alert", "Pre-Alert"))
  expect_equal(parsed$custom_mdro, c("ESBL", "ESBL"))
})

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
  expect_true(all(c("anatomic_site", "custom_day", "custom_selective_media", "custom_site", "cfu_raw", "cfu_log10",
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

test_that("OpenSpecimen parser derives specific MDRO categories from positive flags", {
  rows <- tibble::tibble(
    `REACT Specimen#MDRO` = c("Positive", "Positive", "Negative", NA_character_),
    `REACT Specimen#CRE` = c("Negative", "Negative", "Positive", NA_character_),
    `REACT Specimen#ESBL` = c("Positive", "Negative", "Positive", NA_character_),
    `REACT Specimen#VRE` = c("Positive", "Negative", "Negative", "Positive"),
    `REACT Specimen#MDRP` = c("Negative", "Negative", "Negative", NA_character_),
    `REACT Specimen#CRAB` = c("Negative", "Positive", "Negative", NA_character_)
  )

  out <- .derive_mdro_from_flags(
    rows,
    primary_col = "REACT Specimen#MDRO",
    flags = c(
      "REACT Specimen#CRE", "REACT Specimen#ESBL", "REACT Specimen#VRE",
      "REACT Specimen#MDRP", "REACT Specimen#CRAB"
    )
  )

  expect_equal(out, c("ESBL; VRE", "CRAB", "CRE; ESBL", "VRE"))
})
