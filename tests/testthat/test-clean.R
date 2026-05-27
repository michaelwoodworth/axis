library(dplyr)
library(tibble)
library(tidyr)

source("../../R/data_clean.R")

test_that("build_cleaned applies latest field overrides over source values", {
  links <- tibble::tibble(
    link_id = "L1",
    lab_id = "ARG026ESBL1",
    isolate_number = "1",
    os_identifier = "OS1",
    project_id = "ARRRRG",
    specimen_label = "ARG026_P2",
    cp_short_title = "ARRRRG 2.0",
    confidence = 0.95,
    match_method = "auto",
    state = "confirmed",
    batch_id = "B-test"
  )

  vitek <- tibble::tibble(
    lab_id = "ARG026ESBL1",
    isolate_number = "1",
    organism_name = "Klebsiella pneumoniae",
    specimen_type = "Isolate",
    specimen_source = "Rectal",
    collection_date = as.Date(NA),
    testing_date = as.Date("2025-01-10"),
    parsed_study = "ARRRRG",
    parsed_subject = "ARG026",
    parsed_target = "ESBL",
    cp_hint = "ARRRRG 2.0",
    n_drugs = 18L,
    file_name = "vitek.xlsx"
  )

  specimens <- tibble::tibble(
    os_identifier = "OS1",
    participant_id = "ARG026",
    custom_collection_date = as.Date("2025-01-08"),
    custom_organism = "K. pneumoniae",
    custom_mdro = "CRE",
    class = "Fluid",
    type = "Perirectal eSwab",
    lineage = "New"
  )

  overrides <- tibble::tibble(
    link_id = c("L1", "L1"),
    field = c("organism", "organism"),
    cleaned_value = c("Older value", "Klebsiella pneumoniae complex"),
    edited_at = as.POSIXct(c("2025-01-01 00:00:00", "2025-01-02 00:00:00")),
    edited_by = "analyst"
  )

  cleaned <- build_cleaned(links, overrides, vitek, specimens)

  expect_equal(cleaned$clean_organism, "Klebsiella pneumoniae complex")
  expect_equal(cleaned$n_edits, 2L)
  expect_true(cleaned$has_edit)
  expect_true(cleaned$mdro_disagree)
})

test_that("cleaned export includes linked AST rows and writes files", {
  cleaned <- tibble::tibble(
    link_id = "L1",
    batch_id = "B-test",
    lab_id = "ARG026ESBL1",
    isolate_number = "1",
    os_identifier = "OS1",
    specimen_label = "ARG026_P2",
    project_id = "ARRRRG",
    cp_short_title = "ARRRRG 2.0",
    clean_lab_id = "ARG026ESBL1",
    clean_organism = "Klebsiella pneumoniae",
    clean_mdro_category = "ESBL",
    clean_testing_date = "2025-01-10",
    clean_participant_id = "ARG026"
  )

  ast <- tibble::tibble(
    source_file = "vitek.xlsx",
    source_row = 1L,
    lab_id = "ARG026ESBL1",
    isolate_number = "1",
    drug_code = c("AN", "CRO"),
    drug_name = c("Amikacin", "Ceftriaxone"),
    mic = c("<=2", ">=64"),
    call_instr = c("S", "R"),
    call_expert = c("S", "R"),
    ingested_at = as.POSIXct("2025-01-01 00:00:00")
  )

  cleaned_ast <- build_cleaned_ast(cleaned, ast)
  expect_equal(nrow(cleaned_ast), 2)
  expect_true(all(c("link_id", "drug_code", "mic", "call_expert") %in% names(cleaned_ast)))

  out_dir <- tempfile("axis_export_")
  info <- export_cleaned_dataset(
    cleaned = cleaned,
    cleaned_ast = cleaned_ast,
    batch_id = "B-test",
    output_dir = out_dir,
    formats = c("csv", "xlsx")
  )

  expect_equal(info$n_cleaned, 1)
  expect_equal(info$n_ast, 2)
  expect_true(all(file.exists(info$csv)))
  expect_true(file.exists(info$xlsx))
})
