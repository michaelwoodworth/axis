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
  specimens <- tibble::tibble(
    project_id = "ARRRRG",
    os_identifier = c("OS-P", "OS1", "OS-N"),
    specimen_label_raw = c("ARG026_P2", "ARG026ESBL1", "ARG027_P2"),
    specimen_label = c("ARG026_P2", "ARG026ESBL1", "ARG027_P2"),
    cp_short_title = "ARRRRG 2.0",
    parent_label = c(NA_character_, "ARG026_P2", NA_character_),
    participant_id = c("ARG026", "ARG026", "ARG027"),
    type = c("Stool", "Cryopreserved Cells", "Stool"),
    class = c("Specimen", "Aliquot", "Specimen"),
    custom_mdro = c("ESBL", "ESBL", "Negative"),
    custom_collection_date = as.Date(c("2025-01-08", "2025-01-08", "2025-01-09"))
  )
  info <- export_cleaned_dataset(
    cleaned = cleaned,
    cleaned_ast = cleaned_ast,
    batch_id = "B-test",
    specimens = specimens,
    output_dir = out_dir,
    formats = c("csv", "xlsx")
  )

  expect_equal(info$n_cleaned, 1)
  expect_equal(info$n_ast, 2)
  expect_equal(info$n_specimens, 2)
  expect_true("specimen_dataset" %in% names(info$csv))
  expect_true(all(file.exists(info$csv)))
  expect_true(file.exists(info$xlsx))
})

test_that("specimen dataset follows hierarchy and retains unlinked negative parents", {
  specimens <- tibble::tibble(
    project_id = "PRE_ALERT",
    os_identifier = c("P1", "A1", "I1", "P2"),
    specimen_label_raw = c("1610001", "1610001-A", "1610001ESBL1", "1610002"),
    specimen_label = c("1610001", "1610001-A", "1610001ESBL1", "1610002"),
    cp_short_title = "Pre-Alert",
    parent_label = c(NA_character_, "1610001", "1610001-A", NA_character_),
    participant_id = c("1610001", "1610001", "1610001", "1610002"),
    type = c("Stool", "Aliquot", "Cryopreserved Cells", "Stool"),
    class = c("Specimen", "Aliquot", "Aliquot", "Specimen"),
    custom_mdro = c("ESBL", NA_character_, "ESBL", "Negative"),
    custom_collection_date = as.Date("2025-01-10")
  )
  cleaned <- tibble::tibble(
    os_identifier = "I1", lab_id = "1610001ESBL1", isolate_number = "1",
    clean_mdro_category = "ESBL", clean_organism = "Escherichia coli",
    clean_testing_date = "2025-01-11", mdro_disagree = FALSE
  )

  hierarchy <- resolve_specimen_hierarchy(specimens)
  isolate <- dplyr::filter(hierarchy, os_identifier == "I1")
  expect_equal(isolate$parent_os_identifier, "P1")
  expect_equal(isolate$hierarchy_depth, 2L)

  result <- build_specimen_dataset(cleaned, specimens)
  expect_equal(nrow(result), 2L)
  expect_equal(result$n_linked_isolates[result$parent_os_identifier == "P1"], 1L)
  expect_equal(result$mdro_concordance[result$parent_os_identifier == "P1"], "concordant_positive")
  expect_equal(result$n_linked_isolates[result$parent_os_identifier == "P2"], 0L)
  expect_equal(result$mdro_concordance[result$parent_os_identifier == "P2"], "concordant_negative")
})
