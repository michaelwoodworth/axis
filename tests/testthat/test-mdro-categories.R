library(dplyr)
library(tibble)

source("../../R/mdro_categories.R")
source("../../R/data_clean.R")
source("../../R/mod_inventory.R")

test_that("explicit MDRO categories are canonicalized", {
  cleaned <- tibble::tibble(
    link_id = paste0("L", 1:7),
    clean_mdro_category = c("ESBL", "CRKP", "VRE", "CRPA", "CRAB", "Negative", NA),
    clean_organism = c(
      "Esch.coli",
      "K.pneumoniae",
      "Enterococcus faecium",
      "Ps.aeruginosa",
      "Aci.baumannii cplx",
      "Esch.coli",
      "Esch.coli"
    )
  )

  out <- axis_interpret_mdro_categories(cleaned)

  expect_equal(
    out$inv_mdro_category,
    c("ESBL", "CRE", "VRE", "MDRP", "MDRA", "Non-MDRO", "Unspecified")
  )
})

test_that("VRE labels on non-Enterococcus linked rows are not binned as VRE", {
  cleaned <- tibble::tibble(
    link_id = "bad-vre",
    clean_mdro_category = "VRE",
    clean_organism = "K.pneumoniae"
  )

  out <- axis_interpret_mdro_categories(cleaned)

  expect_equal(out$inv_mdro_category, "Unspecified")
  expect_equal(out$inv_mdro_basis, "VRE label on non-Enterococcus organism")
})

test_that("positive labels can be resolved from linked AST phenotype", {
  cleaned <- tibble::tibble(
    link_id = c("CRE1", "ESBL1", "MDRP1", "MDRA1", "UNK1"),
    clean_mdro_category = c("Positive", "Positive", "Positive", "Positive", "Positive"),
    clean_organism = c(
      "K.pneumoniae",
      "Esch.coli",
      "Ps.aeruginosa",
      "Aci.baumannii cplx",
      "Esch.coli"
    )
  )
  ast <- tibble::tibble(
    link_id = c(
      "CRE1", "ESBL1",
      "MDRP1", "MDRP1", "MDRP1",
      "MDRA1", "MDRA1", "MDRA1"
    ),
    drug_code = c("MEM", "CRO", "MEM", "LEV", "CAZ", "MEM", "CIP", "SAM"),
    call_expert = "R",
    call_instr = NA_character_
  )

  out <- axis_interpret_mdro_categories(cleaned, ast)

  expect_equal(
    out$inv_mdro_category,
    c("CRE", "ESBL", "MDRP", "MDRA", "Unspecified")
  )
})

test_that("OpenSpecimen-only Enterococcus isolates can feed Sankey VRE bins", {
  specimens <- tibble::tibble(
    type = rep("Cryopreserved Cells", 4),
    custom_organism = c(
      "Enterococcus faecium",
      "Enterococcus faecalis",
      "Enterococcus faecium",
      "Klebsiella pneumoniae"
    ),
    custom_mdro = c("Positive", "Negative", "VRE", "Positive"),
    participant_id = c("rEM001", "rEM002", "rEM003", "rEM004"),
    specimen_label = c("e1", "e2", "e3", "k1"),
    cp_short_title = "REACT",
    project_id = "REACT",
    custom_parent_specimen_type = "Perirectal eSwab",
    custom_collection_date = as.Date("2025-01-01"),
    collection_dt = as.POSIXct("2025-01-01 00:00:00")
  )

  flow <- os_enterococcus_flow_rows(specimens)

  expect_equal(nrow(flow), 3)
  expect_equal(sort(unique(flow$flow_species)),
               c("Enterococcus faecalis", "Enterococcus faecium"))
  expect_equal(sort(unique(flow$flow_mdro)), c("Non-MDRO", "VRE"))
})

test_that("Inventory flow fields use Sankey categories for filter pills", {
  cleaned <- tibble::tibble(
    link_id = paste0("L", 1:5),
    clean_participant_id = c("aEM001", "rEM001", "SNT001", "FR001", "ARG026"),
    v_parsed_subject = NA_character_,
    lab_id = c("aEM001", "rEM001", "SNT001", "6180012ESBL1", "ARG026P2CRE1"),
    v_parsed_study = c("aEM", "aEM", "Unknown", "FAIR618", "ARRRRG"),
    clean_cp_title = c("SNT/APPS/React", "REACT", "SNT/APPS/React", "FAIR 618", "ARRRRG 2.0"),
    cp_short_title = c("SNT/APPS/React", "REACT", "SNT/APPS/React", "FAIR 618", "ARRRRG 2.0"),
    project_id = c("SNT_output", "REACT_output", "SNT_output", "FAIR_output", "ARRRRGv2_output"),
    clean_parent_specimen_type = c(
      "Perirectal eSwab", "Cryopreserved Cells", "Environmental Sponge",
      "Cryopreserved Cells", "Cryopreserved Cells"
    ),
    inv_mdro_category = c("VRE", "Non-MDRO", "MDRP", "ESBL", "CRE"),
    clean_organism = c(
      "Enterococcus faecium", "Escherichia coli", "Pseudomonas aeruginosa",
      "Escherichia coli", "Klebsiella pneumoniae"
    )
  )

  flow <- add_inventory_flow_fields(cleaned)

  expect_setequal(flow$flow_study, c("APPS", "REACT", "APPS + REACT", "FAIR", "ARRRRG"))
  expect_true("Isolates" %in% flow$flow_parent)
  expect_true("VRE" %in% ordered_flow_choices(flow$flow_mdro, "mdro"))
  expect_equal(
    ordered_flow_choices(flow$flow_study, "study"),
    c("APPS", "REACT", "APPS + REACT", "FAIR", "ARRRRG")
  )
})
