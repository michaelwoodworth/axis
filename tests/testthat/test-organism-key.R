library(dplyr)
library(tibble)
library(tidyr)
library(readr)

source("../../R/data_dedup.R")
source("../../R/organism_key.R")
source("../../R/data_export_cfu.R")
source("../../R/data_clean.R")

# The key ships with the repository; tests read the real file so a bad edit to
# the CSV fails here rather than in an export.
options(axis.organism_key_path = normalizePath(
  file.path("..", "..", "inst", "extdata", "vitek_organism_key.csv"),
  mustWork = FALSE
))

test_that("the shipped key loads and is well formed", {
  clear_organism_key_cache()
  key <- load_organism_key()

  expect_gt(nrow(key), 0)
  expect_false(any(duplicated(key$organism_code)))
  expect_true(all(!is.na(key$organism_code)))
  expect_setequal(
    unique(key$clean_organism_rank),
    c("species", "subspecies", "complex", "group", "genus", "unidentified")
  )
  # Every row except the deliberate non-identification names something.
  named <- key |> dplyr::filter(.data$clean_organism_rank != "unidentified")
  expect_true(all(!is.na(named$clean_organism)))
  expect_true(all(!is.na(named$clean_organism_genus)))
})

test_that("species rows carry a genus and a species epithet, and coarser ranks do not", {
  key <- load_organism_key()

  species <- key |> dplyr::filter(.data$clean_organism_rank %in% c("species", "subspecies"))
  expect_true(all(!is.na(species$clean_organism_species)))
  expect_true(all(paste(species$clean_organism_genus, species$clean_organism_species) ==
                    species$clean_organism))

  coarse <- key |> dplyr::filter(.data$clean_organism_rank %in% c("complex", "group", "genus"))
  expect_true(all(is.na(coarse$clean_organism_species)))
  expect_true(all(!is.na(coarse$clean_organism_genus)))
})

test_that("the reviewed genus changes are the ones in the key", {
  key <- load_organism_key()
  expected <- c(
    EEE  = "Klebsiella aerogenes",
    EKV  = "Klebsiella planticola",
    PPS  = "Stutzerimonas stutzeri",
    ECU  = "Pseudescherichia vulneris",
    PHC  = "Agrobacterium radiobacter"
  )
  for (code in names(expected)) {
    expect_equal(key$clean_organism[key$organism_code == code], unname(expected[[code]]),
                 info = code)
  }

  # LPSN flags these newer combinations "not recommended for medical use", so
  # the key deliberately keeps the older names.
  unchanged <- c(PPM = "Pseudomonas mendocina", MLE = "Staphylococcus lentus")
  for (code in names(unchanged)) {
    expect_equal(key$clean_organism[key$organism_code == code], unname(unchanged[[code]]),
                 info = code)
  }
})

test_that("subspecies collapse to the species and keep the subspecies separately", {
  key <- load_organism_key()

  kp <- key |> dplyr::filter(.data$organism_code %in% c("EKPN", "EKP", "EKZ"))
  expect_equal(nrow(kp), 3L)
  expect_true(all(kp$clean_organism == "Klebsiella pneumoniae"))
  expect_setequal(kp$clean_organism_subspecies, c(NA, "pneumoniae", "ozaenae"))

  ec <- key |> dplyr::filter(.data$organism_code %in% c("EEC", "EEF", "EGO"))
  expect_true(all(ec$clean_organism == "Enterobacter cloacae"))
  expect_setequal(ec$clean_organism_subspecies, c(NA, "cloacae", "dissolvens"))
})

test_that("resolve_organism_names maps codes and preserves rank", {
  res <- resolve_organism_names(
    c("ECO", "EECG", "SMT0", "EKX", "NOSP"),
    c("Esch.coli", "Ent.cloacae complex", "Str.mitis/oralis", "Klebsiella spp",
      "Low Discrim Organism")
  )

  expect_equal(res$clean_organism[1], "Escherichia coli")
  expect_equal(res$clean_organism_rank, c("species", "complex", "group", "genus", "unidentified"))
  # A non-identification is never given a species name.
  expect_true(is.na(res$clean_organism[5]))
  expect_true(is.na(res$clean_organism_species[2]))
})

test_that("the same organism under two Vitek display names resolves to one name", {
  # Code EEE appears as both Ent.aerogenes and K.aerogenes across card versions.
  res <- resolve_organism_names(c("EEE", "EEE"), c("Ent.aerogenes", "K.aerogenes"))
  expect_equal(unique(res$clean_organism), "Klebsiella aerogenes")
})

test_that("codes are matched case-insensitively and with surrounding whitespace", {
  res <- resolve_organism_names(c(" eco ", "Eco"), c("Esch.coli", "Esch.coli"))
  expect_equal(res$clean_organism, rep("Escherichia coli", 2))
})

test_that("an unknown code keeps the raw Vitek name and is flagged", {
  res <- resolve_organism_names("ZZZZ", "New.organism")
  expect_equal(res$clean_organism, "New.organism")
  expect_equal(res$clean_organism_rank, "unmapped")
  expect_true(is.na(res$clean_organism_genus))
})

test_that("a row with no Vitek organism at all resolves to nothing", {
  res <- resolve_organism_names(NA_character_, NA_character_)
  expect_true(is.na(res$clean_organism))
  expect_true(is.na(res$clean_organism_rank))
})

test_that("unmapped_organism_codes reports codes the key does not cover", {
  vitek <- tibble::tibble(
    organism_code = c("ECO", "ECO", "ZZZZ", NA_character_),
    organism_name = c("Esch.coli", "Esch.coli", "New.organism", "Nothing")
  )
  out <- unmapped_organism_codes(vitek)
  expect_equal(out$organism_code, "ZZZZ")
  expect_equal(out$n, 2L * 0L + 1L)

  expect_equal(nrow(unmapped_organism_codes(NULL)), 0L)
  expect_equal(nrow(unmapped_organism_codes(tibble::tibble())), 0L)
})

# ── build_cleaned integration ────────────────────────────────────────────────

.clean_fixture <- function(code = "EEE", name = "Ent.aerogenes") {
  links <- tibble::tibble(
    link_id = "L1", lab_id = "SYN001CRE1of1", isolate_number = "1",
    os_identifier = "OS1", project_id = "SYNTH", specimen_label = "SYN001CRE1of1",
    cp_short_title = "Synthetic Protocol", confidence = 0.95,
    match_method = "auto", state = "confirmed", batch_id = "B-1"
  )
  vitek <- tibble::tibble(
    lab_id = "SYN001CRE1of1", isolate_number = "1",
    organism_code = code, organism_name = name,
    specimen_type = "Isolate", specimen_source = "Rectal",
    collection_date = as.Date(NA), testing_date = as.Date("2026-01-03"),
    parsed_study = "SYNTH", parsed_subject = "SYN001", parsed_target = "CRE",
    cp_hint = "Synthetic Protocol", n_drugs = 3L, file_name = "synthetic.xlsx"
  )
  specimens <- tibble::tibble(
    os_identifier = "OS1", participant_id = "SYN001",
    custom_collection_date = as.Date("2026-01-01"),
    custom_organism = "Enterobacter aerogenes", custom_mdro = "CRE",
    custom_parent_specimen_type = "Stool", class = "Fluid",
    type = "Cryopreserved Cells", lineage = "Aliquot", specimen_label = "SYN001CRE1of1"
  )
  list(links = links, vitek = vitek, specimens = specimens)
}

test_that("build_cleaned writes the curated organism and keeps the raw Vitek name", {
  fx <- .clean_fixture()
  cleaned <- build_cleaned(fx$links, overrides = NULL, fx$vitek, fx$specimens)

  expect_equal(cleaned$clean_organism, "Klebsiella aerogenes")
  expect_equal(cleaned$clean_organism_genus, "Klebsiella")
  expect_equal(cleaned$clean_organism_species, "aerogenes")
  expect_equal(cleaned$clean_organism_rank, "species")
  # Source values are untouched and still available for audit.
  expect_equal(cleaned$v_organism, "Ent.aerogenes")
  expect_equal(cleaned$v_organism_code, "EEE")
  expect_equal(cleaned$o_custom_organism, "Enterobacter aerogenes")
})

test_that("an analyst override still outranks the curated name", {
  fx <- .clean_fixture()
  overrides <- tibble::tibble(
    link_id = "L1", field = "organism", cleaned_value = "Klebsiella variicola",
    source_hint = "manual", rationale = "sequencing result",
    edited_at = as.POSIXct("2026-02-01 10:00:00", tz = "UTC"), edited_by = "analyst"
  )
  cleaned <- build_cleaned(fx$links, overrides, fx$vitek, fx$specimens)

  expect_equal(cleaned$clean_organism, "Klebsiella variicola")
  expect_equal(cleaned$clean_organism_rank, "override")
  # The parsed parts are cleared so nothing contradicts the override.
  expect_true(is.na(cleaned$clean_organism_genus))
  expect_true(is.na(cleaned$clean_organism_species))
  expect_equal(cleaned$v_organism, "Ent.aerogenes")
})

test_that("a non-identification produces no cleaned organism, not the raw string", {
  fx <- .clean_fixture(code = "NOSP", name = "Low Discrim Organism")
  cleaned <- build_cleaned(fx$links, overrides = NULL, fx$vitek, fx$specimens)

  expect_true(is.na(cleaned$clean_organism))
  expect_equal(cleaned$clean_organism_rank, "unidentified")
  expect_equal(cleaned$v_organism, "Low Discrim Organism")
})

test_that("an unmapped code falls back to the raw Vitek name rather than blanking", {
  fx <- .clean_fixture(code = "ZZZZ", name = "New.organism")
  cleaned <- build_cleaned(fx$links, overrides = NULL, fx$vitek, fx$specimens)

  expect_equal(cleaned$clean_organism, "New.organism")
  expect_equal(cleaned$clean_organism_rank, "unmapped")
})

test_that("a complex keeps its granularity and is not resolved to a species", {
  fx <- .clean_fixture(code = "EECG", name = "Ent.cloacae complex")
  cleaned <- build_cleaned(fx$links, overrides = NULL, fx$vitek, fx$specimens)

  expect_equal(cleaned$clean_organism, "Enterobacter cloacae complex")
  expect_equal(cleaned$clean_organism_rank, "complex")
  expect_true(is.na(cleaned$clean_organism_species))
})

test_that("a subspecies is collapsed to the species with the subspecies retained", {
  fx <- .clean_fixture(code = "EKZ", name = "K.pneum.ozaenae")
  cleaned <- build_cleaned(fx$links, overrides = NULL, fx$vitek, fx$specimens)

  expect_equal(cleaned$clean_organism, "Klebsiella pneumoniae")
  expect_equal(cleaned$clean_organism_subspecies, "ozaenae")
  expect_equal(cleaned$clean_organism_rank, "subspecies")
})

test_that("build_cleaned still works when the Vitek table has no organism_code", {
  fx <- .clean_fixture()
  vitek <- fx$vitek |> dplyr::select(-organism_code)
  cleaned <- build_cleaned(fx$links, overrides = NULL, vitek, fx$specimens)

  # No code to look up, so the raw name carries through and is flagged.
  expect_equal(cleaned$clean_organism, "Ent.aerogenes")
  expect_equal(cleaned$clean_organism_rank, "unmapped")
})

test_that("the curated organism columns reach the exported CSV", {
  fx <- .clean_fixture()
  cleaned <- build_cleaned(fx$links, overrides = NULL, fx$vitek, fx$specimens)
  out_dir <- withr::local_tempdir()

  export_cleaned_dataset(
    cleaned = cleaned, cleaned_ast = cleaned_ast_empty(), batch_id = "B-1",
    specimens = fx$specimens, output_dir = out_dir, formats = "csv", conn = NULL
  )

  written <- readr::read_csv(file.path(out_dir, "AXIS_clean_B-1_isolates.csv"),
                             show_col_types = FALSE, progress = FALSE)
  expect_true(all(c("clean_organism", "clean_organism_genus", "clean_organism_species",
                    "clean_organism_subspecies", "clean_organism_rank",
                    "v_organism", "v_organism_code") %in% names(written)))
  expect_equal(written$clean_organism, "Klebsiella aerogenes")
  expect_equal(written$v_organism, "Ent.aerogenes")
})

test_that("cleaned_empty carries the curated organism columns", {
  nm <- names(cleaned_empty())
  expect_true(all(c("v_organism_code", "clean_organism", "clean_organism_genus",
                    "clean_organism_species", "clean_organism_subspecies",
                    "clean_organism_rank") %in% nm))
})

test_that("a missing key file degrades to the raw Vitek names with a warning", {
  withr::local_options(list(axis.organism_key_path = tempfile(fileext = ".csv")))
  clear_organism_key_cache()
  withr::defer(clear_organism_key_cache())

  expect_warning(key <- load_organism_key(), "Organism key not found")
  expect_equal(nrow(key), 0L)

  res <- resolve_organism_names("ECO", "Esch.coli", key = key)
  expect_equal(res$clean_organism, "Esch.coli")
  expect_equal(res$clean_organism_rank, "unmapped")
})
