library(tibble)

source("../../R/mod_linking.R")

test_that("manual linking includes ordinary OpenSpecimen aliquots", {
  specimens <- tibble::tibble(
    os_identifier = c("snt-isolate", "cryo-isolate"),
    specimen_label = c("SNT0002_d14_env_CRE1of1", "ARG026ESBL1"),
    type = c("Aliquot", "Cryopreserved Cells")
  )

  choices <- manual_link_specimen_choices(specimens)

  expect_equal(choices$os_identifier, c("snt-isolate", "cryo-isolate"))
  expect_true("SNT0002_d14_env_CRE1of1" %in% choices$specimen_label)
})

test_that("manual linking excludes records without a usable identifier", {
  specimens <- tibble::tibble(
    os_identifier = c("valid-id", NA_character_, "", "   "),
    specimen_label = c("valid", "missing", "blank", "spaces")
  )

  choices <- manual_link_specimen_choices(specimens)

  expect_equal(choices$os_identifier, "valid-id")
})

test_that("manual linking handles unavailable specimen data", {
  expect_equal(nrow(manual_link_specimen_choices(NULL)), 0L)
  expect_equal(nrow(manual_link_specimen_choices(tibble::tibble())), 0L)
})
