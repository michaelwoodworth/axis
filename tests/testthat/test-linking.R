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

test_that("the saved-versus-exported indicator distinguishes the three states", {
  fresh <- export_state_message(0L, FALSE)
  expect_equal(fresh$state, "current")
  expect_match(fresh$label, "No changes waiting")

  one <- export_state_message(1L, FALSE)
  expect_equal(one$state, "stale")
  expect_match(one$label, "^1 change saved")

  many <- export_state_message(20L, FALSE)
  expect_equal(many$state, "stale")
  expect_match(many$label, "^20 changes saved")

  failed <- export_state_message(3L, TRUE)
  expect_equal(failed$state, "failed")
  expect_match(failed$label, "Export failed")
  expect_match(failed$label, "3 saved changes")
})

test_that("the indicator tolerates missing or malformed counts", {
  expect_equal(export_state_message(NA_integer_, FALSE)$state, "current")
  expect_equal(export_state_message(-4L, FALSE)$state, "current")
})
