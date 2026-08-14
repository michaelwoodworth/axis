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

test_that("manual link choices carry every eligible record, including late cohorts", {
  # Records are ordered by the file they were parsed from. A cohort loaded last
  # is the one a truncated dropdown hides, so it must survive this helper.
  specimens <- tibble::tibble(
    os_identifier = c(sprintf("A-%04d", 1:1500), "1436914"),
    specimen_label = c(sprintf("ARG%04d_P1", 1:1500), "SNT0002_d14_env_CRE1of1"),
    cp_short_title = c(rep("ARRRRG 2.0", 1500), "SNT/APPS/React"),
    type = c(rep("Aliquot", 1500), "Cryopreserved Cells")
  )

  choices <- manual_link_os_choices(specimens)

  expect_equal(length(choices), 1501L)
  expect_true("1436914" %in% choices)
  expect_true(any(grepl("SNT0002_d14_env_CRE1of1", names(choices), fixed = TRUE)))
  # The label carries identifier, specimen label, protocol and type so any of
  # them can be typed to find the record.
  late <- names(choices)[choices == "1436914"]
  expect_match(late, "1436914")
  expect_match(late, "SNT/APPS/React")
  expect_match(late, "Cryopreserved Cells")
})

test_that("manual link choices drop records without a usable identifier", {
  specimens <- tibble::tibble(
    os_identifier = c("OS1", NA_character_, "  "),
    specimen_label = c("a", "b", "c"),
    cp_short_title = "CP", type = "Aliquot"
  )
  expect_equal(unname(manual_link_os_choices(specimens)), "OS1")
  expect_equal(length(manual_link_os_choices(NULL)), 0L)
  expect_equal(length(manual_link_os_choices(tibble::tibble())), 0L)
})

test_that("manual link choices tolerate missing descriptive columns", {
  specimens <- tibble::tibble(os_identifier = "OS1")
  choices <- manual_link_os_choices(specimens)
  expect_equal(unname(choices), "OS1")
  expect_match(names(choices), "OS1")
})

test_that("the manual link caption names every loaded collection protocol", {
  specimens <- tibble::tibble(
    os_identifier = sprintf("OS-%03d", 1:6),
    specimen_label = sprintf("L%03d", 1:6),
    cp_short_title = c("REACT", "REACT", "REACT", "SNT/APPS/React", "FAIR 618", NA),
    type = "Aliquot"
  )
  caption <- manual_link_os_summary(specimens)

  expect_match(caption, "^6 records loaded")
  expect_match(caption, "REACT 3", fixed = TRUE)
  expect_match(caption, "SNT/APPS/React 1", fixed = TRUE)
  expect_match(caption, "FAIR 618 1", fixed = TRUE)
  expect_match(caption, "(no collection protocol) 1", fixed = TRUE)
})

test_that("the manual link caption is explicit when nothing is loaded", {
  expect_match(manual_link_os_summary(NULL), "No eligible OpenSpecimen records")
  expect_match(manual_link_os_summary(tibble::tibble()), "No eligible OpenSpecimen records")
})

test_that("the dropdown render cap is small enough to be honest about", {
  # A cap near selectize's 1000 default reads as a complete list. Keeping it
  # small is what makes the "type to search" caption believable.
  expect_lte(.LK_MAX_DROPDOWN_OPTIONS, 100L)
  expect_gte(.LK_MAX_DROPDOWN_OPTIONS, 10L)
})
