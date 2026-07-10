library(dplyr)
library(tibble)

source("../../R/store.R")

test_that("manual links retain their confirmation method", {
  candidate <- tibble::tibble(
    lab_id = "1619001ESBL1",
    isolate_number = "1",
    os_identifier = "PA-I1",
    project_id = "PRE_ALERT",
    specimen_label = "1619001ESBL1",
    cp_short_title = "Pre Alert Legacy",
    score = NA_real_
  )

  link <- build_links_from_matches(
    candidate,
    batch_id = "B-test",
    match_method = "manual_selected"
  )

  expect_equal(link$match_method, "manual_selected")
  expect_equal(link$state, "confirmed")
  expect_equal(link$confidence, 0)
  expect_true(nzchar(link$link_id))
})
