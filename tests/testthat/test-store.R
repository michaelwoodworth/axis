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

test_that("insert_new_links returns the rows it actually inserted", {
  path <- tempfile(fileext = ".duckdb")
  conn <- open_db(path)
  withr::defer(close_db(conn))

  links <- build_links_from_matches(
    tibble::tibble(
      lab_id = c("A1", "B1"), isolate_number = c("1", "1"),
      os_identifier = c("OS-A", "OS-B"), project_id = "P",
      specimen_label = c("A1", "B1"), cp_short_title = "CP",
      score = c(90, 90)
    ),
    batch_id = "B-test"
  )

  inserted <- insert_new_links(conn, links)
  expect_equal(nrow(inserted), 2L)
  expect_setequal(inserted$os_identifier, c("OS-A", "OS-B"))

  # The same logical links a second time insert nothing, even though
  # build_links_from_matches() minted fresh link_id values.
  again <- insert_new_links(conn, build_links_from_matches(
    tibble::tibble(
      lab_id = c("A1", "B1"), isolate_number = c("1", "1"),
      os_identifier = c("OS-A", "OS-B"), project_id = "P",
      specimen_label = c("A1", "B1"), cp_short_title = "CP",
      score = c(90, 90)
    ),
    batch_id = "B-test"
  ))
  expect_equal(nrow(again), 0L)
  expect_equal(nrow(read_table(conn, "links_confirmed")), 2L)
})

test_that("links_confirmed_empty has the schema build_links_from_matches produces", {
  empty <- links_confirmed_empty()
  expect_equal(nrow(empty), 0L)
  expect_equal(
    names(empty),
    names(build_links_from_matches(NULL, batch_id = "B-test"))
  )
})
