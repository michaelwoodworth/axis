library(tibble)

source("../../R/data_dedup.R")

test_that("dedup_vitek supports latest, first, and manual/flag rules", {
  raw <- tibble::tibble(
    lab_id = c("A1", "A1", "B1"),
    isolate_number = c("1", "1", "1"),
    file_name = c("old.xlsx", "new.xlsx", "only.xlsx"),
    organism_name = c("old", "new", "only")
  )

  latest <- dedup_vitek(raw, "latest")
  first <- dedup_vitek(raw, "first")
  manual <- dedup_vitek(raw, "manual")

  expect_equal(nrow(latest), 2)
  expect_equal(latest$organism_name[latest$lab_id == "A1"], "new")
  expect_equal(first$organism_name[first$lab_id == "A1"], "old")

  expect_equal(nrow(manual), 3)
  expect_true(all(manual$conflict[manual$lab_id == "A1"]))
  expect_false(manual$conflict[manual$lab_id == "B1"])
})
