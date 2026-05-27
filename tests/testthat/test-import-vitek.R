source("../../R/import_vitek.R")

testthat::test_that("import_vitek normalizes synthetic VITEK2 exports", {
  data <- import_vitek("../fixtures/synthetic_vitek2.csv")

  testthat::expect_equal(nrow(data), 4)
  testthat::expect_true(all(c("isolate_id", "specimen_key", "interpretation") %in% names(data)))
  testthat::expect_equal(data$specimen_key[1], "SYNSPEC001")
  testthat::expect_s3_class(data$test_date, "Date")
  testthat::expect_false(any(grepl("patient|mrn|dob", names(data), ignore.case = TRUE)))
})
