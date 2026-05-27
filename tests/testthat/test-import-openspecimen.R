source("../../R/import_openspecimen.R")

testthat::test_that("import_openspecimen normalizes synthetic inventory exports", {
  data <- import_openspecimen("../fixtures/synthetic_openspecimen.csv")

  testthat::expect_equal(nrow(data), 4)
  testthat::expect_equal(data$specimen_key[1], "SYNSPEC001")
  testthat::expect_type(data$quantity, "double")
  testthat::expect_s3_class(data$collection_date, "Date")
})
