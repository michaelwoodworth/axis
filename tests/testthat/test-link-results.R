source("../../R/import_vitek.R")
source("../../R/import_openspecimen.R")
source("../../R/link_results.R")
source("../../R/summarize_inventory.R")

testthat::test_that("link_results links synthetic susceptibility rows to inventory", {
  vitek <- import_vitek("../fixtures/synthetic_vitek2.csv")
  specimens <- import_openspecimen("../fixtures/synthetic_openspecimen.csv")

  linked <- link_results(vitek, specimens)

  testthat::expect_equal(sum(linked$link_status == "matched"), 3)
  testthat::expect_equal(sum(linked$link_status == "unmatched"), 1)
  testthat::expect_true("mdro_category" %in% names(linked))
  testthat::expect_true(all(c("ESBL", "CRE", "VRE") %in% linked$mdro_category))
})

testthat::test_that("summarize_inventory counts linked synthetic specimens", {
  vitek <- import_vitek("../fixtures/synthetic_vitek2.csv")
  specimens <- import_openspecimen("../fixtures/synthetic_openspecimen.csv")

  summary <- summarize_inventory(link_results(vitek, specimens))

  testthat::expect_equal(sum(summary$specimen_count), 3)
  testthat::expect_true(all(c("mdro_category", "specimen_count") %in% names(summary)))
})
