library(dplyr)
library(purrr)
library(tibble)

source("../../R/data_parse_cfu.R")

test_that("CFU parser handles scientific notation, units, censoring, and flags", {
  parsed <- parse_cfu(c(
    "2.75 x 10^4",
    "5.1 x 10^5/gram of stool",
    ">1x10^6",
    "35.6x10^6 CFU/mg",
    "6.35e7",
    "42.7x10^5",
    "11",
    "0.37",
    "TNTC"
  ))

  expect_equal(parsed$cfu_value[[1]], 2.75e4, tolerance = 1)
  expect_equal(parsed$cfu_unit[[2]], "CFU/g stool")
  expect_equal(parsed$cfu_value[[2]], 5.1e5, tolerance = 1)
  expect_true(parsed$cfu_censored[[3]])
  expect_equal(parsed$cfu_value[[3]], 1e6, tolerance = 1)
  expect_equal(parsed$cfu_unit[[4]], "CFU/mg")
  expect_equal(parsed$cfu_value[[4]], 3.56e7, tolerance = 1)
  expect_equal(parsed$cfu_value[[5]], 6.35e7, tolerance = 1)
  expect_equal(parsed$cfu_flag[[6]], "renormalized")
  expect_equal(parsed$cfu_value[[6]], 4.27e6, tolerance = 1)
  expect_equal(parsed$cfu_flag[[7]], "ambiguous_bare")
  expect_equal(parsed$cfu_flag[[8]], "below_floor")
  expect_equal(parsed$cfu_flag[[9]], "unparseable")
  expect_true(is.na(parsed$cfu_log10[[9]]))
})

test_that("CFU parser preserves DP and EB growth methods for values and pseudocounts", {
  parsed <- parse_cfu(
    raw = c("Yes (EB)", NA_character_, "2.1 x 10^5 (DP)"),
    growth_flag = c(NA_character_, "Yes (DP)", NA_character_),
    cohort_floor_log10 = 1
  )

  expect_equal(parsed$growth_method, c("EB", "DP", "DP"))
  expect_equal(parsed$is_pseudocount, c(TRUE, TRUE, FALSE))
  expect_equal(parsed$cfu_log10[[1]], 1)
  expect_equal(parsed$cfu_log10[[2]], 1)
  expect_equal(parsed$cfu_value[[2]], 10)
  expect_equal(parsed$cfu_value[[3]], 2.1e5, tolerance = 1)
})
