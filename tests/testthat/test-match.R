library(dplyr)
library(purrr)
library(tibble)

source("../../R/data_match.R")

test_that("auto_match ranks accession/label matches above weaker candidates", {
  vitek <- tibble::tibble(
    lab_id = "6180012ESBL1",
    isolate_number = "1",
    parsed_subject = "6180012",
    parsed_target = "ESBL",
    cp_hint = "FAIR 618",
    testing_date = as.Date("2025-01-10")
  )

  specimens <- tibble::tibble(
    os_identifier = c("os-strong", "os-weak"),
    project_id = c("FAIR", "FAIR"),
    specimen_label = c("6180012ESBL1", "6180099ESBL1"),
    cp_short_title = c("FAIR 618", "FAIR 618"),
    participant_id = c("6180012", "6180099"),
    custom_collection_date = as.Date(c("2025-01-10", "2025-01-10")),
    custom_mdro = c("ESBL", "ESBL")
  )

  candidates <- auto_match(vitek, specimens, thresh_review = 0)

  expect_equal(candidates$os_identifier[1], "os-strong")
  expect_gt(candidates$score[1], candidates$score[2])
  expect_false(candidates$mdro_disagree[1])
})

test_that("auto_match flags MDRO disagreement", {
  vitek <- tibble::tibble(
    lab_id = "ARG026ESBL1",
    isolate_number = "1",
    parsed_subject = "ARG026",
    parsed_target = "ESBL",
    cp_hint = "ARRRRG 2.0",
    testing_date = as.Date("2025-01-10")
  )

  specimens <- tibble::tibble(
    os_identifier = "os-disagree",
    project_id = "ARRRRG",
    specimen_label = "ARG026ESBL1",
    cp_short_title = "ARRRRG 2.0",
    participant_id = "ARG026",
    custom_collection_date = as.Date("2025-01-10"),
    custom_mdro = "CRE"
  )

  candidates <- auto_match(vitek, specimens, thresh_review = 0)
  expect_true(candidates$mdro_disagree[1])
})

test_that("auto_match normalizes organism labels and filters to cryopreserved cells", {
  vitek <- tibble::tibble(
    lab_id = "ARG026_P2",
    isolate_number = "1",
    parsed_subject = "ARG026",
    parsed_target = "ESBL",
    cp_hint = "ARRRRG 2.0",
    testing_date = as.Date("2025-01-10"),
    organism_name = "Esch.coli"
  )

  specimens <- tibble::tibble(
    os_identifier = c("parent", "isolate"),
    project_id = "ARRRRG",
    specimen_label = c("ARG026_P2", "ARG026_P2"),
    cp_short_title = "ARRRRG 2.0",
    participant_id = "ARG026",
    custom_collection_date = as.Date("2025-01-10"),
    custom_mdro = "ESBL",
    custom_organism = "Escherichia coli",
    type = c("Perirectal eSwab", "Cryopreserved Cells")
  )

  candidates <- auto_match(vitek, specimens, thresh_review = 0)

  expect_equal(nrow(candidates), 1)
  expect_equal(candidates$os_identifier, "isolate")
  expect_equal(candidates$organism_score, 10L)
  expect_equal(candidates$cryo_score, 10L)
})

test_that("auto_match excludes banked aliquots but retains Cryopreserved Cells", {
  vitek <- tibble::tibble(
    lab_id = "1610123ESBL1",
    isolate_number = "1",
    parsed_study = "PRE_ALERT",
    parsed_subject = "1610123",
    parsed_target = "ESBL",
    cp_hint = "Pre-Alert",
    testing_date = as.Date("2025-01-10"),
    organism_name = "Esch.coli"
  )

  specimens <- tibble::tibble(
    os_identifier = c("banked-aliquot", "cryo-isolate"),
    project_id = "PRE_ALERT",
    specimen_label = c("1610123ESBL1", "1610123ESBL1"),
    cp_short_title = c("Pre Alert", "Pre Alert"),
    participant_id = "1610123",
    custom_collection_date = as.Date("2025-01-10"),
    custom_mdro = "ESBL",
    custom_organism = "Escherichia coli",
    type = c("Aliquot", "Cryopreserved Cells"),
    class = c("Aliquot", "Aliquot")
  )

  candidates <- auto_match(vitek, specimens, thresh_review = 0)

  expect_equal(nrow(candidates), 1)
  expect_equal(candidates$os_identifier, "cryo-isolate")
  expect_equal(candidates$parsed_study, "PRE_ALERT")
  expect_match(candidates$match_explanation, "study=PRE_ALERT")
})

test_that("auto_match ignores ARRRRG underscores and REACT case differences", {
  vitek <- tibble::tibble(
    lab_id = c("ARG026P2CRE1", "bEM037PRAE1"),
    isolate_number = c("1", "1"),
    parsed_subject = c("ARG026", "bEM037"),
    parsed_target = c("CRE", NA_character_),
    cp_hint = c("ARRRRG 2.0", NA_character_),
    testing_date = as.Date(c("2025-01-10", "2025-01-10")),
    organism_name = c("K.pneumoniae", "Ps.aeruginosa")
  )

  specimens <- tibble::tibble(
    os_identifier = c("arrrg", "react"),
    project_id = c("ARRRRG", "REACT"),
    specimen_label = c("ARG026_P2CRE1", "bEM037PRaE1"),
    cp_short_title = c("ARRRRG 2.0", "REACT"),
    participant_id = c("ARG026", "BEM037"),
    custom_collection_date = as.Date(c("2025-01-10", "2025-01-10")),
    custom_mdro = c("CRE", NA_character_),
    custom_organism = c("Klebsiella pneumoniae", "Pseudomonas aeruginosa"),
    type = c("Cryopreserved Cells", "Cryopreserved Cells")
  )

  candidates <- auto_match(vitek, specimens, thresh_review = 0)

  expect_true(any(candidates$lab_id == "ARG026P2CRE1" &
                    candidates$os_identifier == "arrrg" &
                    candidates$label_score == 60L))
  expect_true(any(candidates$lab_id == "bEM037PRAE1" &
                    candidates$os_identifier == "react" &
                    candidates$label_score == 60L))
})

test_that("auto_match parallel path matches serial scoring", {
  vitek <- tibble::tibble(
    lab_id = c("ARG026P2CRE1", "bEM037PRAE1"),
    isolate_number = c("1", "1"),
    parsed_subject = c("ARG026", "bEM037"),
    parsed_target = c("CRE", NA_character_),
    cp_hint = c("ARRRRG 2.0", NA_character_),
    testing_date = as.Date(c("2025-01-10", "2025-01-10")),
    organism_name = c("K.pneumoniae", "Ps.aeruginosa")
  )

  specimens <- tibble::tibble(
    os_identifier = c("arrrg", "react"),
    project_id = c("ARRRRG", "REACT"),
    specimen_label = c("ARG026_P2CRE1", "bEM037PRaE1"),
    cp_short_title = c("ARRRRG 2.0", "REACT"),
    participant_id = c("ARG026", "BEM037"),
    custom_collection_date = as.Date(c("2025-01-10", "2025-01-10")),
    custom_mdro = c("CRE", NA_character_),
    custom_organism = c("Klebsiella pneumoniae", "Pseudomonas aeruginosa"),
    type = c("Cryopreserved Cells", "Cryopreserved Cells")
  )

  serial <- auto_match(vitek, specimens, thresh_review = 0, parallel = FALSE)
  parallel <- auto_match(vitek, specimens, thresh_review = 0, parallel = TRUE, workers = 2)

  expect_equal(
    serial |> dplyr::arrange(lab_id, isolate_number, os_identifier),
    parallel |> dplyr::arrange(lab_id, isolate_number, os_identifier)
  )
})

test_that("bucket_results sends high-scoring organism disagreements to review", {
  vitek <- tibble::tibble(
    lab_id = "ARG027SESBL1",
    isolate_number = "1"
  )

  candidates <- tibble::tibble(
    lab_id = "ARG027SESBL1",
    isolate_number = "1",
    os_identifier = "OS1",
    project_id = "ARRRRG",
    specimen_label = "ARG027_SESBL1",
    cp_short_title = "ARRRRG 2.0",
    score = 100L,
    label_score = 60L,
    subject_score = 35L,
    mdro_score = 15L,
    organism_score = 0L,
    date_score = 10L,
    cp_score = 5L,
    cryo_score = 10L,
    date_diff_days = 0,
    mdro_disagree = FALSE,
    organism_disagree = TRUE
  )

  buckets <- bucket_results(candidates, vitek)

  expect_equal(nrow(buckets$matched), 0)
  expect_equal(nrow(buckets$review), 1)
  expect_equal(nrow(buckets$none), 0)
})

test_that("match bucket counts use Vitek isolate keys instead of candidate rows", {
  buckets <- list(
    matched = tibble::tibble(
      lab_id = "L1",
      isolate_number = "1",
      os_identifier = "OS1"
    ),
    review = tibble::tibble(
      lab_id = c("L2", "L2", "L2", "L2"),
      isolate_number = c("1", "1", "2", "2"),
      os_identifier = c("OS2", "OS3", "OS4", "OS5")
    ),
    none = tibble::tibble(
      lab_id = "L3",
      isolate_number = "1"
    )
  )

  counts <- match_bucket_counts(buckets)

  expect_equal(counts$matched, 1L)
  expect_equal(counts$review, 2L)
  expect_equal(counts$none, 1L)
})
