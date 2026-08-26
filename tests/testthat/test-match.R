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

test_that("SNT APPS REACT protocol family normalizes known title variants", {
  known_titles <- c(
    "SNT", "Sentinel", "Sentinel-REACT", "APPS", "APPS 2", "APPS_2",
    "APPS _2", "react"
  )

  expect_equal(
    .protocol_family(known_titles),
    rep("SNT_APPS_REACT", length(known_titles))
  )
  expect_true(all(.cp_titles_overlap("SNT/APPS/React", known_titles)))
  expect_false(any(.cp_titles_overlap(
    "SNT/APPS/React",
    c("FAIR", "ARRRRG", "Pre-Alert", "MEPSD", "unknown", "APPS pilot")
  )))
})

test_that("Sentinel-REACT overlaps the shared SNT APPS REACT family", {
  expect_equal(.protocol_family("Sentinel-REACT"), "SNT_APPS_REACT")
  expect_true(.cp_titles_overlap("SNT/APPS/React", "Sentinel-REACT"))
  expect_true(.cp_titles_overlap("Sentinel / REACT", "APPS _2"))
})

test_that("SNT Vitek record can match an APPS 2 cryopreserved cell directly", {
  vitek <- tibble::tibble(
    lab_id = "SNT001ESBL1",
    isolate_number = "1",
    parsed_study = "SNT",
    parsed_subject = "SNT001",
    parsed_target = "ESBL",
    cp_hint = "SNT/APPS/React",
    testing_date = as.Date("2025-01-10"),
    organism_name = "Esch.coli"
  )

  specimens <- tibble::tibble(
    os_identifier = c("apps-2-correct", "fair-lookalike"),
    project_id = c("APPS_2", "FAIR"),
    specimen_label = c("SNT001ESBL1", "SNT001ESBL1"),
    cp_short_title = c("APPS _2", "FAIR 618"),
    participant_id = c("SNT001", "SNT001"),
    custom_collection_date = as.Date(c("2025-01-10", "2025-01-10")),
    custom_mdro = c("ESBL", "ESBL"),
    custom_organism = c("Escherichia coli", "Escherichia coli"),
    type = c("Cryopreserved Cells", "Cryopreserved Cells")
  )

  candidates <- auto_match(vitek, specimens, thresh_review = 0, parallel = FALSE)
  buckets <- bucket_results(candidates, vitek)

  expect_equal(candidates$os_identifier, "apps-2-correct")
  expect_equal(candidates$cp_score, 5L)
  expect_equal(buckets$matched$os_identifier, "apps-2-correct")
  expect_equal(nrow(buckets$review), 0)
})

test_that("shared SNT APPS REACT family is not enough for automatic matching", {
  vitek <- tibble::tibble(
    lab_id = "SNT001ESBL1",
    isolate_number = "1",
    parsed_study = "SNT",
    parsed_subject = "SNT001",
    parsed_target = "ESBL",
    cp_hint = "SNT/APPS/React",
    testing_date = as.Date("2025-01-10"),
    organism_name = "Esch.coli"
  )

  specimens <- tibble::tibble(
    os_identifier = "apps-2-wrong-record",
    project_id = "APPS_2",
    specimen_label = "SNT999ESBL1",
    cp_short_title = "APPS 2",
    participant_id = "SNT999",
    custom_collection_date = as.Date("2025-02-10"),
    custom_mdro = "CRE",
    custom_organism = "Klebsiella pneumoniae",
    type = "Cryopreserved Cells"
  )

  candidates <- auto_match(vitek, specimens, parallel = FALSE)
  buckets <- bucket_results(candidates, vitek)

  expect_equal(nrow(candidates), 0)
  expect_equal(nrow(buckets$matched), 0)
  expect_equal(buckets$none$lab_id, "SNT001ESBL1")
})

test_that("similarly scored SNT APPS candidates remain in needs-review", {
  vitek <- tibble::tibble(
    lab_id = "SNT001ESBL1",
    isolate_number = "1",
    parsed_study = "SNT",
    parsed_subject = "SNT001",
    parsed_target = "ESBL",
    cp_hint = "SNT/APPS/React",
    testing_date = as.Date("2025-01-10"),
    organism_name = "Esch.coli"
  )

  specimens <- tibble::tibble(
    os_identifier = c("apps-2-a", "apps-2-b"),
    project_id = "APPS_2",
    specimen_label = c("SNT001ESBL1", "SNT001ESBL1"),
    cp_short_title = c("APPS 2", "APPS_2"),
    participant_id = "SNT001",
    custom_collection_date = as.Date(c("2025-01-10", "2025-01-10")),
    custom_mdro = "ESBL",
    custom_organism = "Escherichia coli",
    type = "Cryopreserved Cells"
  )

  candidates <- auto_match(vitek, specimens, parallel = FALSE)
  buckets <- bucket_results(candidates, vitek)

  expect_equal(nrow(buckets$matched), 0)
  expect_equal(sort(buckets$review$os_identifier), c("apps-2-a", "apps-2-b"))
  expect_equal(nrow(buckets$none), 0)
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

# ── Duplicate glycerol aliquots ──────────────────────────────────────────────

.glycerol_specimens <- function(include_parent = TRUE) {
  parent <- tibble::tibble(
    os_identifier = "OS-parent", participant_id = "ARG030",
    specimen_label = "ARG030_P1ESBL1", specimen_label_raw = "ARG030_P1ESBL1",
    cp_short_title = "ARRRRG 2.0", project_id = "ARRRRG",
    class = "Cell", type = "Cryopreserved Cells", lineage = "Derived",
    custom_organism = "Escherichia coli", custom_mdro = "ESBL",
    custom_collection_date = as.Date("2026-01-01")
  )
  aliquot <- parent |>
    dplyr::mutate(
      os_identifier = "OS-glycerol",
      specimen_label_raw = "ARG030_P1ESBL1w/glycerol1",
      lineage = "Aliquot"
    )
  if (include_parent) dplyr::bind_rows(parent, aliquot) else aliquot
}

test_that("a glycerol aliquot is not offered alongside the specimen it came from", {
  prepared <- .prepare_match_specimens(.glycerol_specimens())

  expect_equal(sum(prepared$.axis_is_duplicate_aliquot), 1L)
  expect_true(prepared$.axis_is_duplicate_aliquot[
    prepared$os_identifier == "OS-glycerol"])
  expect_false(prepared$.axis_is_duplicate_aliquot[
    prepared$os_identifier == "OS-parent"])

  # The type and class tests alone never caught these: both records are
  # Cell / Cryopreserved Cells.
  expect_false(any(prepared$.axis_is_review_aliquot))
})

test_that("a glycerol aliquot whose parent is absent is still reachable", {
  prepared <- .prepare_match_specimens(.glycerol_specimens(include_parent = FALSE))
  expect_false(any(prepared$.axis_is_duplicate_aliquot))
})

test_that("auto_match returns one candidate per specimen, not one per aliquot", {
  vitek <- tibble::tibble(
    lab_id = "ARG030P1ESBL1", isolate_number = "1",
    parsed_study = "ARRRRG", parsed_subject = "ARG030", parsed_target = "ESBL",
    cp_hint = "ARRRRG 2.0", organism_name = "Escherichia coli",
    testing_date = as.Date("2026-01-01")
  )
  cands <- auto_match(vitek, .glycerol_specimens(), parallel = FALSE)

  expect_equal(nrow(cands), 1L)
  expect_equal(cands$os_identifier, "OS-parent")
})

# ── Accession label normalisation ────────────────────────────────────────────

test_that("every separator convention normalises to the same accession key", {
  expect_equal(.norm_accession_label("ARG026_P2"), "ARG026P2")
  expect_equal(.norm_accession_label("APPS0028_env_ESBL#1of1"), "APPS0028ENVESBL1OF1")
  expect_equal(.norm_accession_label("SNT0002_d14_env_CRE1of1"), "SNT0002D14ENVCRE1OF1")
  # The '#' in APPS labels used to survive, which zeroed the label signal for
  # that entire cohort.
  expect_false(grepl("#", .norm_accession_label("APPS0028_ig_dp_CRE#3of3")))
})

test_that("ordered subsequence detects an inserted token", {
  expect_true(.is_ordered_subsequence("APPS0028IGCRE3OF3", "APPS0028IGDPCRE3OF3"))
  expect_false(.is_ordered_subsequence("APPS0028IGDPCRE3OF3", "APPS0028IGCRE3OF3"))
  expect_false(.is_ordered_subsequence("APPS0029IGCRE3OF3", "APPS0028IGDPCRE3OF3"))
  expect_false(.is_ordered_subsequence("", "ABC"))
  expect_false(.is_ordered_subsequence(NA_character_, "ABC"))
})

test_that("an APPS isolate scores its own OpenSpecimen record highest", {
  vitek <- tibble::tibble(
    lab_id = "APPS0028igCRE3of3", isolate_number = "1",
    parsed_study = "APPS", parsed_subject = "APPS0028", parsed_target = "CRE",
    cp_hint = "SNT/APPS/React", organism_name = "Klebsiella pneumoniae",
    testing_date = as.Date("2026-01-05")
  )
  specimens <- tibble::tibble(
    os_identifier = c("OS-right", "OS-wrong"),
    participant_id = "APPS0028",
    specimen_label = c("APPS0028_ig_dp_CRE#3of3", "APPS0028_pr_dp_ESBL#1of1"),
    cp_short_title = "SNT/APPS/React", project_id = "SNT",
    class = "Cell", type = "Cryopreserved Cells", lineage = "Derived",
    custom_organism = "Klebsiella pneumoniae",
    custom_mdro = c("CRE", "ESBL"),
    custom_collection_date = as.Date("2026-01-05")
  )

  cands <- auto_match(vitek, specimens, parallel = FALSE) |>
    dplyr::arrange(dplyr::desc(score))

  expect_equal(cands$os_identifier[[1]], "OS-right")
  expect_equal(cands$label_match_kind[[1]], "subsequence")
  expect_gt(cands$score[[1]], cands$score[[2]])
})

test_that("a subsequence-only label never reaches the auto-matched bucket", {
  candidates <- tibble::tibble(
    lab_id = "APPS0028igCRE3of3", isolate_number = "1",
    os_identifier = "OS-right", project_id = "SNT",
    specimen_label = "APPS0028_ig_dp_CRE#3of3", cp_short_title = "SNT/APPS/React",
    score = 100L, label_match_kind = "subsequence",
    mdro_disagree = FALSE, organism_disagree = FALSE
  )
  vitek <- tibble::tibble(lab_id = "APPS0028igCRE3of3", isolate_number = "1")

  buckets <- bucket_results(candidates, vitek)

  expect_equal(nrow(buckets$matched), 0L)
  expect_equal(nrow(buckets$review), 1L)
})

test_that("an exact label match at the same score does auto-match", {
  candidates <- tibble::tibble(
    lab_id = "ARG030P1ESBL1", isolate_number = "1",
    os_identifier = "OS-parent", project_id = "ARRRRG",
    specimen_label = "ARG030_P1ESBL1", cp_short_title = "ARRRRG 2.0",
    score = 100L, label_match_kind = "exact",
    mdro_disagree = FALSE, organism_disagree = FALSE
  )
  vitek <- tibble::tibble(lab_id = "ARG030P1ESBL1", isolate_number = "1")

  expect_equal(nrow(bucket_results(candidates, vitek)$matched), 1L)
})
