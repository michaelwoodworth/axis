# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/data_match.R — Multi-signal match scoring (no fuzzyjoin dependency)
#
# SCORING (0 – 125 pts, normalised to 0-100 after capping)
#
#   Signal                          Max pts  Key insight
#   ─────────────────────────────── ───────  ──────────────────────────────────
#   Label match                        60    lab_id == specimen_label (exact)
#                                            or lab_id substring of specimen_label
#   Subject match                       35   parsed_subject == participant_id
#   MDRO target match                   15   parsed_target == custom_mdro (token)
#   Organism match                      10   Vitek organism == OS organism
#   Date proximity                      10   testing_date vs custom_collection_date
#   CP hint match                        5   cp_hint == cp_short_title (prefix OK)
#   Specimen type                       10   OS candidate is Cryopreserved Cells
#
# Thresholds (overridable):
#   thresh_auto   ≥ 80 → auto-matched
#   thresh_review ≥ 50 → needs-review
#   < thresh_review   → no match
#
# MDRO-disagreement flag:
#   mdro_disagree = TRUE when both parsed_target and custom_mdro are non-NA
#   and they do NOT agree on the same token.
# ─────────────────────────────────────────────────────────────────────────────

# ── MDRO token normaliser ─────────────────────────────────────────────────────

.MDRO_NORM <- c(
  ESBL  = "ESBL", esbl = "ESBL",
  CRE   = "CRE",  cre  = "CRE",
  CRKP  = "CRE",  crkp = "CRE",   # CRE Klebsiella maps to CRE family
  CREC  = "CRE",  crec = "CRE",
  VRE   = "VRE",  vre  = "VRE",
  MDRP  = "MDRP", mdrp = "MDRP",
  CRAB  = "CRAB", crab = "CRAB",
  CRPA  = "CRPA", crpa = "CRPA",
  MRSA  = "MRSA", mrsa = "MRSA"
)

#' Normalise an MDRO string to a canonical token (or NA).
.norm_mdro <- function(x) {
  x <- trimws(toupper(as.character(x)))
  x[x %in% c("", "NA", "NONE", "N/A")] <- NA_character_
  # Try direct lookup first, then substring search
  result <- .MDRO_NORM[x]
  names(result) <- NULL
  # For unmatched, try each token as substring
  for (i in which(is.na(result) & !is.na(x))) {
    for (tok in names(.MDRO_NORM)) {
      if (grepl(tok, x[i], ignore.case = TRUE)) {
        result[i] <- .MDRO_NORM[[tok]]
        break
      }
    }
  }
  result
}

# ── Organism normaliser ───────────────────────────────────────────────────────

.ORGANISM_NORM <- c(
  "ESCH.COLI" = "Escherichia coli",
  "ESCHERICHIA COLI" = "Escherichia coli",
  "ENT.CLOACAE COMPLEX" = "Enterobacter cloacae_complex",
  "ENTEROBACTER CLOACAE COMPLEX" = "Enterobacter cloacae_complex",
  "ENTEROBACTER CLOACAE_COMPLEX" = "Enterobacter cloacae_complex",
  "PS.AERUGINOSA" = "Pseudomonas aeruginosa",
  "PSEUDOMONAS AERUGINOSA" = "Pseudomonas aeruginosa",
  "K.PNEUMONIAE" = "Klebsiella pneumoniae",
  "K.PNEUM.PNEUMONIAE" = "Klebsiella pneumoniae",
  "KLEBSIELLA PNEUMONIAE" = "Klebsiella pneumoniae",
  "PROV.STUARTII" = "Providencia stuartii",
  "PROVIDENCIA STUARTII" = "Providencia stuartii",
  "CITRO.FREUNDII" = "Citrobacter freundii",
  "CITROBACTER FREUNDII" = "Citrobacter freundii",
  "CITRO.BRAAKII" = "Citrobacter braakii",
  "CITROBACTER BRAAKII" = "Citrobacter braakii",
  "K.AEROGENES" = "Enterobacter aerogenes",
  "KLEBSIELLA AEROGENES" = "Enterobacter aerogenes",
  "ENTEROBACTER AEROGENES" = "Enterobacter aerogenes",
  "MORG.MORGANII" = "Morganella morganii",
  "MORGANELLA MORGANII" = "Morganella morganii",
  "K.OXYTOCA" = "Klebsiella oxytoca",
  "KLEBSIELLA OXYTOCA" = "Klebsiella oxytoca",
  "STENO.MALTOPHILIA" = "Stenotrophomonas maltophilia",
  "STENOTROPHOMONAS MALTOPHILIA" = "Stenotrophomonas maltophilia",
  "ENT.CLOACAE" = "Enterobacter cloacae_complex"
)

.norm_organism <- function(x) {
  raw <- trimws(as.character(x))
  raw[raw %in% c("", "NA", "N/A", "na", "n/a")] <- NA_character_
  key <- toupper(gsub("\\s+", " ", raw))
  key <- gsub("_", " ", key)
  norm <- .ORGANISM_NORM[key]
  names(norm) <- NULL
  norm[is.na(norm) & !is.na(raw)] <- raw[is.na(norm) & !is.na(raw)]
  norm
}

# ── Core scoring engine ───────────────────────────────────────────────────────

#' Run auto-match between vitek_unique and specimens.
#'
#' Uses a cross-join approach: for each Vitek isolate, finds candidate
#' specimens from matching projects (via cp_hint) or all specimens if no
#' cp_hint. Then scores each candidate pair.
#'
#' @param vitek_unique  tibble from dedup_vitek(). Must have:
#'   lab_id, isolate_number, parsed_study, parsed_subject, parsed_target,
#'   cp_hint, testing_date.
#' @param specimens     tibble from parse_os_specimens_multi(). Must have:
#'   os_identifier, specimen_label, cp_short_title, project_id,
#'   participant_id, custom_collection_date, custom_mdro.
#' @param thresh_auto   Numeric ≥ this → auto-matched (default 80).
#' @param thresh_review Numeric ≥ this → needs-review (default 50).
#' @param exclude_aliquots Logical. Exclude banked aliquot records from
#'   candidate review by default while retaining Cryopreserved Cells.
#' @return match_candidates tibble (all candidate pairs ≥ thresh_review).
auto_match <- function(vitek_unique, specimens,
                       thresh_auto   = 80,
                       thresh_review = 50,
                       exclude_aliquots = TRUE,
                       parallel = getOption("axis.match_parallel", TRUE),
                       workers = getOption("axis.match_workers", NULL)) {

  if (is.null(vitek_unique) || nrow(vitek_unique) == 0 ||
      is.null(specimens)    || nrow(specimens)    == 0)
    return(match_candidates_empty())

  # Ensure required columns exist with sensible defaults
  vitek_unique <- .ensure_col(vitek_unique, "cp_hint",        NA_character_)
  vitek_unique <- .ensure_col(vitek_unique, "parsed_study",   NA_character_)
  vitek_unique <- .ensure_col(vitek_unique, "parsed_subject", NA_character_)
  vitek_unique <- .ensure_col(vitek_unique, "parsed_target",  NA_character_)
  vitek_unique <- .ensure_col(vitek_unique, "testing_date",   as.Date(NA))
  vitek_unique <- .ensure_col(vitek_unique, "organism_name",   NA_character_)

  specimens <- .ensure_col(specimens, "participant_id",         NA_character_)
  specimens <- .ensure_col(specimens, "custom_collection_date", as.Date(NA))
  specimens <- .ensure_col(specimens, "custom_mdro",            NA_character_)
  specimens <- .ensure_col(specimens, "cp_short_title",         NA_character_)
  specimens <- .ensure_col(specimens, "custom_organism",        NA_character_)
  specimens <- .ensure_col(specimens, "type",                   NA_character_)
  specimens <- .ensure_col(specimens, "class",                  NA_character_)
  specimens <- .ensure_col(specimens, "lineage",                NA_character_)
  specimens <- .prepare_match_specimens(specimens)
  if (isTRUE(exclude_aliquots)) {
    specimens <- dplyr::filter(
      specimens,
      !.data$.axis_is_review_aliquot,
      !.data$.axis_is_duplicate_aliquot
    )
  }
  if (nrow(specimens) == 0) return(match_candidates_empty())

  # Score all pairs using vectorized per-isolate scoring. Specimen-side
  # normalized fields are precomputed once above; large jobs can split isolate
  # scoring across local cores on Unix-like systems.
  idx <- seq_len(nrow(vitek_unique))
  use_parallel <- isTRUE(parallel) &&
    .Platform$OS.type != "windows" &&
    length(idx) > 1L
  if (is.null(workers)) {
    detected_cores <- suppressWarnings(parallel::detectCores(logical = TRUE))
    if (is.null(detected_cores) || length(detected_cores) == 0L ||
        is.na(detected_cores[[1]]) || detected_cores[[1]] < 1L) {
      detected_cores <- 1L
    }
    workers <- max(1L, as.integer(detected_cores[[1]]) - 1L)
  }
  workers <- max(1L, min(as.integer(workers), length(idx)))

  pieces <- if (use_parallel && workers > 1L) {
    parallel::mclapply(
      idx,
      function(vi) .score_one_vitek(vitek_unique[vi, ], specimens),
      mc.cores = workers
    )
  } else {
    lapply(idx, function(vi) .score_one_vitek(vitek_unique[vi, ], specimens))
  }
  results <- dplyr::bind_rows(pieces)

  if (is.null(results) || nrow(results) == 0) return(match_candidates_empty())

  results |>
    dplyr::filter(score >= thresh_review) |>
    dplyr::arrange(lab_id, isolate_number, dplyr::desc(score))
}

# ── Bucketing ─────────────────────────────────────────────────────────────────

#' Bucket match_candidates into matched / review / none / confirmed.
#'
#' An isolate whose link an analyst has already confirmed is settled: it is
#' removed from all three working buckets and reported in $confirmed. Without
#' this, re-running auto-match regenerates candidates for confirmed isolates and
#' puts them back in the review queue, where confirming a second, different
#' OpenSpecimen record would leave the isolate linked to two specimens.
#'
#' @param match_candidates  tibble from auto_match().
#' @param vitek_unique      All deduplicated Vitek rows.
#' @param thresh_auto       Score ≥ this → auto-matched (default 80).
#' @param thresh_review     Score ≥ this → needs-review (default 50).
#' @param ambiguity_margin  A runner-up within this many points of the best
#'   candidate keeps the isolate in needs-review (default 5).
#' @param links_confirmed   Confirmed links, or NULL. Isolates appearing here are
#'   withheld from every working bucket.
#' @return Named list: $matched, $review, $none, $confirmed.
bucket_results <- function(match_candidates, vitek_unique,
                            thresh_auto = 80, thresh_review = 50,
                            ambiguity_margin = 5,
                            links_confirmed = NULL) {
  confirmed_keys <- .confirmed_isolate_keys(links_confirmed)

  vitek_unique <- .drop_confirmed_isolates(vitek_unique, confirmed_keys)
  match_candidates <- .drop_confirmed_isolates(match_candidates, confirmed_keys)

  empty <- list(
    matched = match_candidates_empty(),
    review  = match_candidates_empty(),
    none    = vitek_unique,
    confirmed = confirmed_keys
  )

  if (is.null(match_candidates) || nrow(match_candidates) == 0)
    return(empty)

  ambiguity_margin <- max(0, as.numeric(ambiguity_margin[[1]]))

  if (!"label_match_kind" %in% names(match_candidates)) {
    match_candidates$label_match_kind <- NA_character_
  }

  # Best score per (lab_id, isolate_number) pair. A close runner-up is kept in
  # needs-review so row order can never silently decide an uncertain match.
  best <- match_candidates |>
    dplyr::group_by(lab_id, isolate_number) |>
    dplyr::slice_max(score, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup()

  ambiguity <- match_candidates |>
    dplyr::group_by(lab_id, isolate_number) |>
    dplyr::summarise(
      best_score = max(score, na.rm = TRUE),
      similarly_scored = sum(score >= best_score - ambiguity_margin, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(has_ambiguity = .data$similarly_scored > 1L)

  disagreement <- best |>
    dplyr::left_join(
      dplyr::select(ambiguity, lab_id, isolate_number, has_ambiguity),
      by = c("lab_id", "isolate_number")
    ) |>
    dplyr::mutate(
      has_disagreement = dplyr::coalesce(mdro_disagree, FALSE) |
        dplyr::coalesce(organism_disagree, FALSE),
      has_ambiguity = dplyr::coalesce(.data$has_ambiguity, FALSE),
      # A label that matches only as an ordered subsequence is a suggestion,
      # not an identification. Such a link always goes to needs-review for an
      # analyst to confirm, however high the other signals push the score.
      has_weak_label = dplyr::coalesce(
        .data$label_match_kind == "subsequence", FALSE
      )
    )

  matched_keys <- disagreement |>
    dplyr::filter(score >= thresh_auto, !has_disagreement, !has_ambiguity,
                  !has_weak_label) |>
    dplyr::select(lab_id, isolate_number)

  review_keys <- disagreement |>
    dplyr::filter(
      score >= thresh_review,
      score < thresh_auto | has_disagreement | has_ambiguity | has_weak_label
    ) |>
    dplyr::select(lab_id, isolate_number)

  none_rows <- vitek_unique |>
    dplyr::anti_join(matched_keys, by = c("lab_id", "isolate_number")) |>
    dplyr::anti_join(review_keys,  by = c("lab_id", "isolate_number"))

  list(
    matched = best |> dplyr::semi_join(matched_keys, by = c("lab_id", "isolate_number")),
    review  = match_candidates |> dplyr::semi_join(review_keys, by = c("lab_id", "isolate_number")),
    none    = none_rows,
    confirmed = confirmed_keys
  )
}

#' Distinct (lab_id, isolate_number) keys that already carry a confirmed link.
#'
#' The identifiers are returned as written, so a caller can display them.
#' Comparison happens on the normalised form, in .isolate_key_cols().
.confirmed_isolate_keys <- function(links_confirmed) {
  empty <- tibble::tibble(lab_id = character(), isolate_number = character())
  if (is.null(links_confirmed) || nrow(links_confirmed) == 0) return(empty)
  if (length(setdiff(c("lab_id", "isolate_number"), names(links_confirmed))) > 0) {
    return(empty)
  }

  links_confirmed |>
    dplyr::select(lab_id, isolate_number) |>
    dplyr::filter(!is.na(lab_id), !is.na(isolate_number)) |>
    .isolate_key_cols() |>
    dplyr::distinct(.axis_lab_key, .axis_iso_key, .keep_all = TRUE) |>
    dplyr::select(-dplyr::starts_with(".axis_"))
}

#' Attach upper-cased, trimmed isolate keys, matching how links are written.
.isolate_key_cols <- function(df) {
  df |>
    dplyr::mutate(
      .axis_lab_key = toupper(trimws(as.character(.data$lab_id))),
      .axis_iso_key = toupper(trimws(as.character(.data$isolate_number)))
    )
}

#' Remove rows whose isolate key appears in `keys`.
.drop_confirmed_isolates <- function(df, keys) {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (is.null(keys) || nrow(keys) == 0) return(df)
  if (length(setdiff(c("lab_id", "isolate_number"), names(df))) > 0) return(df)

  df |>
    .isolate_key_cols() |>
    dplyr::anti_join(
      keys |> .isolate_key_cols() |> dplyr::select(.axis_lab_key, .axis_iso_key),
      by = c(".axis_lab_key", ".axis_iso_key")
    ) |>
    dplyr::select(-dplyr::starts_with(".axis_"))
}

#' Count unique Vitek isolate keys in a match-like table.
#'
#' Review buckets can contain multiple candidate OpenSpecimen rows per Vitek
#' isolate, so UI summaries should count isolate keys rather than candidate rows.
match_isolate_key_count <- function(x) {
  if (is.null(x) || nrow(x) == 0 || !"lab_id" %in% names(x)) return(0L)

  if (!"isolate_number" %in% names(x)) {
    return(as.integer(dplyr::n_distinct(x$lab_id)))
  }

  as.integer(
    x |>
      dplyr::distinct(lab_id, isolate_number) |>
      nrow()
  )
}

#' Count matched / review / none buckets using consistent isolate-level units.
match_bucket_counts <- function(buckets) {
  if (is.null(buckets)) {
    return(list(matched = 0L, review = 0L, none = 0L, confirmed = 0L))
  }

  list(
    matched = match_isolate_key_count(buckets$matched),
    review  = match_isolate_key_count(buckets$review),
    none    = match_isolate_key_count(buckets$none),
    confirmed = match_isolate_key_count(buckets$confirmed)
  )
}

# ── Project match summary ─────────────────────────────────────────────────────

#' Per-project match summary for the preview card.
#'
#' @param buckets   Result of bucket_results().
#' @param projects  tibble from scan_os_projects().
#' @return tibble: project_id, study_label, color, n_matched, n_review, n_none, pct_matched.
project_match_summary <- function(buckets, projects) {
  if (is.null(projects) || nrow(projects) == 0)
    return(tibble::tibble(
      project_id  = character(), study_label = character(), color = character(),
      n_matched   = integer(),   n_review    = integer(),   n_none = integer(),
      pct_matched = double()
    ))

  total_none <- if (!is.null(buckets$none)) {
    match_isolate_key_count(buckets$none)
  } else {
    0L
  }

  purrr::map_dfr(seq_len(nrow(projects)), function(i) {
    pid <- projects$project_id[i]

    n_m <- if (!is.null(buckets$matched) && nrow(buckets$matched) > 0)
      match_isolate_key_count(
        dplyr::filter(buckets$matched, project_id == pid)
      ) else 0L
    n_r <- if (!is.null(buckets$review) && nrow(buckets$review) > 0)
      match_isolate_key_count(
        dplyr::filter(buckets$review, project_id == pid)
      ) else 0L
    n_n <- total_none   # unmatched Vitek rows aren't project-attributed

    total <- n_m + n_r + n_n
    tibble::tibble(
      project_id  = pid,
      study_label = projects$study_label[i],
      color       = projects$color[i],
      n_matched   = as.integer(n_m),
      n_review    = as.integer(n_r),
      n_none      = as.integer(n_n),
      pct_matched = if (total > 0) round(n_m / total * 100) else 0
    )
  })
}

# ── Empty tibble ──────────────────────────────────────────────────────────────

#' Empty match_candidates tibble.
match_candidates_empty <- function() {
  tibble::tibble(
    lab_id         = character(),
    isolate_number = character(),
    os_identifier  = character(),
    project_id     = character(),
    specimen_label = character(),
    cp_short_title = character(),
    score          = integer(),
    label_match_kind = character(),
    label_score    = integer(),
    subject_score  = integer(),
    mdro_score     = integer(),
    organism_score = integer(),
    date_score     = integer(),
    cp_score       = integer(),
    cryo_score     = integer(),
    date_diff_days = double(),
    mdro_disagree  = logical(),
    organism_disagree = logical()
  )
}

# ── Utility ───────────────────────────────────────────────────────────────────

.ensure_col <- function(df, col, default) {
  if (!col %in% names(df)) df[[col]] <- rep(default, nrow(df))
  df
}

.prepare_match_specimens <- function(specimens) {
  specimens |>
    dplyr::mutate(
      .axis_specimen_label_norm = .norm_accession_label(.data$specimen_label),
      .axis_participant_norm = .norm_accession_label(.data$participant_id),
      .axis_mdro_norm = .norm_mdro(.data$custom_mdro),
      .axis_organism_norm = .norm_organism(.data$custom_organism),
      .axis_organism_upper = toupper(.data$.axis_organism_norm),
      .axis_organism_genus = .organism_genus(.data$.axis_organism_norm),
      .axis_type_trim = trimws(as.character(.data$type)),
      .axis_class_trim = trimws(as.character(.data$class)),
      .axis_cp_title_trim = trimws(as.character(.data$cp_short_title)),
      .axis_lineage_trim = trimws(as.character(.data$lineage)),
      .axis_is_review_aliquot = grepl("^aliquot$", .data$.axis_type_trim, ignore.case = TRUE) |
        (grepl("^aliquot$", .data$.axis_class_trim, ignore.case = TRUE) &
           .data$.axis_type_trim != "Cryopreserved Cells")
    ) |>
    .flag_duplicate_lineage_aliquots()
}

#' Flag cryopreserved aliquots that duplicate a specimen already in the set.
#'
#' A glycerol cryovial is stored as a separate OpenSpecimen record whose
#' lineage is "Aliquot" but whose class and type are Cell / Cryopreserved
#' Cells, so the type and class tests above never exclude it. Its display label
#' also has the glycerol suffix stripped, which makes it normalise to exactly
#' its parent's label. Both records then score identically and the analyst sees
#' the same specimen twice in needs-review.
#'
#' Only aliquots whose parent is present are flagged. Where the parent is
#' absent from the export the aliquot is the only record of that isolate, and
#' dropping it would hide a record the analyst has to be able to reach.
.flag_duplicate_lineage_aliquots <- function(specimens) {
  if (!".axis_lineage_trim" %in% names(specimens)) {
    specimens$.axis_lineage_trim <- NA_character_
  }
  is_aliquot_lineage <- grepl("^aliquot$", specimens$.axis_lineage_trim,
                              ignore.case = TRUE)
  label <- specimens$.axis_specimen_label_norm

  parent_labels <- unique(label[!is_aliquot_lineage & !is.na(label) & nzchar(label)])

  specimens |>
    dplyr::mutate(
      .axis_is_duplicate_aliquot = is_aliquot_lineage &
        !is.na(label) & nzchar(label) & label %in% parent_labels
    )
}

.score_one_vitek <- function(vrow, specimens) {
  # Restrict candidate specimens:
  # If cp_hint is set, prefer specimens from that CP; always include all as
  # fallback so we don't miss cross-CP matches at lower scores.
  cands <- specimens

  if (!is.na(vrow$cp_hint) && nchar(trimws(vrow$cp_hint)) > 0) {
    cp_match <- .cp_titles_overlap(vrow$cp_hint, cands$cp_short_title)
    if (any(cp_match, na.rm = TRUE)) cands <- cands[cp_match, ]
  }

  cryo_match <- cands$.axis_type_trim == "Cryopreserved Cells"
  if (any(cryo_match, na.rm = TRUE)) cands <- cands[cryo_match, ]

  if (nrow(cands) == 0) return(NULL)

  # ── Signal 1: Label match (0–60) ─────────────────────────────────────
  lid_match <- .norm_accession_label(vrow$lab_id)
  slabel_match <- cands$.axis_specimen_label_norm

  label_substring <- mapply(function(a, b) {
    if (is.na(a) || is.na(b) || nchar(a) < 6 || nchar(b) < 6) return(FALSE)
    grepl(a, b, fixed = TRUE) || grepl(b, a, fixed = TRUE)
  }, lid_match, slabel_match)

  label_subsequence <- mapply(function(a, b) {
    if (is.na(a) || is.na(b) || nchar(a) < 6 || nchar(b) < 6) return(FALSE)
    .is_ordered_subsequence(a, b) || .is_ordered_subsequence(b, a)
  }, lid_match, slabel_match)

  label_match_kind <- dplyr::case_when(
    is.na(slabel_match) | slabel_match == "" ~ "none",
    lid_match == slabel_match                ~ "exact",
    label_substring                          ~ "substring",
    label_subsequence                        ~ "subsequence",
    TRUE                                     ~ "none"
  )

  label_score <- dplyr::case_when(
    label_match_kind == "exact"       ~ 60L,
    label_match_kind == "substring"   ~ 45L,
    label_match_kind == "subsequence" ~ 30L,
    TRUE                              ~  0L
  )

  # ── Signal 2: Subject match (0–35) ───────────────────────────────────
  vsub_match <- .norm_accession_label(vrow$parsed_subject)
  spid_match <- cands$.axis_participant_norm

  subject_score <- dplyr::case_when(
    is.na(vsub_match) | vsub_match == "" | vsub_match == "NA" |
      is.na(spid_match) | spid_match == "" | spid_match == "NA"  ~  7L,
    vsub_match == spid_match                                 ~ 35L,
    nchar(vsub_match) >= 4 & nchar(spid_match) >= 4 &
      (startsWith(spid_match, vsub_match) | startsWith(vsub_match, spid_match)) ~ 20L,
    TRUE                                         ~  0L
  )

  # ── Signal 3: MDRO target match (0–15) ───────────────────────────────
  vtarget <- .norm_mdro(vrow$parsed_target)
  smdro <- cands$.axis_mdro_norm

  mdro_score <- dplyr::case_when(
    is.na(vtarget) | is.na(smdro) ~  5L,
    vtarget == smdro              ~ 15L,
    TRUE                          ~  0L
  )
  mdro_disagree <- !is.na(vtarget) & !is.na(smdro) & vtarget != smdro

  # ── Signal 4: Organism match (0–10) ──────────────────────────────────
  vorg <- .norm_organism(vrow$organism_name)
  vorg_upper <- toupper(vorg)
  vorg_genus <- .organism_genus(vorg)
  sorg <- cands$.axis_organism_norm

  organism_score <- dplyr::case_when(
    is.na(vorg) | is.na(sorg) ~ 3L,
    vorg_upper == cands$.axis_organism_upper ~ 10L,
    vorg_genus != "" & vorg_genus == cands$.axis_organism_genus ~ 5L,
    TRUE ~ 0L
  )
  organism_disagree <- !is.na(vorg) & !is.na(sorg) &
    vorg_upper != cands$.axis_organism_upper

  # ── Signal 5: Date proximity (0–10) ──────────────────────────────────
  vdate <- vrow$testing_date
  sdate <- cands$custom_collection_date
  date_diff <- suppressWarnings(abs(as.numeric(difftime(vdate, sdate, units = "days"))))
  date_score <- dplyr::case_when(
    is.na(date_diff)  ~  4L,
    date_diff == 0    ~ 10L,
    date_diff <= 1    ~  8L,
    date_diff <= 3    ~  5L,
    date_diff <= 7    ~  2L,
    TRUE              ~  0L
  )

  # ── Signal 6: CP hint match (0–5) ────────────────────────────────────
  cp_hint_val <- trimws(as.character(vrow$cp_hint))
  scp <- cands$.axis_cp_title_trim
  cp_overlap <- .cp_titles_overlap(cp_hint_val, scp)

  cp_score <- dplyr::case_when(
    is.na(cp_hint_val) | cp_hint_val == "" |
      is.na(scp)       | scp == ""         ~  2L,
    cp_overlap                              ~ 5L,
    TRUE                                    ~  0L
  )

  # ── Signal 7: Specimen type (0–10) ───────────────────────────────────
  cryo_score <- dplyr::if_else(
    cands$.axis_type_trim == "Cryopreserved Cells",
    10L,
    0L,
    missing = 0L
  )

  score <- pmin(100L, as.integer(label_score + subject_score +
                                   mdro_score + organism_score +
                                   date_score + cp_score + cryo_score))

  tibble::tibble(
    lab_id            = vrow$lab_id,
    isolate_number    = vrow$isolate_number,
    os_identifier     = cands$os_identifier,
    project_id        = cands$project_id,
    specimen_label    = cands$specimen_label,
    cp_short_title    = cands$cp_short_title,
    parsed_study      = .first_or_na(vrow$parsed_study),
    parsed_subject    = .first_or_na(vrow$parsed_subject),
    parsed_target     = .first_or_na(vrow$parsed_target),
    cp_hint           = .first_or_na(vrow$cp_hint),
    os_type           = cands$type,
    os_class          = cands$class,
    score             = score,
    label_match_kind  = label_match_kind,
    label_score       = label_score,
    subject_score     = subject_score,
    mdro_score        = mdro_score,
    organism_score    = organism_score,
    date_score        = date_score,
    cp_score          = cp_score,
    cryo_score        = cryo_score,
    date_diff_days    = date_diff,
    mdro_disagree     = mdro_disagree,
    organism_disagree = organism_disagree,
    match_explanation = sprintf(
      "study=%s; cp_hint=%s; OS_CP=%s; OS_type=%s; label=%s; subject=%s; mdro=%s; organism=%s; date_days=%s",
      .first_or_na(vrow$parsed_study),
      .first_or_na(vrow$cp_hint),
      cands$cp_short_title,
      cands$type,
      label_score,
      subject_score,
      mdro_score,
      organism_score,
      ifelse(is.na(date_diff), "NA", as.character(date_diff))
    )
  )
}

.first_or_na <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]])) return(NA_character_)
  as.character(x[[1]])
}

.norm_cp_title <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[is.na(x) | x %in% c("", "NA", "N/A")] <- NA_character_
  gsub("[^A-Z0-9]+", "", x)
}

#' Classify recognized collection protocols into stable internal families.
#'
#' SNT, Sentinel, APPS (including numbered rounds), and REACT are alternate
#' protocol titles for the same study family. Unrecognized titles intentionally
#' remain unclassified so a word such as "APPS" inside an unknown protocol does
#' not create a cross-cohort match.
.protocol_family <- function(x) {
  norm <- .norm_cp_title(x)
  family <- rep(NA_character_, length(norm))
  known_snt_apps_react <- !is.na(norm) & (
    norm %in% c(
      "SNT", "SENTINEL", "APPS", "REACT", "SNTAPPSREACT",
      "SENTINELREACT"
    ) |
      grepl("^APPS[0-9]+$", norm)
  )
  family[known_snt_apps_react] <- "SNT_APPS_REACT"
  family
}

.cp_titles_overlap <- function(hint, cp) {
  hint_norm <- .norm_cp_title(hint)
  cp_norm <- .norm_cp_title(cp)
  hint_family <- .protocol_family(hint)
  cp_family <- .protocol_family(cp)

  mapply(function(h, c, hf, cf) {
    if (is.na(h) || h == "" || is.na(c) || c == "") return(FALSE)
    if (!is.na(hf) || !is.na(cf)) {
      return(!is.na(hf) && !is.na(cf) && hf == cf)
    }
    grepl(h, c, fixed = TRUE) || grepl(c, h, fixed = TRUE)
  }, hint_norm, cp_norm, hint_family, cp_family)
}

.organism_genus <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  sub("\\s+.*$", "", x)
}

.norm_accession_label <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "na", "n/a")] <- NA_character_
  x <- toupper(x)
  # Accession labels use different separator conventions on each side:
  # ARRRRG writes ARG026_P2 where Vitek writes ARG026P2, and APPS writes
  # APPS0028_ig_dp_CRE#3of3 where Vitek writes APPS0028igCRE3of3. Removing
  # every separator - not just underscores - keeps display labels intact while
  # making linkage tolerant of all of them. No Vitek identifier in the loaded
  # data contains punctuation at all.
  x <- gsub("[^A-Z0-9]+", "", x)
  x
}

#' Is `a` an ordered subsequence of `b`?
#'
#' Accession labels sometimes differ by an inserted token rather than by
#' punctuation: OpenSpecimen writes APPS0028_ig_dp_CRE#3of3 where Vitek writes
#' APPS0028igCRE3of3, so neither normalised string contains the other and both
#' the exact and substring tests fail. Every character of the shorter label
#' still appears in the longer one, in order.
#'
#' This is a weaker signal than a substring match and is scored accordingly.
#' Matching alone never promotes a link to auto-matched; see bucket_results().
.is_ordered_subsequence <- function(a, b) {
  if (is.na(a) || is.na(b)) return(FALSE)
  if (!nzchar(a) || !nzchar(b)) return(FALSE)
  if (nchar(a) > nchar(b)) return(FALSE)

  av <- strsplit(a, "", fixed = TRUE)[[1]]
  bv <- strsplit(b, "", fixed = TRUE)[[1]]
  i <- 1L
  for (ch in bv) {
    if (i > length(av)) break
    if (identical(ch, av[[i]])) i <- i + 1L
  }
  i > length(av)
}
