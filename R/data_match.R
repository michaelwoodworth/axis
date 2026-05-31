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
#' @return match_candidates tibble (all candidate pairs ≥ thresh_review).
auto_match <- function(vitek_unique, specimens,
                       thresh_auto   = 80,
                       thresh_review = 50,
                       parallel = getOption("axis.match_parallel", TRUE),
                       workers = getOption("axis.match_workers", NULL)) {

  if (is.null(vitek_unique) || nrow(vitek_unique) == 0 ||
      is.null(specimens)    || nrow(specimens)    == 0)
    return(match_candidates_empty())

  # Ensure required columns exist with sensible defaults
  vitek_unique <- .ensure_col(vitek_unique, "cp_hint",        NA_character_)
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
  specimens <- .prepare_match_specimens(specimens)

  # Score all pairs using vectorized per-isolate scoring. Specimen-side
  # normalized fields are precomputed once above; large jobs can split isolate
  # scoring across local cores on Unix-like systems.
  idx <- seq_len(nrow(vitek_unique))
  use_parallel <- isTRUE(parallel) &&
    .Platform$OS.type != "windows" &&
    length(idx) > 1L
  if (is.null(workers)) {
    workers <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
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

#' Bucket match_candidates into matched / review / none.
#'
#' @param match_candidates  tibble from auto_match().
#' @param vitek_unique      All deduplicated Vitek rows.
#' @param thresh_auto       Score ≥ this → auto-matched (default 80).
#' @param thresh_review     Score ≥ this → needs-review (default 50).
#' @return Named list: $matched, $review, $none.
bucket_results <- function(match_candidates, vitek_unique,
                            thresh_auto = 80, thresh_review = 50) {
  empty <- list(
    matched = match_candidates_empty(),
    review  = match_candidates_empty(),
    none    = vitek_unique
  )

  if (is.null(match_candidates) || nrow(match_candidates) == 0)
    return(empty)

  # Best score per (lab_id, isolate_number) pair
  best <- match_candidates |>
    dplyr::group_by(lab_id, isolate_number) |>
    dplyr::slice_max(score, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup()

  disagreement <- best |>
    dplyr::mutate(
      has_disagreement = dplyr::coalesce(mdro_disagree, FALSE) |
        dplyr::coalesce(organism_disagree, FALSE)
    )

  matched_keys <- disagreement |>
    dplyr::filter(score >= thresh_auto, !has_disagreement) |>
    dplyr::select(lab_id, isolate_number)

  review_keys <- disagreement |>
    dplyr::filter(
      score >= thresh_review,
      score < thresh_auto | has_disagreement
    ) |>
    dplyr::select(lab_id, isolate_number)

  none_rows <- vitek_unique |>
    dplyr::anti_join(matched_keys, by = c("lab_id", "isolate_number")) |>
    dplyr::anti_join(review_keys,  by = c("lab_id", "isolate_number"))

  list(
    matched = best |> dplyr::semi_join(matched_keys, by = c("lab_id", "isolate_number")),
    review  = match_candidates |> dplyr::semi_join(review_keys, by = c("lab_id", "isolate_number")),
    none    = none_rows
  )
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
    return(list(matched = 0L, review = 0L, none = 0L))
  }

  list(
    matched = match_isolate_key_count(buckets$matched),
    review  = match_isolate_key_count(buckets$review),
    none    = match_isolate_key_count(buckets$none)
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
      .axis_cp_title_trim = trimws(as.character(.data$cp_short_title))
    )
}

.score_one_vitek <- function(vrow, specimens) {
  # Restrict candidate specimens:
  # If cp_hint is set, prefer specimens from that CP; always include all as
  # fallback so we don't miss cross-CP matches at lower scores.
  cands <- specimens

  if (!is.na(vrow$cp_hint) && nchar(trimws(vrow$cp_hint)) > 0) {
    cp_match <- grepl(vrow$cp_hint, cands$cp_short_title,
                      ignore.case = TRUE, fixed = FALSE)
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

  label_score <- dplyr::case_when(
    is.na(slabel_match) | slabel_match == "" ~  0L,
    lid_match == slabel_match                ~ 60L,
    label_substring                          ~ 45L,
    TRUE                                     ~  0L
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
  cp_overlap <- mapply(function(hint, cp) {
    if (is.na(hint) || hint == "" || is.na(cp) || cp == "") return(FALSE)
    grepl(hint, cp, ignore.case = TRUE) || grepl(cp, hint, ignore.case = TRUE)
  }, cp_hint_val, scp)

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
    score             = score,
    label_score       = label_score,
    subject_score     = subject_score,
    mdro_score        = mdro_score,
    organism_score    = organism_score,
    date_score        = date_score,
    cp_score          = cp_score,
    cryo_score        = cryo_score,
    date_diff_days    = date_diff,
    mdro_disagree     = mdro_disagree,
    organism_disagree = organism_disagree
  )
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
  # ARRRRG labels sometimes have a separator after the subject id
  # (ARG026_P2) while Vitek may omit it (ARG026P2). Removing underscores
  # keeps display labels intact but makes linkage tolerant to that convention.
  x <- gsub("_+", "", x)
  x
}
