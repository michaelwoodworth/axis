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
                       thresh_review = 50) {

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

  # Score all pairs using vectorized cross-join
  results <- purrr::map_dfr(seq_len(nrow(vitek_unique)), function(vi) {
    vrow <- vitek_unique[vi, ]

    # Restrict candidate specimens:
    # If cp_hint is set, prefer specimens from that CP; always include all
    # as fallback so we don't miss cross-CP matches at lower scores.
    cands <- specimens

    if (!is.na(vrow$cp_hint) && nchar(trimws(vrow$cp_hint)) > 0) {
      cp_match <- grepl(vrow$cp_hint, cands$cp_short_title,
                        ignore.case = TRUE, fixed = FALSE)
      # If any CP-hint matches exist, restrict to those; otherwise keep all
      if (any(cp_match, na.rm = TRUE)) cands <- cands[cp_match, ]
    }

    cryo_match <- trimws(as.character(cands$type)) == "Cryopreserved Cells"
    if (any(cryo_match, na.rm = TRUE)) cands <- cands[cryo_match, ]

    if (nrow(cands) == 0) return(NULL)

    # ── Signal 1: Label match (0–60) ─────────────────────────────────────
    lid    <- trimws(as.character(vrow$lab_id))
    slabel <- trimws(as.character(cands$specimen_label))
    lid_match    <- .norm_accession_label(lid)
    slabel_match <- .norm_accession_label(slabel)

    label_substring <- mapply(function(a, b) {
      if (is.na(a) || is.na(b) || nchar(a) < 6 || nchar(b) < 6) return(FALSE)
      grepl(a, b, fixed = TRUE) || grepl(b, a, fixed = TRUE)
    }, lid_match, slabel_match)

    label_score <- dplyr::case_when(
      is.na(slabel_match) | slabel_match == "" ~  0L,
      lid_match == slabel_match                ~ 60L,
      # lab_id is a substring of specimen_label (e.g., "6180011" in "6180011ESBL1")
      label_substring                          ~ 45L,
      TRUE                                     ~  0L
    )

    # ── Signal 2: Subject match (0–35) ───────────────────────────────────
    vsub  <- trimws(as.character(vrow$parsed_subject))
    spid  <- trimws(as.character(cands$participant_id))
    vsub_match <- .norm_accession_label(vsub)
    spid_match <- .norm_accession_label(spid)

    subject_score <- dplyr::case_when(
      is.na(vsub_match) | vsub_match == "" | vsub_match == "NA" |
        is.na(spid_match) | spid_match == "" | spid_match == "NA"  ~  7L,   # unknown → neutral
      vsub_match == spid_match                                 ~ 35L,
      # Prefix match (≥ 4 chars): e.g., "ARG026" vs "ARG026ESBL"
      nchar(vsub_match) >= 4 & nchar(spid_match) >= 4 &
        (startsWith(spid_match, vsub_match) | startsWith(vsub_match, spid_match)) ~ 20L,
      TRUE                                         ~  0L
    )

    # ── Signal 3: MDRO target match (0–15) ───────────────────────────────
    vtarget <- .norm_mdro(vrow$parsed_target)
    smdro   <- .norm_mdro(cands$custom_mdro)

    mdro_score <- dplyr::case_when(
      is.na(vtarget) | is.na(smdro) ~  5L,   # unknown → neutral
      vtarget == smdro              ~ 15L,
      TRUE                          ~  0L
    )

    # MDRO disagreement flag
    mdro_disagree <- !is.na(vtarget) & !is.na(smdro) & vtarget != smdro

    # ── Signal 4: Organism match (0–10) ──────────────────────────────────
    vorg <- .norm_organism(vrow$organism_name)
    sorg <- .norm_organism(cands$custom_organism)

    organism_score <- dplyr::case_when(
      is.na(vorg) | is.na(sorg) ~ 3L,
      toupper(vorg) == toupper(sorg) ~ 10L,
      .organism_genus(vorg) != "" & .organism_genus(vorg) == .organism_genus(sorg) ~ 5L,
      TRUE ~ 0L
    )

    organism_disagree <- !is.na(vorg) & !is.na(sorg) &
      toupper(vorg) != toupper(sorg)

    # ── Signal 5: Date proximity (0–10) ──────────────────────────────────
    vdate <- vrow$testing_date
    sdate <- cands$custom_collection_date

    date_diff <- suppressWarnings(abs(as.numeric(difftime(vdate, sdate, units = "days"))))
    date_score <- dplyr::case_when(
      is.na(date_diff)  ~  4L,    # unknown → neutral
      date_diff == 0    ~ 10L,
      date_diff <= 1    ~  8L,
      date_diff <= 3    ~  5L,
      date_diff <= 7    ~  2L,
      TRUE              ~  0L
    )

    # ── Signal 6: CP hint match (0–5) ────────────────────────────────────
    cp_hint_val <- trimws(as.character(vrow$cp_hint))
    scp         <- trimws(as.character(cands$cp_short_title))

    cp_overlap <- mapply(function(hint, cp) {
      if (is.na(hint) || hint == "" || is.na(cp) || cp == "") return(FALSE)
      grepl(hint, cp, ignore.case = TRUE) || grepl(cp, hint, ignore.case = TRUE)
    }, cp_hint_val, scp)

    cp_score <- dplyr::case_when(
      is.na(cp_hint_val) | cp_hint_val == "" |
        is.na(scp)       | scp == ""         ~  2L,   # unknown → neutral
      cp_overlap                              ~ 5L,
      TRUE                                    ~  0L
    )

    # ── Signal 7: Specimen type (0–10) ───────────────────────────────────
    cryo_score <- dplyr::if_else(
      trimws(as.character(cands$type)) == "Cryopreserved Cells",
      10L,
      0L,
      missing = 0L
    )

    # ── Composite score ───────────────────────────────────────────────────
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
  })

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

  total_none <- if (!is.null(buckets$none)) nrow(buckets$none) else 0L

  purrr::map_dfr(seq_len(nrow(projects)), function(i) {
    pid <- projects$project_id[i]

    n_m <- if (!is.null(buckets$matched) && nrow(buckets$matched) > 0)
      sum(buckets$matched$project_id == pid, na.rm = TRUE) else 0L
    n_r <- if (!is.null(buckets$review) && nrow(buckets$review) > 0)
      dplyr::n_distinct(
        buckets$review$lab_id[buckets$review$project_id == pid]
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
