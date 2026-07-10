# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/data_parse_labid.R — Per-study Lab ID regex table
#
# PURPOSE
#   Converts a raw Vitek2 "Lab ID" string into structured fields:
#     parsed_study, parsed_subject, parsed_target, cp_hint
#
# EXTENDING
#   Analysts can add rules without touching this file:
#     options(axis.labid_rules_extra = list(
#       list(
#         pattern    = "^MYID(\\d+)",
#         study      = "MY_STUDY",
#         cp_hint    = "My CP Title",
#         subject_fn = function(m) paste0("MYID", m[, 2]),
#         target_fn  = NULL          # derive target separately via extract_mdro_target()
#       )
#     ))
# ─────────────────────────────────────────────────────────────────────────────

# ── MDRO token extraction ─────────────────────────────────────────────────────

#' Known MDRO target tokens (order matters — more specific first).
.MDRO_TOKENS <- c(
  "ESBL", "CRAB", "CRPA", "CRKP", "CREC", "MRSA",
  "CRE",  "VRE",  "MDRP",
  "ClinIsolate", "Clin"
)

#' Extract the MDRO / organism target from a Lab ID string.
#'
#' Looks for known token substrings (case-insensitive). Returns the first
#' match, or NA_character_ if none found.
#'
#' @param lid  Character vector of Lab IDs.
#' @return Character vector (same length); NA where no token matched.
extract_mdro_target <- function(lid) {
  lid <- as.character(lid)
  result <- rep(NA_character_, length(lid))
  for (tok in .MDRO_TOKENS) {
    hits <- is.na(result) & grepl(tok, lid, ignore.case = TRUE)
    result[hits] <- tok
  }
  result
}

# ── Default rule table ────────────────────────────────────────────────────────
# Each rule is a list with:
#   pattern    — PCRE regex applied to the raw lab_id (anchored at start ^)
#   study      — canonical study code
#   cp_hint    — OpenSpecimen Collection Protocol short title (or NA)
#   subject_fn — function(m) returning parsed_subject; m = regmatches capture matrix
#   target_fn  — function(m, lid) returning parsed_target, or NULL to use
#                extract_mdro_target(lid) as fallback

.DEFAULT_LABID_RULES <- list(

  # ── ARG / ARRRRG family ─────────────────────────────────────────────────────
  # Format: ARG###[<target>][<suffix>]
  # Examples: ARG026ESBL1, ARG038CRE2, ARG007
  list(
    pattern    = "^(ARG)(\\d{3})",
    study      = "ARRRRG",
    cp_hint    = "ARRRRG 2.0",
    subject_fn = function(m) paste0("ARG", m[, 3]),
    target_fn  = NULL   # extract_mdro_target() applied to full lab_id
  ),

  # ── FAIR 618 family ─────────────────────────────────────────────────────────
  # Format: 618####[<target>][<isolate>]
  # The lab_id IS the specimen_label in OpenSpecimen; participant_id = FR01…
  # Examples: 6180011, 6180012ESBL1, 6180013CRE
  list(
    pattern    = "^(618)(\\d{4})",
    study      = "FAIR618",
    cp_hint    = "FAIR 618",
    subject_fn = function(m) paste0("618", m[, 3]),
    target_fn  = NULL
  ),

  # ── Pre-Alert family ────────────────────────────────────────────────────────
  # Format: 161####[<target>][<isolate>]
  # These older labels can look like FAIR 161 identifiers, but they belong to
  # Pre-Alert and should not be constrained to the FAIR 618 CP.
  list(
    pattern    = "^(?:PRE[-_ ]?ALERT[-_ ]*)?(161)(\\d{4})",
    study      = "PRE_ALERT",
    cp_hint    = "Pre-Alert",
    subject_fn = function(m) paste0("161", m[, 3]),
    target_fn  = NULL
  ),

  # ── aAG / bAG family ────────────────────────────────────────────────────────
  # Format: [ab]AG###[suffix]
  # Examples: aAG001, bAG014ESBL
  list(
    pattern    = "^([abAB]AG)(\\d{3})",
    study      = "aAG",
    cp_hint    = NA_character_,
    subject_fn = function(m) paste0(m[, 2], m[, 3]),
    target_fn  = NULL
  ),

  # ── aEM / bEM family ────────────────────────────────────────────────────────
  # Format: [ab]EM###[suffix]
  list(
    pattern    = "^([abAB]EM)(\\d{3})",
    study      = "aEM",
    cp_hint    = NA_character_,
    subject_fn = function(m) paste0(m[, 2], m[, 3]),
    target_fn  = NULL
  ),

  # ── MEPSD family ───────────────────────────────────────────────────────────
  # MEPSD records may not have an OpenSpecimen parent. Classifying the cohort
  # still preserves them in no-match/isolate exports without fabricating links.
  list(
    pattern    = "^(MEPSD[[:alnum:]_-]+)",
    study      = "MEPSD",
    cp_hint    = "MEPSD",
    subject_fn = function(m) m[, 2],
    target_fn  = NULL
  ),

  # ── SNT / APPS / React / MW family ──────────────────────────────────────────
  # Patterns observed: MW##-######, APPS####, REACT####
  # subject = the full matched prefix before any MDRO token
  list(
    pattern    = "^(MW\\d{2}-\\d{6})",
    study      = "SNT",
    cp_hint    = "SNT/APPS/React",
    subject_fn = function(m) m[, 2],
    target_fn  = NULL
  ),
  list(
    pattern    = "^(APPS\\d{4})",
    study      = "SNT",
    cp_hint    = "SNT/APPS/React",
    subject_fn = function(m) m[, 2],
    target_fn  = NULL
  ),
  list(
    pattern    = "^(REACT\\d{4})",
    study      = "SNT",
    cp_hint    = "SNT/APPS/React",
    subject_fn = function(m) m[, 2],
    target_fn  = NULL
  )
)

# ── Public API ────────────────────────────────────────────────────────────────

#' Return the active rule list (default + any analyst-supplied extras).
#'
#' @return list of rule objects.
get_labid_rules <- function() {
  extras <- getOption("axis.labid_rules_extra", default = list())
  c(.DEFAULT_LABID_RULES, extras)
}

#' Parse a vector of Vitek2 Lab IDs into structured fields.
#'
#' Applies rules in order; first match wins. Unrecognised IDs receive
#' study = "unknown" and parsed_subject = the original lab_id.
#'
#' @param lab_ids  Character vector.
#' @return tibble with columns:
#'   lab_id, parsed_study, parsed_subject, parsed_target, cp_hint
parse_lab_ids <- function(lab_ids) {
  lab_ids <- as.character(lab_ids)
  n       <- length(lab_ids)

  out <- tibble::tibble(
    lab_id         = lab_ids,
    parsed_study   = rep(NA_character_, n),
    parsed_subject = rep(NA_character_, n),
    parsed_target  = rep(NA_character_, n),
    cp_hint        = rep(NA_character_, n)
  )

  rules <- get_labid_rules()

  for (rule in rules) {
    unmatched <- which(is.na(out$parsed_study))
    if (length(unmatched) == 0L) break

    m <- regmatches(
      lab_ids[unmatched],
      regexpr(rule$pattern, lab_ids[unmatched], perl = TRUE, ignore.case = TRUE)
    )
    # regexpr gives one match per element; use regexec for capture groups
    me <- regexec(rule$pattern, lab_ids[unmatched], perl = TRUE, ignore.case = TRUE)
    mat <- regmatches(lab_ids[unmatched], me)

    hit_idx <- which(lengths(mat) > 0)
    if (length(hit_idx) == 0L) next

    # Build capture matrix (row = match, col = capture group)
    cap_mat <- do.call(rbind, lapply(mat[hit_idx], function(x) x))

    global_idx <- unmatched[hit_idx]

    out$parsed_study[global_idx]   <- rule$study
    out$cp_hint[global_idx]        <- rule$cp_hint
    out$parsed_subject[global_idx] <- rule$subject_fn(cap_mat)

    if (!is.null(rule$target_fn)) {
      out$parsed_target[global_idx] <- rule$target_fn(cap_mat, lab_ids[global_idx])
    }
  }

  # Fill remaining unmatched rows
  still_unmatched <- which(is.na(out$parsed_study))
  if (length(still_unmatched) > 0L) {
    out$parsed_study[still_unmatched]   <- "unknown"
    out$parsed_subject[still_unmatched] <- lab_ids[still_unmatched]
  }

  # MDRO target: apply extract_mdro_target() where target_fn was NULL / not set
  no_target <- which(is.na(out$parsed_target))
  if (length(no_target) > 0L) {
    out$parsed_target[no_target] <- extract_mdro_target(lab_ids[no_target])
  }

  out
}
