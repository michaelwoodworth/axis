# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/data_parse_cfu.R — quantitative culture CFU parser
#
# Normalization rule order:
# 1. Strip the unit suffix and record cfu_unit: /gram of stool -> CFU/g stool,
#    CFU/mg -> CFU/mg, else default CFU/mL. Units are never converted.
# 2. Capture a leading operator (>, >=, ≥) as cfu_censored, then drop it.
# 3. Parse magnitude in x10^n, e-notation, or plain decimal notation.
# 4. Renormalize mantissas >= 10 and flag as "renormalized".
# 5. If the quantitative cell is blank but a growth flag reads Yes (DP) or
#    Yes (EB), assign cohort_floor_log10 as a pseudocount and preserve method.
# 6. Flag ambiguous bare small values, values below 1, and unparseable text.
# ─────────────────────────────────────────────────────────────────────────────

.cfu_blank <- function(x) {
  x <- trimws(as.character(x))
  is.na(x) | x %in% c("", "NA", "na", "N/A", "n/a", "NULL", "null")
}

.cfu_growth_method <- function(...) {
  vals <- unlist(list(...), use.names = FALSE)
  vals <- vals[!is.na(vals)]
  txt <- toupper(paste(vals, collapse = " "))
  dplyr::case_when(
    grepl("\\bDP\\b|DIRECT", txt, perl = TRUE) ~ "DP",
    grepl("\\bEB\\b|ENRICH", txt, perl = TRUE) ~ "EB",
    TRUE ~ NA_character_
  )
}

.cfu_growth_positive <- function(x) {
  x <- trimws(toupper(as.character(x)))
  !is.na(x) & grepl("^(YES|Y|POS|POSITIVE|GROWTH|1|TRUE)\\b", x, perl = TRUE)
}

.parse_cfu_one <- function(raw, growth_flag = NA, cohort_floor_log10 = 0) {
  raw_chr <- if (length(raw) == 0 || is.na(raw)) NA_character_ else as.character(raw)
  growth_chr <- if (length(growth_flag) == 0 || is.na(growth_flag)) NA_character_ else as.character(growth_flag)
  method <- .cfu_growth_method(raw_chr, growth_chr)

  out <- tibble::tibble(
    cfu_raw = raw_chr,
    cfu_log10 = NA_real_,
    cfu_value = NA_real_,
    cfu_unit = "CFU/mL",
    cfu_censored = FALSE,
    growth_method = method,
    is_pseudocount = FALSE,
    cfu_flag = NA_character_
  )

  if (.cfu_blank(raw_chr)) {
    if (.cfu_growth_positive(growth_chr) && !is.na(method)) {
      out$cfu_log10 <- cohort_floor_log10
      out$cfu_value <- 10^cohort_floor_log10
      out$is_pseudocount <- TRUE
    }
    return(out)
  }

  txt <- trimws(raw_chr)

  if (grepl("/\\s*gram\\s+of\\s+stool|CFU\\s*/\\s*g(?:\\b|ram)|/\\s*g(?:\\b|ram)", txt,
            ignore.case = TRUE, perl = TRUE)) {
    out$cfu_unit <- "CFU/g stool"
  } else if (grepl("CFU\\s*/\\s*mg|/\\s*mg\\b", txt, ignore.case = TRUE, perl = TRUE)) {
    out$cfu_unit <- "CFU/mg"
  }

  txt <- gsub("/\\s*gram\\s+of\\s+stool", "", txt, ignore.case = TRUE, perl = TRUE)
  txt <- gsub("CFU\\s*/\\s*(?:mL|ml|g|gram|mg)", "", txt, ignore.case = TRUE, perl = TRUE)
  txt <- gsub("/\\s*(?:mL|ml|g|gram|mg)\\b", "", txt, ignore.case = TRUE, perl = TRUE)
  txt <- gsub("\\bCFU\\b", "", txt, ignore.case = TRUE, perl = TRUE)
  txt <- trimws(txt)

  out$cfu_censored <- grepl("^\\s*(?:>|>=|≥)", txt, perl = TRUE)
  txt <- trimws(sub("^\\s*(?:>|>=|≥)\\s*", "", txt, perl = TRUE))

  sci_match <- regexec(
    "([0-9]+(?:\\.[0-9]+)?)\\s*[xX×]\\s*10\\s*(?:\\^|\\*\\*)?\\s*([+-]?[0-9]+)",
    txt,
    perl = TRUE
  )
  sci_parts <- regmatches(txt, sci_match)[[1]]

  value <- NA_real_
  renormalized <- FALSE
  if (length(sci_parts) == 3) {
    mantissa <- suppressWarnings(as.numeric(sci_parts[[2]]))
    exponent <- suppressWarnings(as.numeric(sci_parts[[3]]))
    if (!is.na(mantissa) && !is.na(exponent)) {
      if (mantissa >= 10) {
        exponent <- exponent + floor(log10(mantissa))
        mantissa <- mantissa / (10^floor(log10(mantissa)))
        renormalized <- TRUE
      }
      value <- mantissa * (10^exponent)
    }
  } else {
    numeric_txt <- regmatches(
      txt,
      regexpr("[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?", txt, perl = TRUE)
    )
    if (length(numeric_txt) == 1 && nzchar(numeric_txt)) {
      value <- suppressWarnings(as.numeric(numeric_txt))
    }
  }

  if (is.na(value)) {
    if (.cfu_growth_positive(raw_chr) && !is.na(method)) {
      out$cfu_log10 <- cohort_floor_log10
      out$cfu_value <- 10^cohort_floor_log10
      out$is_pseudocount <- TRUE
      return(out)
    }
    out$cfu_flag <- "unparseable"
    return(out)
  }

  out$cfu_value <- value
  out$cfu_log10 <- if (value > 0) log10(value) else NA_real_

  if (renormalized) {
    out$cfu_flag <- "renormalized"
  } else if (value < 1) {
    out$cfu_flag <- "below_floor"
  } else if (!grepl("[xX×]|[eE][+-]?[0-9]+", txt, perl = TRUE) && value < 100) {
    out$cfu_flag <- "ambiguous_bare"
  }

  out
}

#' Parse free-text CFU/mL culture values into normalized numeric fields.
#'
#' @param raw Character vector of raw quantitative culture values.
#' @param growth_flag Character vector of direct-plate/enrichment growth flags.
#' @param cohort_floor_log10 Numeric floor used for growth-flag pseudocounts.
#' @return Tibble with cfu_raw, cfu_log10, cfu_value, cfu_unit, cfu_censored,
#'   growth_method, is_pseudocount, and cfu_flag.
parse_cfu <- function(raw, growth_flag = NA, cohort_floor_log10 = 0) {
  n <- max(length(raw), length(growth_flag), length(cohort_floor_log10))
  raw <- rep(raw, length.out = n)
  growth_flag <- rep(growth_flag, length.out = n)
  cohort_floor_log10 <- rep(cohort_floor_log10, length.out = n)

  purrr::pmap_dfr(
    list(raw = raw, growth_flag = growth_flag, cohort_floor_log10 = cohort_floor_log10),
    .parse_cfu_one
  )
}
