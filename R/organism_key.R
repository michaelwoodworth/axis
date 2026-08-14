# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/organism_key.R — curated VITEK2 → organism name key
#
# VITEK2 reports abbreviated organism names ("Esch.coli", "Psdes.vulneris")
# whose spelling changes between card versions. In the loaded batch the same
# organism code, EEE, appears as both "Ent.aerogenes" and "K.aerogenes", so the
# raw name splits one organism across two strings in every count.
#
# This module maps the stable VITEK2 organism *code* to a reviewed, current
# organism name, plus its genus, species, subspecies, and rank. The key lives in
# inst/extdata/vitek_organism_key.csv so it can be reviewed and extended without
# touching R code.
#
# Names follow LPSN (https://lpsn.dsmz.de) and its List of Recommended Names for
# bacteria of medical importance. Where LPSN flags a newer combination as "not
# recommended for medical use" — Mammaliicoccus lentus, Ectopseudomonas
# mendocina — the key keeps the recommended name instead.
#
# CRITICAL: this never modifies vitek_raw. The original VITEK name stays in
# v_organism in the cleaned output, and an analyst override always wins over the
# key. See build_cleaned() in R/data_clean.R.
# ─────────────────────────────────────────────────────────────────────────────

ORGANISM_KEY_FILE <- "vitek_organism_key.csv"

#' Locate the organism key CSV.
#'
#' Override with options(axis.organism_key_path = "/path/to/key.csv").
organism_key_path <- function() {
  configured <- getOption("axis.organism_key_path", default = NULL)
  if (!is.null(configured) && nzchar(configured)) return(configured)

  candidates <- file.path(
    c(".", "..", file.path("..", ".."), file.path("..", "..", "..")),
    "inst", "extdata", ORGANISM_KEY_FILE
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) return(NA_character_)
  normalizePath(hit[[1]], mustWork = FALSE)
}

.organism_key_cache <- new.env(parent = emptyenv())

#' Empty organism key with the canonical columns.
organism_key_empty <- function() {
  tibble::tibble(
    organism_code             = character(),
    vitek_name_example        = character(),
    clean_organism            = character(),
    clean_organism_genus      = character(),
    clean_organism_species    = character(),
    clean_organism_subspecies = character(),
    clean_organism_rank       = character(),
    note                      = character()
  )
}

#' Read the curated organism key, cached on path and modification time.
#'
#' @param path Character. Defaults to organism_key_path().
#' @return tibble with one row per VITEK2 organism code.
load_organism_key <- function(path = organism_key_path()) {
  if (is.na(path) || !file.exists(path)) {
    warning("Organism key not found; cleaned organism names will fall back to the raw Vitek names.")
    return(organism_key_empty())
  }

  stamp <- paste0(path, "|", as.numeric(file.info(path)$mtime))
  cached <- .organism_key_cache[[stamp]]
  if (!is.null(cached)) return(cached)

  key <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )

  required <- c("organism_code", "clean_organism", "clean_organism_genus",
                "clean_organism_species", "clean_organism_subspecies",
                "clean_organism_rank")
  missing <- setdiff(required, names(key))
  if (length(missing) > 0) {
    stop("Organism key is missing required columns: ", paste(missing, collapse = ", "))
  }

  key <- key |>
    dplyr::mutate(
      organism_code = .norm_organism_code(.data$organism_code),
      dplyr::across(dplyr::everything(), ~ dplyr::na_if(trimws(.x), ""))
    ) |>
    dplyr::filter(!is.na(.data$organism_code))

  duplicated_codes <- key$organism_code[duplicated(key$organism_code)]
  if (length(duplicated_codes) > 0) {
    stop("Organism key has duplicate codes: ",
         paste(unique(duplicated_codes), collapse = ", "))
  }

  .organism_key_cache[[stamp]] <- key
  key
}

#' Drop the cached key. Used by tests and after editing the CSV.
clear_organism_key_cache <- function() {
  rm(list = ls(.organism_key_cache), envir = .organism_key_cache)
  invisible(NULL)
}

.norm_organism_code <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[is.na(x) | x %in% c("", "NA", "N/A")] <- NA_character_
  x
}

#' Resolve VITEK2 organism codes to curated organism names.
#'
#' Rank values:
#'   species | subspecies | complex | group | genus  — from the key
#'   unidentified — VITEK made no identification; clean_organism is NA
#'   unmapped     — the code is not in the key; the raw Vitek name is kept
#'   NA           — no Vitek organism at all for this row
#'
#' A code the key does not know is never silently blanked: the raw VITEK name
#' carries through and the row is marked "unmapped" so it can be found and the
#' key extended.
#'
#' @param organism_code Character vector of VITEK2 organism codes.
#' @param organism_name Character vector of raw VITEK2 organism names, used as
#'   the fallback for unmapped codes.
#' @param key Optional pre-loaded key.
#' @return tibble with clean_organism, clean_organism_genus,
#'   clean_organism_species, clean_organism_subspecies, clean_organism_rank.
resolve_organism_names <- function(organism_code, organism_name = NULL,
                                   key = load_organism_key()) {
  n <- length(organism_code)
  if (is.null(organism_name)) organism_name <- rep(NA_character_, n)
  if (length(organism_name) != n) {
    stop("organism_code and organism_name must be the same length.")
  }

  code <- .norm_organism_code(organism_code)
  raw <- dplyr::na_if(trimws(as.character(organism_name)), "")

  out <- tibble::tibble(
    clean_organism            = rep(NA_character_, n),
    clean_organism_genus      = rep(NA_character_, n),
    clean_organism_species    = rep(NA_character_, n),
    clean_organism_subspecies = rep(NA_character_, n),
    clean_organism_rank       = rep(NA_character_, n)
  )

  idx <- match(code, key$organism_code)
  known <- !is.na(idx)

  if (any(known)) {
    out$clean_organism[known]            <- key$clean_organism[idx[known]]
    out$clean_organism_genus[known]      <- key$clean_organism_genus[idx[known]]
    out$clean_organism_species[known]    <- key$clean_organism_species[idx[known]]
    out$clean_organism_subspecies[known] <- key$clean_organism_subspecies[idx[known]]
    out$clean_organism_rank[known]       <- key$clean_organism_rank[idx[known]]
  }

  # A Vitek organism the key does not recognise keeps its raw name and is
  # flagged, so an unreviewed code is visible rather than lost.
  unmapped <- !known & (!is.na(code) | !is.na(raw))
  if (any(unmapped)) {
    out$clean_organism[unmapped] <- raw[unmapped]
    out$clean_organism_rank[unmapped] <- "unmapped"
  }

  out
}

#' Which loaded Vitek organism codes are missing from the key?
#'
#' @param vitek Tibble with organism_code and organism_name (vitek_raw or
#'   vitek_unique).
#' @param key Optional pre-loaded key.
#' @return tibble of code, name, and row count, one row per unmapped code.
unmapped_organism_codes <- function(vitek, key = load_organism_key()) {
  empty <- tibble::tibble(organism_code = character(),
                          organism_name = character(), n = integer())
  if (is.null(vitek) || !is.data.frame(vitek) || nrow(vitek) == 0) return(empty)
  if (!"organism_code" %in% names(vitek)) return(empty)

  name_col <- if ("organism_name" %in% names(vitek)) vitek$organism_name else NA_character_

  tibble::tibble(
    organism_code = .norm_organism_code(vitek$organism_code),
    organism_name = as.character(name_col)
  ) |>
    dplyr::filter(!is.na(.data$organism_code)) |>
    dplyr::filter(!.data$organism_code %in% key$organism_code) |>
    dplyr::count(.data$organism_code, .data$organism_name, name = "n") |>
    dplyr::arrange(dplyr::desc(.data$n))
}
