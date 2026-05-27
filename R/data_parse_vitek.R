# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/data_parse_vitek.R — Vitek2 XLSX parser
#
# Key design decisions
#   • Input format : Excel (.xlsx), read via readxl — Vitek2 native export
#   • Drug panel   : detected by header triplets {CODE}-{Name} /
#                    {CODE}-Other-Instrument / {CODE}-Other-Expertized
#                    starting at first such triplet column (NOT by position)
#   • PHI          : Patient Name column stripped immediately on read
#   • Lab ID parse : delegated to parse_lab_ids() from data_parse_labid.R
#
# Output schemas
#   vitek_raw  (wide, one row per isolate):
#     lab_id, isolate_number, patient_id, specimen_type, specimen_source,
#     collection_date (Date), testing_date (Date), organism_name, organism_code,
#     bio_number, pct_probability, id_confidence, selected_bp_site,
#     parsed_study, parsed_subject, parsed_target, cp_hint,
#     n_drugs (int), source_file, source_row, ingested_at, file_name
#
#   vitek_ast  (long, one row per drug × isolate):
#     lab_id, isolate_number, drug_code, drug_name,
#     mic, call_instr, call_expert,
#     result_mic, result_instrument, result_expertized
# ─────────────────────────────────────────────────────────────────────────────

# ── Date helpers ──────────────────────────────────────────────────────────────

#' Parse a mixed date field: Excel serial OR common date string formats.
.parse_date_field <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "na", "N/A", "n/a")] <- NA_character_
  result <- as.Date(rep(NA_character_, length(x)))

  # Excel serial numbers (exactly 5 digits)
  serial_mask <- !is.na(x) & grepl("^\\d{5}$", x)
  if (any(serial_mask)) {
    result[serial_mask] <- as.Date(
      as.integer(x[serial_mask]), origin = "1899-12-30"
    )
  }

  # Standard date strings
  remaining <- !is.na(x) & is.na(result)
  if (any(remaining)) {
    result[remaining] <- suppressWarnings(
      as.Date(x[remaining], tryFormats = c(
        "%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y",
        "%d/%m/%Y", "%d-%m-%Y", "%Y/%m/%d"
      ))
    )
  }
  result
}

# ── Drug triplet detection ────────────────────────────────────────────────────

#' Find the 1-based column index where drug triplets begin.
#' A drug column header matches: CODE-DrugName  (CODE = 2-6 uppercase alnum,
#' DrugName does NOT start with "Other").
.find_drug_start <- function(col_names) {
  drug_re <- "^[A-Z0-9]{2,6}-(?!Other)[A-Za-z]"
  hits <- grep(drug_re, col_names, perl = TRUE)
  if (length(hits) == 0L) return(NA_integer_)
  min(hits)
}

#' Parse drug triplet columns into a long tibble.
.parse_drug_triplets <- function(raw_df, drug_start, lab_ids, isolate_nums,
                                 source_file, source_rows, ingested_at) {
  drug_cols   <- names(raw_df)[drug_start:ncol(raw_df)]
  codes       <- sub("-.*$", "", drug_cols)
  unique_codes <- unique(codes)

  rows <- vector("list", length(unique_codes))
  for (i in seq_along(unique_codes)) {
    code          <- unique_codes[i]
    cols_for_code <- drug_cols[codes == code]

    mic_col  <- cols_for_code[!grepl("Other",             cols_for_code)][1]
    inst_col <- cols_for_code[ grepl("Other-Instrument",  cols_for_code, fixed = TRUE)][1]
    exp_col  <- cols_for_code[ grepl("Other-Expertized",  cols_for_code, fixed = TRUE)][1]

    drug_name <- if (!is.na(mic_col)) sub("^[^-]+-", "", mic_col) else code

    rows[[i]] <- tibble::tibble(
      source_file      = source_file,
      source_row       = source_rows,
      lab_id            = lab_ids,
      isolate_number    = isolate_nums,
      drug_code         = code,
      drug_name         = drug_name,
      mic               = if (!is.na(mic_col))  as.character(raw_df[[mic_col]])  else NA_character_,
      call_instr        = if (!is.na(inst_col)) as.character(raw_df[[inst_col]]) else NA_character_,
      call_expert       = if (!is.na(exp_col))  as.character(raw_df[[exp_col]])  else NA_character_,
      ingested_at       = ingested_at
    ) |>
      dplyr::mutate(
        result_mic        = mic,
        result_instrument = call_instr,
        result_expertized = call_expert
      ) |>
      dplyr::select(
        source_file, source_row, lab_id, isolate_number,
        drug_code, drug_name, mic, call_instr, call_expert,
        result_mic, result_instrument, result_expertized, ingested_at
      )
  }
  dplyr::bind_rows(rows)
}

# ── Column-finding helpers ────────────────────────────────────────────────────

.pick_col <- function(raw, patterns, default = NA_character_) {
  for (pat in patterns) {
    idx <- grep(pat, names(raw), ignore.case = TRUE, perl = TRUE)[1]
    if (!is.na(idx)) return(as.character(raw[[idx]]))
  }
  rep(default, nrow(raw))
}

# ── Single-file parser ────────────────────────────────────────────────────────

#' Parse one Vitek2 XLSX file.
#'
#' @param path  Character. Path to the .xlsx file.
#' @param name  Character. Display name (default = basename(path)).
#' @param ingested_at POSIXct timestamp shared across all rows from this file.
#' @return Named list:
#'   $vitek_raw   tibble (wide, per-isolate)
#'   $vitek_ast   tibble (long, per-drug)
#'   $n_rows      integer
#'   $date_range  character or NA
#'   $n_dupes     integer (rows with duplicate lab_id + isolate_number)
#'   $schema_ok   named logical vector
#'   $file_name   character
#'   $error       character or NULL
parse_one_vitek <- function(path, name = basename(path), ingested_at = Sys.time()) {
  err_result <- function(msg) list(
    vitek_raw  = vitek_raw_empty(),
    vitek_ast  = vitek_ast_empty(),
    n_rows     = 0L, date_range = NA_character_,
    n_dupes    = 0L, schema_ok  = check_vitek_schema(vitek_raw_empty()),
    file_name  = name, error = msg
  )

  if (!file.exists(path)) return(err_result(paste("File not found:", path)))

  raw <- tryCatch(
    readxl::read_excel(path, col_types = "text", .name_repair = "minimal"),
    error = function(e) NULL
  )
  if (is.null(raw) || nrow(raw) == 0) return(err_result("readxl returned empty data"))

  source_rows <- seq_len(nrow(raw))

  # ── PHI strip: remove Patient Name immediately ───────────────────────────
  phi_idx <- grep("^Patient.?Name$", names(raw), ignore.case = TRUE, perl = TRUE)
  if (length(phi_idx) > 0) raw <- raw[, -phi_idx, drop = FALSE]

  # ── Locate drug triplet columns ──────────────────────────────────────────
  drug_start <- .find_drug_start(names(raw))

  # ── Extract meta columns via pattern matching ────────────────────────────
  lab_id          <- .pick_col(raw, "^Lab.?ID$")
  isolate_number  <- .pick_col(raw, "^Isolate.?Number$")
  patient_id      <- .pick_col(raw, "^Patient.?ID$")
  specimen_type   <- .pick_col(raw, "^Specimen.?Type$")
  specimen_source <- .pick_col(raw, "^Specimen.?Source$")
  collection_date <- .parse_date_field(.pick_col(raw, "^Collection.?Date$"))
  testing_date    <- .parse_date_field(.pick_col(raw, "^Testing.?Date$"))
  organism_name   <- .pick_col(raw, "^Organism.?Name$")
  organism_code   <- .pick_col(raw, "^Organism.?Code$")
  bio_number      <- .pick_col(raw, "^Bio.?Number$")
  pct_probability <- .pick_col(raw, "^Percent.?Probability$")
  id_confidence   <- .pick_col(raw, "^ID.?Confidence$")
  selected_bp     <- .pick_col(raw, "^Selected.?BP.?Infection.?Site$")

  # ── Lab ID parsing (study / subject / target / cp_hint) ─────────────────
  parsed <- parse_lab_ids(lab_id)

  # ── Drug count ───────────────────────────────────────────────────────────
  n_drugs <- if (!is.na(drug_start)) {
    length(unique(sub("-.*$", "", names(raw)[drug_start:ncol(raw)])))
  } else {
    0L
  }

  # ── Assemble vitek_raw ───────────────────────────────────────────────────
  vitek_raw <- tibble::tibble(
    source_file      = name,
    source_row       = source_rows,
    lab_id           = lab_id,
    isolate_number   = isolate_number,
    patient_id       = patient_id,
    specimen_type    = specimen_type,
    specimen_source  = specimen_source,
    collection_date  = collection_date,
    testing_date     = testing_date,
    organism_name    = organism_name,
    organism_code    = organism_code,
    bio_number       = bio_number,
    pct_probability  = pct_probability,
    id_confidence    = id_confidence,
    selected_bp_site = selected_bp,
    parsed_study     = parsed$parsed_study,
    parsed_subject   = parsed$parsed_subject,
    parsed_target    = parsed$parsed_target,
    cp_hint          = parsed$cp_hint,
    n_drugs          = as.integer(n_drugs),
    ingested_at      = ingested_at,
    file_name        = name
  )

  # ── Build vitek_ast ──────────────────────────────────────────────────────
  vitek_ast <- if (!is.na(drug_start)) {
    tryCatch(
      .parse_drug_triplets(
        raw, drug_start, lab_id, isolate_number,
        source_file = name, source_rows = source_rows,
        ingested_at = ingested_at
      ),
      error = function(e) vitek_ast_empty()
    )
  } else {
    vitek_ast_empty()
  }

  # ── Dupe count ───────────────────────────────────────────────────────────
  n_dupes <- vitek_raw |>
    dplyr::filter(!is.na(lab_id), lab_id != "") |>
    dplyr::group_by(lab_id, isolate_number) |>
    dplyr::filter(dplyr::n() > 1) |>
    nrow()

  # ── Date range ───────────────────────────────────────────────────────────
  all_dates <- c(vitek_raw$testing_date, vitek_raw$collection_date)
  all_dates <- all_dates[!is.na(all_dates)]
  date_range <- if (length(all_dates) > 0) {
    paste0(format(min(all_dates)), " – ", format(max(all_dates)))
  } else {
    NA_character_
  }

  list(
    vitek_raw  = vitek_raw,
    vitek_ast  = vitek_ast,
    n_rows     = nrow(vitek_raw),
    date_range = date_range,
    n_dupes    = n_dupes,
    schema_ok  = check_vitek_schema(vitek_raw),
    file_name  = name,
    error      = NULL
  )
}

# ── Multi-file parser ─────────────────────────────────────────────────────────

#' Parse multiple Vitek2 XLSX files and combine results.
#'
#' @param paths  Character vector of file paths.
#' @param names  Character vector of display names (same length as paths).
#' @return Named list:
#'   $results   list of per-file results from parse_one_vitek()
#'   $vitek_raw combined tibble (all files stacked)
#'   $vitek_ast combined long tibble (all files stacked)
parse_vitek_files <- function(paths, names = NULL) {
  if (is.null(names) || length(names) != length(paths))
    names <- basename(paths)

  ingest_times <- rep(list(Sys.time()), length(paths))
  results   <- mapply(parse_one_vitek, paths, names, ingest_times, SIMPLIFY = FALSE)
  vitek_raw <- dplyr::bind_rows(lapply(results, `[[`, "vitek_raw"))
  vitek_ast <- dplyr::bind_rows(lapply(results, `[[`, "vitek_ast"))

  list(results = results, vitek_raw = vitek_raw, vitek_ast = vitek_ast)
}

# ── Schema check ──────────────────────────────────────────────────────────────

#' Check whether required columns are present and non-empty.
#' @param df  vitek_raw tibble.
#' @return Named logical vector.
check_vitek_schema <- function(df) {
  required <- c("lab_id", "isolate_number", "testing_date",
                "organism_name", "organism_code")
  setNames(
    sapply(required, function(col) {
      col %in% names(df) && any(!is.na(df[[col]]))
    }),
    required
  )
}

# ── Empty tibbles ─────────────────────────────────────────────────────────────

#' Empty vitek_raw tibble.
vitek_raw_empty <- function() {
  tibble::tibble(
    source_file      = character(),
    source_row       = integer(),
    lab_id           = character(),
    isolate_number   = character(),
    patient_id       = character(),
    specimen_type    = character(),
    specimen_source  = character(),
    collection_date  = as.Date(character()),
    testing_date     = as.Date(character()),
    organism_name    = character(),
    organism_code    = character(),
    bio_number       = character(),
    pct_probability  = character(),
    id_confidence    = character(),
    selected_bp_site = character(),
    parsed_study     = character(),
    parsed_subject   = character(),
    parsed_target    = character(),
    cp_hint          = character(),
    n_drugs          = integer(),
    ingested_at      = as.POSIXct(character()),
    file_name        = character()
  )
}

#' Empty vitek_ast tibble.
vitek_ast_empty <- function() {
  tibble::tibble(
    source_file       = character(),
    source_row        = integer(),
    lab_id            = character(),
    isolate_number    = character(),
    drug_code         = character(),
    drug_name         = character(),
    mic               = character(),
    call_instr        = character(),
    call_expert       = character(),
    result_mic        = character(),
    result_instrument = character(),
    result_expertized = character(),
    ingested_at       = as.POSIXct(character())
  )
}

# ── Column-finding helper (kept for backward compat with mod_ingestion) ───────
find_col_pattern <- function(col_names, patterns) {
  for (p in patterns) {
    hits <- col_names[grepl(p, col_names, ignore.case = TRUE)]
    if (length(hits) > 0) return(hits[1])
  }
  NULL
}
