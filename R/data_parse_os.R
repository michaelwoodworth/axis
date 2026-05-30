# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/data_parse_os.R — OpenSpecimen CSV parser with per-CP form mapping
#
# OpenSpecimen exports have a fixed set of "core" columns (Identifier,
# Specimen Label, CP Short Title, Class, Type, etc.) plus a variable set of
# custom-form columns whose prefix encodes the form name (e.g., "FAIR#…",
# "reactform#…", "REACT Specimen#…").
#
# CP_FORM_MAP defines, for each known form prefix:
#   pid_col      — column name for participant / patient ID
#   date_col     — column name for custom collection date (or NULL → use
#                  "Collection Event#Date and Time" as fallback)
#   mdro_col     — primary MDRO category column
#   mdro_fn      — optional function(row_df) → character to compute MDRO
#                  from multiple flags (overrides mdro_col when provided)
#   organism_col — genus/species column (or NULL)
#   isolate_col  — isolate number column (or NULL)
#   parent_type_col — parent/source specimen type column (or NULL)
#   cfu_col / cfu_eb_col / growth_cols / day_col / media_col — optional
#                  culture fields used by the Culture tab
#
# OUTPUT specimens tibble (§4b schema):
#   os_identifier, specimen_label, cp_short_title,
#   class, type, lineage, parent_label,
#   collection_dt   (POSIXct, from Collection Event#Date and Time),
#   available_qty, activity_status,
#   location_container, location_row, location_col, location_pos,
#   participant_id,       (from custom form)
#   custom_collection_date (Date, from custom form date_col),
#   custom_organism,      (from form organism col)
#   custom_mdro,          (from form mdro col / mdro_fn)
#   custom_form_blob      (list-col, all custom-form cols for this row)
# ─────────────────────────────────────────────────────────────────────────────

# ── CP form map ───────────────────────────────────────────────────────────────
# Each entry is keyed by the column-prefix that identifies the custom form.
# Keys are matched against the actual column headers via startsWith().

CP_FORM_MAP <- list(

  # FAIR / ARRRRG studies — prefix "FAIR#"
  "FAIR#" = list(
    pid_col      = "FAIR#Participant ID",
    date_col     = "FAIR#Collection Date",
    mdro_col     = "FAIR#MDRO Category",
    mdro_fn      = function(df) .derive_mdro_from_flags(
      df,
      primary_col = "FAIR#MDRO Category",
      flags = c("FAIR#ESBL", "FAIR#CRE", "FAIR#VRE", "FAIR#MDRP", "FAIR#MDRA", "FAIR#MDRO")
    ),
    organism_col = "FAIR#Genus species",
    isolate_col  = "FAIR#Isolate number",
    parent_type_col = "FAIR#Parent Specimen Type",
    cfu_col      = "FAIR#CFU/mL",
    cfu_eb_col   = "FAIR#CFU (EB)",
    growth_cols  = c("FAIR#ESBL Growth", "FAIR#VRE Growth", "FAIR#C. auris Growth"),
    day_col      = "FAIR#Day",
    media_col    = "FAIR#Selective Growth Media"
  ),

  # REACT Specimen — prefix "REACT Specimen#"
  "REACT Specimen#" = list(
    pid_col      = "REACT Specimen#Participant ID",
    date_col     = NULL,           # fall back to Collection Event#Date and Time
    mdro_col     = "REACT Specimen#MDRO",
    mdro_fn      = NULL,
    organism_col = "REACT Specimen#Genus Species",
    isolate_col  = "REACT Specimen#Isolate Number",
    parent_type_col = "REACT Specimen#Parent Specimen Type",
    cfu_col      = "REACT Specimen#CFU/mL",
    cfu_eb_col   = NULL,
    growth_cols  = c("REACT Specimen#ESBL Growth", "REACT Specimen#VRE Growth"),
    day_col      = "REACT Specimen#REACT Day",
    media_col    = "REACT Specimen#Selective Media"
  ),

  # SNT / reactform — prefix "reactform#"
  "reactform#" = list(
    pid_col      = "reactform#Patient ID",
    date_col     = "reactform#Collection Date",
    mdro_col     = "reactform#MDRO",
    mdro_fn      = NULL,
    organism_col = "reactform#Presumptive Organism",
    isolate_col  = "reactform#Isolate ID",
    parent_type_col = "reactform#Specimen Type",
    cfu_col      = NULL,
    cfu_eb_col   = NULL,
    growth_cols  = character(),
    day_col      = NULL,
    media_col    = NULL
  )
)

#' Return the active CP form map (default + analyst extras).
get_cp_form_map <- function() {
  extras <- getOption("axis.cp_form_map_extra", default = list())
  c(CP_FORM_MAP, extras)
}

# ── Date helper ───────────────────────────────────────────────────────────────

.parse_os_date <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "na", "N/A")] <- NA_character_
  suppressWarnings(
    as.Date(
      lubridate::parse_date_time(x,
        orders = c("m/d/Y HM", "m/d/Y HMS", "m/d/Y",
                   "Y-m-d HM", "Y-m-d HMS", "Y-m-d"),
        quiet = TRUE
      )
    )
  )
}

.parse_os_datetime <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "na", "N/A")] <- NA_character_
  suppressWarnings(
    lubridate::parse_date_time(x,
      orders = c("m/d/Y HM", "m/d/Y HMS", "m/d/Y",
                 "Y-m-d HM", "Y-m-d HMS", "Y-m-d"),
      quiet = TRUE
    )
  )
}

# ── Column helper ─────────────────────────────────────────────────────────────

.col_or_na <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) as.character(df[[col]])
  else rep(default, nrow(df))
}

.num_or_na <- function(df, col) {
  if (col %in% names(df)) {
    suppressWarnings(readr::parse_number(as.character(df[[col]])))
  } else {
    rep(NA_real_, nrow(df))
  }
}

.blankish <- function(x) {
  x <- trimws(as.character(x))
  is.na(x) | x %in% c("", "NA", "na", "N/A", "n/a", "NULL", "null")
}

.resolve_form_col <- function(col_names, preferred, prefix = NULL, pattern = NULL,
                              exclude = NULL) {
  if (!is.null(preferred) && preferred %in% col_names) return(preferred)
  candidates <- col_names
  if (!is.null(prefix)) candidates <- candidates[startsWith(candidates, prefix)]
  if (!is.null(pattern)) {
    candidates <- candidates[grepl(pattern, candidates, ignore.case = TRUE, perl = TRUE)]
  }
  if (!is.null(exclude) && length(candidates) > 0) {
    candidates <- candidates[!grepl(exclude, candidates, ignore.case = TRUE, perl = TRUE)]
  }
  if (length(candidates) == 0) return(NULL)
  candidates[[1]]
}

.collapse_cols <- function(df, cols) {
  cols <- cols[cols %in% names(df)]
  if (length(cols) == 0) return(rep(NA_character_, nrow(df)))
  apply(df[, cols, drop = FALSE], 1, function(row) {
    vals <- trimws(as.character(row))
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 0) NA_character_ else paste(vals, collapse = " | ")
  })
}

.truthy <- function(x) {
  x <- trimws(toupper(as.character(x)))
  !is.na(x) & x %in% c("1", "Y", "YES", "TRUE", "T", "POS", "POSITIVE", "GROWTH")
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.derive_mdro_from_flags <- function(df, primary_col = NULL, flags = character()) {
  primary <- if (!is.null(primary_col) && primary_col %in% names(df)) {
    trimws(as.character(df[[primary_col]]))
  } else {
    rep(NA_character_, nrow(df))
  }
  primary[primary %in% c("", "NA", "N/A", "na", "n/a")] <- NA_character_

  result <- primary
  needs_flag <- is.na(result)
  for (flag_col in flags) {
    if (!flag_col %in% names(df)) next
    token <- sub("^.*#", "", flag_col)
    token <- sub("\\s+.*$", "", token)
    hits <- needs_flag & .truthy(df[[flag_col]])
    result[hits] <- token
    needs_flag <- is.na(result)
  }
  result
}

.resolve_os_csv_path <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))
  if (ext == "csv") return(file_path)
  if (ext != "zip") stop("Unsupported OpenSpecimen export type: ", file_path)

  listing <- utils::unzip(file_path, list = TRUE)
  csv_entries <- listing$Name[grepl("\\.csv$", listing$Name, ignore.case = TRUE)]
  if (length(csv_entries) == 0L)
    stop("ZIP export contains no CSV: ", file_path)

  out_dir <- tempfile("axis_os_zip_")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(file_path, files = csv_entries[1], exdir = out_dir)
  file.path(out_dir, csv_entries[1])
}

.read_os_csv <- function(file_path, n_max = Inf) {
  csv_path <- .resolve_os_csv_path(file_path)
  suppressMessages(
    readr::read_csv(
      csv_path,
      n_max = n_max,
      show_col_types = FALSE,
      col_types = readr::cols(.default = readr::col_character())
    )
  )
}

# ── Detect form prefix ────────────────────────────────────────────────────────

#' Detect which CP form prefix applies to this file's columns.
#' Returns the matched form entry from CP_FORM_MAP, or NULL.
.detect_form <- function(col_names) {
  map <- get_cp_form_map()
  for (prefix in names(map)) {
    if (any(startsWith(col_names, prefix))) {
      return(list(prefix = prefix, entry = map[[prefix]]))
    }
  }
  NULL
}

# ── Single-file parser ────────────────────────────────────────────────────────

#' Parse one OpenSpecimen CSV export into a specimens tibble.
#'
#' @param file_path  Character. Path to the CSV file.
#' @param project_id Character. Identifier for this project (default = file stem).
#' @return specimens tibble. On error, returns specimens_empty() with a warning.
parse_os_specimens <- function(file_path, project_id = NULL) {
  if (is.null(project_id))
    project_id <- tools::file_path_sans_ext(basename(file_path))

  tryCatch({
    df <- .read_os_csv(file_path)
    if (nrow(df) == 0) return(specimens_empty())

    col_names <- names(df)
    form      <- .detect_form(col_names)

    # ── Core columns ───────────────────────────────────────────────────────
    os_identifier  <- .col_or_na(df, "Identifier")
    specimen_label_raw <- .col_or_na(df, "Specimen Label")
    cp_short_title <- .col_or_na(df, "CP Short Title", project_id)
    class_val      <- .col_or_na(df, "Class")
    type_val       <- .col_or_na(df, "Type")
    specimen_label <- .strip_cryo_glycerol_suffix(specimen_label_raw, type_val)
    lineage        <- .col_or_na(df, "Lineage")
    parent_label   <- .col_or_na(df, "Parent Specimen Label")
    available_qty  <- .num_or_na(df, "Available Quantity")
    activity_status <- .col_or_na(df, "Activity Status")
    loc_container  <- .col_or_na(df, "Location#Container")
    loc_row        <- .col_or_na(df, "Location#Row")
    loc_col        <- .col_or_na(df, "Location#Column")
    loc_pos        <- .col_or_na(df, "Location#Position")
    collection_dt  <- .parse_os_datetime(.col_or_na(df, "Collection Event#Date and Time"))

    # ── Custom-form columns (form-specific) ───────────────────────────────
    if (!is.null(form)) {
      entry <- form$entry

      participant_id <- if (!is.null(entry$pid_col) && entry$pid_col %in% col_names) {
        as.character(df[[entry$pid_col]])
      } else {
        rep(NA_character_, nrow(df))
      }

      custom_collection_date <- if (!is.null(entry$date_col) && entry$date_col %in% col_names) {
        .parse_os_date(df[[entry$date_col]])
      } else {
        as.Date(collection_dt)   # fall back to event date
      }

      custom_organism <- if (!is.null(entry$organism_col) && entry$organism_col %in% col_names) {
        as.character(df[[entry$organism_col]])
      } else {
        rep(NA_character_, nrow(df))
      }

      custom_parent_specimen_type <- if (!is.null(entry$parent_type_col) &&
                                         entry$parent_type_col %in% col_names) {
        as.character(df[[entry$parent_type_col]])
      } else {
        rep(NA_character_, nrow(df))
      }

      custom_day <- if (!is.null(entry$day_col) && entry$day_col %in% col_names) {
        as.character(df[[entry$day_col]])
      } else {
        rep(NA_character_, nrow(df))
      }

      custom_selective_media <- if (!is.null(entry$media_col) && entry$media_col %in% col_names) {
        as.character(df[[entry$media_col]])
      } else {
        rep(NA_character_, nrow(df))
      }

      # MDRO: prefer mdro_fn, then mdro_col
      custom_mdro <- if (!is.null(entry$mdro_fn)) {
        entry$mdro_fn(df)
      } else if (!is.null(entry$mdro_col) && entry$mdro_col %in% col_names) {
        as.character(df[[entry$mdro_col]])
      } else {
        rep(NA_character_, nrow(df))
      }

      # All custom-form cols as a list-column for future use
      custom_cols   <- col_names[startsWith(col_names, form$prefix)]
      custom_blob   <- purrr::map(seq_len(nrow(df)), function(i) {
        as.list(df[i, custom_cols, drop = FALSE])
      })

      growth_cols <- entry$growth_cols %||% character()
      growth_cols <- growth_cols[growth_cols %in% col_names]
      custom_growth_blob <- purrr::map(seq_len(nrow(df)), function(i) {
        as.list(df[i, growth_cols, drop = FALSE])
      })

      cfu_col <- .resolve_form_col(
        col_names,
        preferred = entry$cfu_col %||% NULL,
        prefix = form$prefix,
        pattern = "CFU",
        exclude = "\\bEB\\b|\\(EB\\)"
      )
      cfu_eb_col <- .resolve_form_col(
        col_names,
        preferred = entry$cfu_eb_col %||% NULL,
        prefix = form$prefix,
        pattern = "CFU.*EB|EB.*CFU|CFU \\(EB\\)"
      )

      cfu_primary <- if (!is.null(cfu_col)) as.character(df[[cfu_col]]) else rep(NA_character_, nrow(df))
      cfu_eb <- if (!is.null(cfu_eb_col)) as.character(df[[cfu_eb_col]]) else rep(NA_character_, nrow(df))
      cfu_raw <- dplyr::if_else(.blankish(cfu_primary), cfu_eb, cfu_primary)
      growth_flag <- .collapse_cols(df, unique(c(growth_cols, cfu_eb_col)))
      cfu_norm <- parse_cfu(cfu_raw, growth_flag = growth_flag)

    } else {
      # No recognised form prefix
      participant_id         <- rep(NA_character_, nrow(df))
      custom_collection_date <- as.Date(collection_dt)
      custom_organism        <- rep(NA_character_, nrow(df))
      custom_parent_specimen_type <- rep(NA_character_, nrow(df))
      custom_day             <- rep(NA_character_, nrow(df))
      custom_selective_media <- rep(NA_character_, nrow(df))
      custom_mdro            <- rep(NA_character_, nrow(df))
      custom_blob            <- vector("list", nrow(df))
      custom_growth_blob     <- vector("list", nrow(df))
      cfu_norm               <- parse_cfu(rep(NA_character_, nrow(df)))
    }

    result <- tibble::tibble(
      source_file            = basename(file_path),
      source_row             = seq_len(nrow(df)),
      project_id             = project_id,
      os_identifier          = os_identifier,
      specimen_label_raw     = specimen_label_raw,
      specimen_label         = specimen_label,
      cp_short_title         = cp_short_title,
      class                  = class_val,
      type                   = type_val,
      lineage                = lineage,
      parent_label           = parent_label,
      collection_dt          = collection_dt,
      available_qty          = available_qty,
      activity_status        = activity_status,
      location_container     = loc_container,
      location_row           = loc_row,
      location_col           = loc_col,
      location_pos           = loc_pos,
      participant_id         = participant_id,
      custom_collection_date = custom_collection_date,
      custom_organism        = custom_organism,
      custom_parent_specimen_type = custom_parent_specimen_type,
      custom_day             = custom_day,
      custom_selective_media = custom_selective_media,
      custom_growth_blob     = custom_growth_blob,
      custom_mdro            = custom_mdro,
      cfu_raw                = cfu_norm$cfu_raw,
      cfu_log10              = cfu_norm$cfu_log10,
      cfu_value              = cfu_norm$cfu_value,
      cfu_unit               = cfu_norm$cfu_unit,
      cfu_censored           = cfu_norm$cfu_censored,
      growth_method          = cfu_norm$growth_method,
      is_pseudocount         = cfu_norm$is_pseudocount,
      cfu_flag               = cfu_norm$cfu_flag,
      has_quant              = !is.na(cfu_norm$cfu_log10) | cfu_norm$is_pseudocount,
      custom_form_blob       = custom_blob
    ) |>
      dplyr::filter(
        !is.na(os_identifier),
        os_identifier != "",
        os_identifier != "NA"
      )

    result

  }, error = function(e) {
    warning(sprintf("parse_os_specimens() failed on %s: %s", file_path, e$message))
    specimens_empty()
  })
}

#' Parse and combine multiple OS project files.
#'
#' @param file_paths  Character vector of CSV paths.
#' @param project_ids Character vector of project IDs (same length; default = file stems).
#' @return Combined specimens tibble.
parse_os_specimens_multi <- function(file_paths, project_ids = NULL) {
  if (length(file_paths) == 0) return(specimens_empty())
  if (is.null(project_ids))
    project_ids <- tools::file_path_sans_ext(basename(file_paths))
  purrr::map2_dfr(file_paths, project_ids, parse_os_specimens)
}

# ── Project scanner ───────────────────────────────────────────────────────────

#' Scan the OS exports directory and return a summary tibble of available projects.
#'
#' @param data_dir  Path to directory containing OS CSV exports.
#' @return tibble: project_id, file_path, file_name, study_label, n_specimens, color.
scan_os_projects <- function(data_dir) {
  empty <- tibble::tibble(
    project_id  = character(), file_path   = character(),
    file_name   = character(), study_label = character(),
    n_specimens = integer(),   color       = character()
  )

  if (!dir.exists(data_dir)) return(empty)

  export_files <- list.files(data_dir, pattern = "\\.(csv|zip)$",
                             full.names = TRUE, ignore.case = TRUE)
  if (length(export_files) == 0) return(empty)

  palette <- c("#1f3a5f", "#5b8def", "#d4a017", "#15803d",
               "#7c3aed", "#0891b2", "#be185d")

  scanned <- purrr::imap_dfr(export_files, function(f, i) {
    tryCatch({
      hdr   <- .read_os_csv(f, n_max = 1)
      n_all <- nrow(.read_os_csv(f)) - 0L
      proj  <- tools::file_path_sans_ext(basename(f))
      label <- if ("CP Short Title" %in% names(hdr) && nrow(hdr) > 0)
        as.character(hdr[["CP Short Title"]][1]) else proj
      tibble::tibble(
        project_id  = proj,
        file_path   = f,
        file_name   = basename(f),
        study_label = label,
        n_specimens = as.integer(n_all),
        color       = palette[((i - 1L) %% length(palette)) + 1L]
      )
    }, error = function(e) NULL)
  })

  scanned |>
    dplyr::mutate(
      .ext = tolower(tools::file_ext(file_name)),
      .rank = dplyr::if_else(.ext == "csv", 1L, 2L)
    ) |>
    dplyr::arrange(study_label, n_specimens, .rank) |>
    dplyr::distinct(study_label, n_specimens, .keep_all = TRUE) |>
    dplyr::select(-.ext, -.rank) |>
    dplyr::mutate(color = palette[((dplyr::row_number() - 1L) %% length(palette)) + 1L])
}

# ── Empty tibble ──────────────────────────────────────────────────────────────

#' Empty specimens tibble matching canonical §4b schema.
specimens_empty <- function() {
  tibble::tibble(
    source_file            = character(),
    source_row             = integer(),
    project_id             = character(),
    os_identifier          = character(),
    specimen_label_raw     = character(),
    specimen_label         = character(),
    cp_short_title         = character(),
    class                  = character(),
    type                   = character(),
    lineage                = character(),
    parent_label           = character(),
    collection_dt          = lubridate::ymd_hms(character()),
    available_qty          = double(),
    activity_status        = character(),
    location_container     = character(),
    location_row           = character(),
    location_col           = character(),
    location_pos           = character(),
    participant_id         = character(),
    custom_collection_date = as.Date(character()),
    custom_organism        = character(),
    custom_parent_specimen_type = character(),
    custom_day             = character(),
    custom_selective_media = character(),
    custom_growth_blob     = list(),
    custom_mdro            = character(),
    cfu_raw                = character(),
    cfu_log10              = double(),
    cfu_value              = double(),
    cfu_unit               = character(),
    cfu_censored           = logical(),
    growth_method          = character(),
    is_pseudocount         = logical(),
    cfu_flag               = character(),
    has_quant              = logical(),
    custom_form_blob       = list()
  )
}

.strip_cryo_glycerol_suffix <- function(specimen_label, type_val) {
  label <- trimws(as.character(specimen_label))
  is_cryo <- !is.na(type_val) & trimws(as.character(type_val)) == "Cryopreserved Cells"
  label[is_cryo] <- sub(
    "w\\s*/\\s*(?:o\\s*)?glycerol.*$",
    "",
    label[is_cryo],
    ignore.case = TRUE,
    perl = TRUE
  )
  label
}
