read_axis_csv <- function(path) {
  if (!file.exists(path)) {
    stop("File does not exist: ", path, call. = FALSE)
  }
  utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}

clean_axis_names <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

require_axis_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(label, " is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(data)
}

reject_phi_like_columns <- function(data, label) {
  blocked <- c(
    "patient_name", "patient_first_name", "patient_last_name", "mrn",
    "medical_record_number", "date_of_birth", "dob"
  )
  found <- intersect(blocked, names(data))
  if (length(found) > 0) {
    stop(label, " contains disallowed patient-data columns: ", paste(found, collapse = ", "), call. = FALSE)
  }
  invisible(data)
}

normalize_text <- function(x) {
  trimws(as.character(x))
}

normalize_id <- function(x) {
  toupper(gsub("[^A-Za-z0-9]", "", normalize_text(x)))
}

as_axis_date <- function(x) {
  as.Date(x)
}

import_vitek <- function(path) {
  data <- read_axis_csv(path)
  names(data) <- clean_axis_names(names(data))
  reject_phi_like_columns(data, "VITEK2 export")

  required <- c(
    "isolate_id", "specimen_accession_id", "species", "specimen_type",
    "study", "site", "collection_date", "test_date", "antibiotic",
    "interpretation"
  )
  require_axis_columns(data, required, "VITEK2 export")

  data$isolate_id <- normalize_text(data$isolate_id)
  data$specimen_accession_id <- normalize_text(data$specimen_accession_id)
  data$specimen_key <- normalize_id(data$specimen_accession_id)
  data$species <- normalize_text(data$species)
  data$specimen_type <- normalize_text(data$specimen_type)
  data$study <- normalize_text(data$study)
  data$site <- normalize_text(data$site)
  data$collection_date <- as_axis_date(data$collection_date)
  data$test_date <- as_axis_date(data$test_date)
  data$antibiotic <- normalize_text(data$antibiotic)
  data$interpretation <- toupper(normalize_text(data$interpretation))

  if (!"mdro_hint" %in% names(data)) {
    # Assumption: VITEK rows may not carry a source MDRO label; rules can infer it.
    data$mdro_hint <- NA_character_
  } else {
    data$mdro_hint <- toupper(normalize_text(data$mdro_hint))
  }

  data
}
