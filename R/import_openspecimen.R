if (!exists("read_axis_csv", mode = "function")) {
  source_path <- c("R/import_vitek.R", "../../R/import_vitek.R")
  source_path <- source_path[file.exists(source_path)][1]
  if (is.na(source_path)) {
    stop("Could not find R/import_vitek.R", call. = FALSE)
  }
  source(source_path)
}

import_openspecimen <- function(path) {
  data <- read_axis_csv(path)
  names(data) <- clean_axis_names(names(data))
  reject_phi_like_columns(data, "OpenSpecimen export")

  required <- c(
    "specimen_id", "specimen_label", "species", "specimen_type",
    "study", "site", "collection_date", "inventory_status"
  )
  require_axis_columns(data, required, "OpenSpecimen export")

  data$specimen_id <- normalize_text(data$specimen_id)
  data$specimen_label <- normalize_text(data$specimen_label)
  data$specimen_key <- normalize_id(data$specimen_label)
  data$species <- normalize_text(data$species)
  data$specimen_type <- normalize_text(data$specimen_type)
  data$study <- normalize_text(data$study)
  data$site <- normalize_text(data$site)
  data$collection_date <- as_axis_date(data$collection_date)
  data$inventory_status <- normalize_text(data$inventory_status)

  if (!"quantity" %in% names(data)) {
    # Assumption: missing quantity means one inventory row represents one specimen.
    data$quantity <- 1
  }
  data$quantity <- as.numeric(data$quantity)

  if (!"unit" %in% names(data)) {
    data$unit <- NA_character_
  }

  if (!"mdro_hint" %in% names(data)) {
    data$mdro_hint <- NA_character_
  } else {
    data$mdro_hint <- toupper(normalize_text(data$mdro_hint))
  }

  data
}
