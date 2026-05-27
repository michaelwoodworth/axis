if (!exists("add_mdro_category", mode = "function")) {
  source_path <- c("R/mdro_categories.R", "../../R/mdro_categories.R")
  source_path <- source_path[file.exists(source_path)][1]
  if (is.na(source_path)) {
    stop("Could not find R/mdro_categories.R", call. = FALSE)
  }
  source(source_path)
}

link_results <- function(vitek, openspecimen) {
  require_axis_columns(vitek, c("specimen_key", "isolate_id"), "VITEK2 data")
  require_axis_columns(openspecimen, c("specimen_key", "specimen_id"), "OpenSpecimen data")

  vitek <- add_mdro_category(vitek)

  linked <- merge(
    vitek,
    openspecimen,
    by = "specimen_key",
    all.x = TRUE,
    suffixes = c("_vitek", "_openspecimen")
  )

  linked$link_status <- ifelse(is.na(linked$specimen_id), "unmatched", "matched")
  linked$link_method <- ifelse(linked$link_status == "matched", "normalized_specimen_label", NA_character_)

  # Assumption: the synthetic VITEK accession maps to the synthetic OpenSpecimen label.
  linked
}
