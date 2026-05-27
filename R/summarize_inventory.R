if (!exists("link_results", mode = "function")) {
  source_path <- c("R/link_results.R", "../../R/link_results.R")
  source_path <- source_path[file.exists(source_path)][1]
  if (is.na(source_path)) {
    stop("Could not find R/link_results.R", call. = FALSE)
  }
  source(source_path)
}

summarize_inventory <- function(linked_results) {
  if (nrow(linked_results) == 0) {
    return(data.frame())
  }

  required <- c(
    "mdro_category", "species_vitek", "specimen_type_openspecimen",
    "study_openspecimen", "site_openspecimen", "link_status", "specimen_id"
  )
  require_axis_columns(linked_results, required, "Linked results")

  matched <- linked_results[linked_results$link_status == "matched", , drop = FALSE]
  if (nrow(matched) == 0) {
    return(data.frame())
  }

  groups <- c(
    "mdro_category", "species_vitek", "specimen_type_openspecimen",
    "study_openspecimen", "site_openspecimen"
  )

  aggregate(
    matched$specimen_id,
    matched[groups],
    function(x) length(unique(x))
  ) |>
    stats::setNames(c(groups, "specimen_count"))
}
