mdro_category <- function(species, mdro_hint = NA_character_, antibiotic = NA_character_,
                          interpretation = NA_character_) {
  species_l <- tolower(trimws(species))
  hint <- toupper(trimws(mdro_hint))
  drug <- toupper(trimws(antibiotic))
  call <- toupper(trimws(interpretation))

  direct <- ifelse(hint %in% c("CRE", "ESBL", "VRE", "MRSA", "MDR-PA", "MDRPA"), hint, NA_character_)
  direct <- ifelse(direct == "MDRPA", "MDR-PA", direct)

  inferred <- rep("Other", length(species_l))
  inferred[grepl("enterococcus", species_l) & drug == "VANCOMYCIN" & call == "R"] <- "VRE"
  inferred[grepl("staphylococcus aureus", species_l) & drug == "OXACILLIN" & call == "R"] <- "MRSA"
  inferred[grepl("pseudomonas aeruginosa", species_l) & call == "R"] <- "MDR-PA"
  inferred[grepl("escherichia coli|klebsiella|enterobacter", species_l) &
             drug %in% c("MEROPENEM", "IMIPENEM", "ERTAPENEM") & call == "R"] <- "CRE"
  inferred[grepl("escherichia coli|klebsiella", species_l) &
             drug %in% c("CEFTRIAXONE", "CEFTAZIDIME", "CEFEPIME") & call == "R"] <- "ESBL"

  ifelse(is.na(direct), inferred, direct)
}

add_mdro_category <- function(data) {
  if (!"mdro_hint" %in% names(data)) {
    data$mdro_hint <- NA_character_
  }
  if (!"antibiotic" %in% names(data)) {
    data$antibiotic <- NA_character_
  }
  if (!"interpretation" %in% names(data)) {
    data$interpretation <- NA_character_
  }
  data$mdro_category <- mdro_category(
    data$species,
    data$mdro_hint,
    data$antibiotic,
    data$interpretation
  )
  data
}
