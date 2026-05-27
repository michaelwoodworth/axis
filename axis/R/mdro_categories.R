# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/mdro_categories.R — Inventory MDRO category interpretation
#
# This layer bins linked isolates into the inventory categories used for flow
# views. It preserves the source/cleaned MDRO value and adds interpreted fields.
# ─────────────────────────────────────────────────────────────────────────────

AXIS_MDRO_CATEGORIES <- c(
  "ESBL", "CRE", "VRE", "MDRP", "MDRA", "Non-MDRO", "Unspecified"
)

axis_interpret_mdro_categories <- function(cleaned, cleaned_ast = NULL) {
  if (is.null(cleaned) || nrow(cleaned) == 0) return(tibble::tibble())

  d <- tibble::as_tibble(cleaned)
  if (!"link_id" %in% names(d)) {
    d$link_id <- as.character(seq_len(nrow(d)))
  }

  ast_flags <- axis_ast_resistance_flags(cleaned_ast)
  d <- d |>
    dplyr::left_join(ast_flags, by = "link_id") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c(
          "carbapenem_ri", "broad_ceph_ri", "fluoroquinolone_ri",
          "aminoglycoside_ri", "pip_tazo_ri", "amp_sulb_ri"
        )),
        ~ tidyr::replace_na(., FALSE)
      )
    )

  category <- axis_canonical_mdro_label(d$clean_mdro_category)
  raw_norm <- .axis_norm_text(d$clean_mdro_category)
  organism <- .axis_norm_text(d$clean_organism)

  is_pseudomonas <- grepl("PSEUD|PS\\.|AERUGINOSA", organism)
  is_acinetobacter <- grepl("ACI\\.|ACINET|BAUMANN", organism)
  is_enterococcus <- grepl("ENTEROCOCCUS|ENT\\.FAEC|E\\.FAEC", organism)
  is_enterobacterales <- .axis_is_enterobacterales(organism)

  antipseudomonal_classes <- rowSums(cbind(
    d$carbapenem_ri,
    d$fluoroquinolone_ri,
    d$aminoglycoside_ri,
    d$broad_ceph_ri,
    d$pip_tazo_ri
  ), na.rm = TRUE)

  acinetobacter_classes <- rowSums(cbind(
    d$carbapenem_ri,
    d$fluoroquinolone_ri,
    d$aminoglycoside_ri,
    d$broad_ceph_ri,
    d$amp_sulb_ri
  ), na.rm = TRUE)

  invalid_vre <- category == "VRE" & !is_enterococcus
  explicit <- !is.na(category) &
    !category %in% c("Non-MDRO", "Unspecified") &
    !invalid_vre
  positive_unspecified <- grepl("^(POSITIVE|YES|MDRO POSITIVE)$", raw_norm)
  negative <- grepl("^(NEGATIVE|NO|NONE|NON[- ]?MDRO|NO MDRO)$", raw_norm)

  derived_mdrp <- is_pseudomonas & antipseudomonal_classes >= 3
  derived_mdra <- is_acinetobacter & acinetobacter_classes >= 3
  derived_cre <- is_enterobacterales & d$carbapenem_ri
  derived_esbl <- is_enterobacterales & !derived_cre & d$broad_ceph_ri

  interpreted <- dplyr::case_when(
    explicit ~ category,
    derived_mdrp ~ "MDRP",
    derived_mdra ~ "MDRA",
    derived_cre ~ "CRE",
    derived_esbl ~ "ESBL",
    negative ~ "Non-MDRO",
    positive_unspecified ~ "Unspecified",
    invalid_vre ~ "Unspecified",
    TRUE ~ "Unspecified"
  )

  basis <- dplyr::case_when(
    explicit ~ "explicit label",
    derived_mdrp ~ "AST-derived MDRP phenotype",
    derived_mdra ~ "AST-derived MDRA phenotype",
    derived_cre ~ "AST-derived carbapenem resistance",
    derived_esbl ~ "AST-derived extended-spectrum beta-lactam resistance",
    negative ~ "explicit non-MDRO label",
    positive_unspecified ~ "positive label without specific category",
    invalid_vre ~ "VRE label on non-Enterococcus organism",
    TRUE ~ "missing or uninterpretable category"
  )

  tibble::tibble(
    link_id = d$link_id,
    inv_mdro_category = factor(interpreted, levels = AXIS_MDRO_CATEGORIES),
    inv_mdro_basis = basis
  ) |>
    dplyr::mutate(inv_mdro_category = as.character(inv_mdro_category))
}

axis_ast_resistance_flags <- function(cleaned_ast) {
  empty <- tibble::tibble(
    link_id = character(),
    carbapenem_ri = logical(),
    broad_ceph_ri = logical(),
    fluoroquinolone_ri = logical(),
    aminoglycoside_ri = logical(),
    pip_tazo_ri = logical(),
    amp_sulb_ri = logical()
  )
  if (is.null(cleaned_ast) || nrow(cleaned_ast) == 0 ||
      !"link_id" %in% names(cleaned_ast)) {
    return(empty)
  }

  ast <- tibble::as_tibble(cleaned_ast)
  if (!"drug_code" %in% names(ast)) ast$drug_code <- NA_character_
  if (!"call_expert" %in% names(ast)) ast$call_expert <- NA_character_
  if (!"call_instr" %in% names(ast)) ast$call_instr <- NA_character_

  ast |>
    dplyr::mutate(
      drug_code = toupper(trimws(as.character(.data$drug_code))),
      call = toupper(trimws(dplyr::coalesce(
        dplyr::na_if(as.character(.data$call_expert), ""),
        dplyr::na_if(as.character(.data$call_instr), "")
      ))),
      resistant_or_intermediate = .data$call %in% c("R", "I")
    ) |>
    dplyr::group_by(link_id) |>
    dplyr::summarise(
      carbapenem_ri = any(resistant_or_intermediate & drug_code %in% c("MEM", "ETP", "IPM", "DOR"), na.rm = TRUE),
      broad_ceph_ri = any(resistant_or_intermediate & drug_code %in% c("CRO", "CTX", "CAZ", "FEP", "ATM", "CPD"), na.rm = TRUE),
      fluoroquinolone_ri = any(resistant_or_intermediate & drug_code %in% c("CIP", "LEV"), na.rm = TRUE),
      aminoglycoside_ri = any(resistant_or_intermediate & drug_code %in% c("GM", "TM", "AN"), na.rm = TRUE),
      pip_tazo_ri = any(resistant_or_intermediate & drug_code %in% "TZP", na.rm = TRUE),
      amp_sulb_ri = any(resistant_or_intermediate & drug_code %in% "SAM", na.rm = TRUE),
      .groups = "drop"
    )
}

axis_canonical_mdro_label <- function(x) {
  raw <- .axis_norm_text(x)
  out <- rep(NA_character_, length(raw))

  out[grepl("VRE", raw)] <- "VRE"
  out[grepl("MDRA|CRAB|MULTIDRUG[- ]RESISTANT ACINETOBACTER|MDR ACINETOBACTER", raw)] <- "MDRA"
  out[grepl("MDRP|CRPA|MULTIDRUG[- ]RESISTANT PSEUDOMONAS|MDR PSEUDOMONAS", raw)] <- "MDRP"
  out[grepl("CRE|CRKP|CREC", raw)] <- "CRE"
  out[grepl("ESBL|ESCRE", raw)] <- "ESBL"
  out[grepl("^(NEGATIVE|NO|NONE|NON[- ]?MDRO|NO MDRO)$", raw)] <- "Non-MDRO"
  out[grepl("^(POSITIVE|YES|MDRO POSITIVE)$", raw)] <- "Unspecified"
  out[is.na(raw)] <- NA_character_

  out
}

.axis_norm_text <- function(x) {
  out <- toupper(trimws(as.character(x)))
  out[out %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  out
}

.axis_is_enterobacterales <- function(organism) {
  grepl(
    paste(
      c(
        "ESCH", "E\\.COLI", "KLEB", "K\\.", "ENTEROBACTER", "ENT\\.",
        "CITRO", "PROTEUS", "MORG", "PROV", "HAFNIA", "PANTOEA",
        "RAOU", "SERRATIA", "SALMONELLA", "SHIGELLA"
      ),
      collapse = "|"
    ),
    organism
  )
}
