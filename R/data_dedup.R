# ─────────────────────────────────────────────────────────────────────────────
# AXIS · R/data_dedup.R  — Dedup logic
# Deduplicates vitek_raw → vitek_unique on (lab_id × isolate_number).
# Three configurable rules: "latest" (default), "first", "flag"/"manual".
# ─────────────────────────────────────────────────────────────────────────────

#' Deduplicate vitek_raw → vitek_unique.
#'
#' @param vitek_raw  tibble. Must contain lab_id, isolate_number, file_name.
#' @param rule       One of "latest" (last file loaded wins), "first",
#'                   or "flag"/"manual" (keep all, mark conflicts).
#' @return vitek_unique tibble (one canonical row per lab_id × isolate_number)
#'         with added columns: n_source_rows (int), conflict (lgl).
dedup_vitek <- function(vitek_raw, rule = "latest") {
  if (is.null(vitek_raw) || nrow(vitek_raw) == 0)
    return(vitek_unique_empty())

  required <- c("lab_id", "isolate_number", "file_name")
  missing  <- setdiff(required, names(vitek_raw))
  if (length(missing) > 0)
    stop("vitek_raw is missing required columns: ", paste(missing, collapse = ", "))

  # Add a row-order column so "latest" = last in the uploaded list order
  vitek_raw <- vitek_raw |> dplyr::mutate(.row_order = dplyr::row_number())

  grouped <- vitek_raw |>
    dplyr::group_by(lab_id, isolate_number) |>
    dplyr::mutate(
      n_source_rows = dplyr::n(),
      conflict      = dplyr::n() > 1L
    )

  winner <- switch(rule,
    latest = grouped |>
      dplyr::arrange(dplyr::desc(.row_order), .by_group = TRUE) |>
      dplyr::slice(1L),
    first  = grouped |>
      dplyr::arrange(.row_order, .by_group = TRUE) |>
      dplyr::slice(1L),
    flag   = grouped,  # keep all; conflict column marks them
    manual = grouped,
    {
      warning(paste0("Unknown dedup rule '", rule, "'; defaulting to 'latest'"))
      grouped |>
        dplyr::arrange(dplyr::desc(.row_order), .by_group = TRUE) |>
        dplyr::slice(1L)
    }
  )

  dplyr::ungroup(winner) |> dplyr::select(-.row_order)
}

#' Summary stats for the dedup step — useful for rendering the dedup card.
#'
#' @param vitek_raw    Raw tibble (pre-dedup).
#' @param vitek_unique Deduplicated tibble (post-dedup).
#' @return Named list: n_raw, n_unique, n_collapsed, n_conflict_files.
dedup_summary <- function(vitek_raw, vitek_unique) {
  if (is.null(vitek_raw) || nrow(vitek_raw) == 0)
    return(list(n_raw = 0L, n_unique = 0L, n_collapsed = 0L, n_conflict_files = 0L))

  n_raw       <- nrow(vitek_raw)
  n_unique    <- if (!is.null(vitek_unique)) nrow(vitek_unique) else n_raw
  n_collapsed <- n_raw - n_unique

  n_conflict_files <- 0L
  if (!is.null(vitek_unique) && "conflict" %in% names(vitek_unique)) {
    conflict_ids <- vitek_unique |>
      dplyr::filter(conflict) |>
      dplyr::select(lab_id, isolate_number)

    if (nrow(conflict_ids) > 0) {
      n_conflict_files <- vitek_raw |>
        dplyr::semi_join(conflict_ids, by = c("lab_id", "isolate_number")) |>
        dplyr::pull(file_name) |>
        unique() |>
        length()
    }
  }

  list(
    n_raw            = n_raw,
    n_unique         = n_unique,
    n_collapsed      = n_collapsed,
    n_conflict_files = n_conflict_files
  )
}

#' Empty vitek_unique tibble matching the new schema.
vitek_unique_empty <- function() {
  tibble::tibble(
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
    file_name        = character(),
    n_source_rows    = integer(),
    conflict         = logical()
  )
}
