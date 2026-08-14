# ─────────────────────────────────────────────────────────────────────────────
# AXIS · inst/bench/bench_confirm.R
#
# Measures the cost of confirming a single Vitek↔OpenSpecimen link, and the
# cost of a full cleaned-data rebuild/export, on a representative dataset.
#
# Usage (from the repository root):
#
#   Rscript -e 'Sys.setenv(AXIS_TEST_DATA_DIR="/absolute/path/to/01.data")' \
#           -e 'source("inst/bench/bench_confirm.R")'
#
#   # or
#   AXIS_TEST_DATA_DIR=/absolute/path/to/01.data Rscript inst/bench/bench_confirm.R
#
# Without AXIS_TEST_DATA_DIR the script falls back to a synthetic dataset of a
# comparable shape, so it always runs, but the numbers are only meaningful on a
# real ingestion batch.
#
# Output is deliberately non-sensitive: row counts and elapsed seconds only.
# No specimen identifiers, participant identifiers or source values are printed
# or written. The results CSV is written to a temporary directory, not the repo.
#
# The script detects which persistence API is present, so the same file
# benchmarks both the pre-change pipeline (commit_matched_links() doing
# everything) and the split pipeline (confirm_links() + rebuild_cleaned() +
# write_cleaned_outputs()).
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(tidyr)
  library(readr); library(readxl); library(lubridate); library(purrr)
})

.bench_repo_root <- function() {
  root <- Sys.getenv("AXIS_REPO_ROOT", unset = NA_character_)
  if (!is.na(root) && nzchar(root)) return(normalizePath(root))
  for (candidate in c(".", "..", "../..")) {
    if (dir.exists(file.path(candidate, "R")) &&
        file.exists(file.path(candidate, "R", "store.R"))) {
      return(normalizePath(candidate))
    }
  }
  stop("Run this script from the AXIS repository root, or set AXIS_REPO_ROOT.")
}

ROOT <- .bench_repo_root()

# Data/persistence layer only; the Shiny modules are not needed to benchmark
# the confirmation path and pull in optional UI packages.
for (f in list.files(file.path(ROOT, "R"), "[.]R$", full.names = TRUE)) {
  if (!grepl("/(mod_|theme|ui_helpers)", f)) source(f)
}

N_CONFIRMATIONS <- as.integer(Sys.getenv("AXIS_BENCH_N", unset = "8"))
CACHE <- Sys.getenv("AXIS_BENCH_CACHE",
                    unset = file.path(tempdir(), "axis_bench_fixture.rds"))

.elapsed <- function(expr) {
  t0 <- proc.time()[["elapsed"]]
  value <- force(expr)
  list(seconds = proc.time()[["elapsed"]] - t0, value = value)
}

.time_only <- function(expr) .elapsed(expr)$seconds

# ── Dataset ──────────────────────────────────────────────────────────────────

.synthetic_fixture <- function(n_isolates = 1900L, n_specimens = 11500L,
                               drugs_per_isolate = 34L) {
  set.seed(20260813)
  lab_ids <- sprintf("SYN%05dCRE1of1", seq_len(n_isolates))
  vitek_raw <- tibble::tibble(
    source_file = "synthetic.xlsx",
    source_row = seq_len(n_isolates),
    lab_id = lab_ids,
    isolate_number = "1",
    patient_id = NA_character_,
    specimen_type = "Isolate",
    specimen_source = "Rectal",
    collection_date = as.Date("2026-01-01") + (seq_len(n_isolates) %% 300),
    testing_date = as.Date("2026-01-03") + (seq_len(n_isolates) %% 300),
    organism_name = "Klebsiella pneumoniae",
    organism_code = "KPN",
    bio_number = NA_character_,
    pct_probability = "99",
    id_confidence = "Excellent",
    selected_bp_site = NA_character_,
    parsed_study = "SYNTH",
    parsed_subject = sprintf("SYN%05d", seq_len(n_isolates)),
    parsed_target = "CRE",
    cp_hint = "SNT/APPS/React",
    n_drugs = drugs_per_isolate,
    ingested_at = Sys.time(),
    file_name = "synthetic.xlsx"
  )
  vitek_ast <- tidyr::expand_grid(
    idx = seq_len(n_isolates),
    drug = seq_len(drugs_per_isolate)
  ) |>
    dplyr::transmute(
      source_file = "synthetic.xlsx",
      source_row = .data$idx,
      lab_id = lab_ids[.data$idx],
      isolate_number = "1",
      drug_code = sprintf("D%02d", .data$drug),
      drug_name = sprintf("Drug %02d", .data$drug),
      mic = "<=1",
      call_instr = "S",
      call_expert = "S",
      result_mic = "<=1",
      result_instrument = "S",
      result_expertized = "S",
      ingested_at = Sys.time()
    )
  specimens <- tibble::tibble(
    source_file = "synthetic_os.csv",
    source_row = seq_len(n_specimens),
    project_id = "SYNTH",
    os_identifier = sprintf("OS-%06d", seq_len(n_specimens)),
    specimen_label_raw = sprintf("SYN%05d_P%d", seq_len(n_specimens) %% n_isolates + 1L,
                                 seq_len(n_specimens) %% 4L),
    specimen_label = sprintf("SYN%05d_P%d", seq_len(n_specimens) %% n_isolates + 1L,
                             seq_len(n_specimens) %% 4L),
    cp_short_title = "SNT/APPS/React",
    class = "Fluid",
    type = ifelse(seq_len(n_specimens) %% 3L == 0L, "Cryopreserved Cells", "Aliquot"),
    lineage = "Aliquot",
    parent_label = NA_character_,
    collection_dt = as.POSIXct("2026-01-01", tz = "UTC"),
    available_qty = 1,
    activity_status = "Active",
    location_container = NA_character_, location_row = NA_character_,
    location_col = NA_character_, location_pos = NA_character_,
    anatomic_site = "Rectal",
    participant_id = sprintf("SYN%05d", seq_len(n_specimens) %% n_isolates + 1L),
    custom_collection_date = as.Date("2026-01-01"),
    custom_organism = "Klebsiella pneumoniae",
    custom_parent_specimen_type = "Stool",
    custom_day = "d0", custom_selective_media = NA_character_,
    custom_site = "Rectal", custom_growth_blob = NA_character_,
    custom_mdro = "CRE",
    cfu_raw = NA_character_, cfu_log10 = NA_real_, cfu_value = NA_real_,
    cfu_unit = NA_character_, cfu_censored = NA, growth_method = NA_character_,
    is_pseudocount = NA, cfu_flag = NA_character_, has_quant = FALSE,
    custom_form_blob = NA_character_
  )
  list(vitek_raw = vitek_raw, vitek_ast = vitek_ast,
       vitek_unique = dedup_vitek(vitek_raw, "latest"),
       specimens = specimens, source = "synthetic")
}

.private_data_dir <- function() {
  d <- Sys.getenv("AXIS_TEST_DATA_DIR", unset = NA_character_)
  if (is.na(d) || !nzchar(d) || !dir.exists(d)) return(NA_character_)
  normalizePath(d)
}

.load_fixture <- function() {
  if (nzchar(CACHE) && file.exists(CACHE)) {
    message("Using cached fixture: ", CACHE)
    return(readRDS(CACHE))
  }
  dd <- .private_data_dir()
  if (is.na(dd)) {
    message("AXIS_TEST_DATA_DIR is not set; using a synthetic dataset.")
    fx <- .synthetic_fixture()
  } else {
    message("Parsing private exports from: ", dd)
    vitek_files <- list.files(file.path(dd, "02.vitek2_exports"),
                              "\\.xlsx$", full.names = TRUE)
    os_files <- list.files(file.path(dd, "01.openspecimen_exports"),
                           "\\.csv$", full.names = TRUE)
    parsed <- parse_vitek_files(vitek_files)
    specimens <- parse_os_specimens_multi(os_files)
    fx <- list(
      vitek_raw = parsed$vitek_raw,
      vitek_ast = parsed$vitek_ast,
      vitek_unique = dedup_vitek(parsed$vitek_raw, "latest"),
      specimens = specimens,
      source = "private exports"
    )
  }
  message("Matching…")
  fx$match_candidates <- auto_match(fx$vitek_unique, fx$specimens)
  fx$buckets <- bucket_results(fx$match_candidates, fx$vitek_unique)
  if (nzchar(CACHE)) saveRDS(fx, CACHE)
  fx
}

# ── Harness ──────────────────────────────────────────────────────────────────

.fresh_db <- function() {
  path <- tempfile(pattern = "axis_bench_", fileext = ".duckdb")
  list(path = path, conn = open_db(path))
}

.source_row_counts <- function(conn) {
  tables <- c("vitek_raw", "vitek_ast", "specimens")
  present <- DBI::dbListTables(conn)
  vapply(tables, function(tb) {
    if (!tb %in% present) return(0L)
    as.integer(DBI::dbGetQuery(
      conn, paste0("SELECT COUNT(*) n FROM ", DBI::dbQuoteIdentifier(conn, tb))
    )$n[[1]])
  }, integer(1))
}

# Which persistence API does the checked-out branch provide? The benchmark is
# run against more than one implementation of this handoff, so it binds by
# capability rather than assuming one set of names.
.api_flavour <- function() {
  if (exists("confirm_links", mode = "function")) return("claude")
  if (exists("record_confirmed_links", mode = "function")) return("codex")
  "pre-change"
}

.api_persist <- function(conn, batch_id, vr, va, sp) {
  switch(.api_flavour(),
    claude = persist_source_batch(conn, batch_id, vr, va, sp),
    codex  = persist_source_batch_once(conn, batch_id, vr, va, sp),
    write_ingested_tables(conn, batch_id = batch_id, vitek_raw = vr,
                          vitek_ast = va, specimens = sp))
}

.api_confirm <- function(conn, rows, batch_id) {
  switch(.api_flavour(),
    claude = confirm_links(conn, rows, batch_id, match_method = "manual_selected",
                           created_by = "bench", rationale = "benchmark"),
    codex  = record_confirmed_links(conn, rows, batch_id, "manual_selected",
                                    "bench", "benchmark"),
    stop("No split confirmation API present."))
}

.api_export <- function(conn, batch_id, vu, va, sp, output_dir, formats) {
  switch(.api_flavour(),
    claude = rebuild_and_export_cleaned(conn, batch_id = batch_id,
               vitek_unique = vu, vitek_ast = va, specimens = sp,
               output_dir = output_dir, formats = formats),
    codex  = rebuild_and_export_cleaned_data(conn, batch_id = batch_id,
               vitek_unique = vu, vitek_ast = va, specimens = sp,
               output_dir = output_dir, formats = formats),
    stop("No split export API present."))
}

.candidate_rows <- function(fx, n) {
  pool <- dplyr::bind_rows(
    if (!is.null(fx$buckets$review)) fx$buckets$review else NULL,
    if (!is.null(fx$buckets$matched)) fx$buckets$matched else NULL
  )
  pool <- pool |>
    dplyr::filter(!is.na(.data$os_identifier), nzchar(.data$os_identifier)) |>
    dplyr::distinct(.data$lab_id, .data$isolate_number, .data$os_identifier,
                    .keep_all = TRUE)
  if (nrow(pool) < n) stop("Not enough candidates to benchmark ", n, " confirmations.")
  pool |> dplyr::slice_head(n = n)
}

bench_run <- function(fx = .load_fixture(), n = N_CONFIRMATIONS) {
  batch_id <- "B-bench"
  out_dir <- file.path(tempdir(), "axis_bench_exports")
  unlink(out_dir, recursive = TRUE)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  db <- .fresh_db()
  on.exit(close_db(db$conn), add = TRUE)
  conn <- db$conn

  split_api <- .api_flavour() != "pre-change"
  rows <- .candidate_rows(fx, n)

  message("\n== Dataset (row counts only) ==")
  counts <- c(
    vitek_raw    = nrow(fx$vitek_raw),
    vitek_ast    = nrow(fx$vitek_ast),
    vitek_unique = nrow(fx$vitek_unique),
    specimens    = nrow(fx$specimens),
    auto_matched = if (is.null(fx$buckets$matched)) 0L else nrow(fx$buckets$matched),
    needs_review = if (is.null(fx$buckets$review)) 0L else nrow(fx$buckets$review),
    no_match     = if (is.null(fx$buckets$none)) 0L else nrow(fx$buckets$none)
  )
  print(counts)
  message("API: ", .api_flavour())

  # ── Phase A: persist the ingestion batch's source tables once ──────────────
  t_sources <- .time_only(.api_persist(
    conn, batch_id, fx$vitek_raw, fx$vitek_ast, fx$specimens))
  src_after_load <- .source_row_counts(conn)

  # ── Phase B: single confirmations ─────────────────────────────────────────
  per_confirmation <- numeric(n)
  for (i in seq_len(n)) {
    row_i <- rows[i, , drop = FALSE]
    if (split_api) {
      per_confirmation[i] <- .time_only(.api_confirm(conn, row_i, batch_id))
    } else {
      per_confirmation[i] <- .time_only(commit_matched_links(
        conn, matched = row_i, batch_id = batch_id,
        vitek_raw = fx$vitek_raw, vitek_ast = fx$vitek_ast,
        vitek_unique = fx$vitek_unique, specimens = fx$specimens,
        output_dir = out_dir, formats = c("csv", "xlsx", "duckdb"),
        match_method = "manual_selected", created_by = "bench",
        rationale = "benchmark"
      ))
    }
  }
  src_after_confirm <- .source_row_counts(conn)

  # ── Phase C: one full rebuild + export ────────────────────────────────────
  links_confirmed <- read_table(conn, "links_confirmed")
  overrides <- read_table(conn, "cleaned_overrides")

  t_read <- .time_only({
    links_confirmed <- read_table(conn, "links_confirmed")
    overrides <- read_table(conn, "cleaned_overrides")
  })
  built <- .elapsed(build_cleaned(links = links_confirmed, overrides = overrides,
                                  vitek = fx$vitek_unique, specimens = fx$specimens))
  cleaned_links <- built$value
  built_ast <- .elapsed(build_cleaned_ast(cleaned_links, fx$vitek_ast))
  cleaned_ast <- built_ast$value
  built_spec <- .elapsed(build_specimen_dataset(cleaned_links, fx$specimens))

  # Marginal cost of each output format, with the specimen dataset supplied so
  # the hierarchy resolution above is not charged to every format.
  .format_only <- function(fmt, use_conn = NULL) {
    .time_only(export_cleaned_dataset(
      cleaned = cleaned_links, cleaned_ast = cleaned_ast, batch_id = batch_id,
      specimens = fx$specimens, specimen_dataset = built_spec$value,
      output_dir = out_dir, formats = fmt, conn = use_conn))
  }
  supports_prebuilt <- "specimen_dataset" %in% names(formals(export_cleaned_dataset))
  if (supports_prebuilt) {
    t_csv <- .format_only("csv")
    t_xlsx <- .format_only("xlsx")
    t_duck <- .format_only("duckdb", conn)
  } else {
    # Pre-change export_cleaned_dataset() rebuilds the specimen dataset itself,
    # so each format pays for it again.
    t_csv <- .time_only(export_cleaned_dataset(
      cleaned = cleaned_links, cleaned_ast = cleaned_ast, batch_id = batch_id,
      specimens = fx$specimens, output_dir = out_dir, formats = "csv", conn = NULL))
    t_xlsx <- .time_only(export_cleaned_dataset(
      cleaned = cleaned_links, cleaned_ast = cleaned_ast, batch_id = batch_id,
      specimens = fx$specimens, output_dir = out_dir, formats = "xlsx", conn = NULL))
    t_duck <- .time_only(export_cleaned_dataset(
      cleaned = cleaned_links, cleaned_ast = cleaned_ast, batch_id = batch_id,
      specimens = fx$specimens, output_dir = out_dir, formats = "duckdb", conn = conn))
  }

  # The whole Phase C action, measured as the application actually runs it.
  if (split_api) {
    t_full_export <- .time_only(.api_export(
      conn, batch_id, fx$vitek_unique, fx$vitek_ast, fx$specimens,
      out_dir, c("csv", "xlsx", "duckdb")))
  } else {
    t_full_export <- .time_only({
      lc <- read_table(conn, "links_confirmed")
      ov <- read_table(conn, "cleaned_overrides")
      cl <- build_cleaned(links = lc, overrides = ov,
                          vitek = fx$vitek_unique, specimens = fx$specimens)
      ca <- build_cleaned_ast(cl, fx$vitek_ast)
      build_specimen_dataset(cl, fx$specimens)
      export_cleaned_dataset(
        cleaned = cl, cleaned_ast = ca, batch_id = batch_id,
        specimens = fx$specimens, output_dir = out_dir,
        formats = c("csv", "xlsx", "duckdb"), conn = conn)
    })
  }

  timings <- tibble::tibble(
    stage = c(
      "phase_a_persist_sources_once",
      "confirm_min", "confirm_median", "confirm_mean", "confirm_max",
      "export_read_links_and_overrides",
      "export_build_cleaned_links",
      "export_build_cleaned_ast",
      "export_build_specimen_dataset",
      "export_write_csv",
      "export_write_xlsx",
      "export_write_duckdb",
      "export_total"
    ),
    seconds = c(
      t_sources,
      min(per_confirmation), stats::median(per_confirmation),
      mean(per_confirmation), max(per_confirmation),
      t_read, built$seconds, built_ast$seconds, built_spec$seconds,
      t_csv, t_xlsx, t_duck, t_full_export
    )
  )

  message("\n== Per-confirmation seconds ==")
  print(round(per_confirmation, 3))
  message("\n== Timings (seconds) ==")
  print(as.data.frame(timings |> dplyr::mutate(seconds = round(.data$seconds, 3))))

  message("\n== Source-table row counts ==")
  print(rbind(after_batch_load = src_after_load,
              after_confirmations = src_after_confirm))
  message("Rows added to source tables by ", n, " confirmations: ",
          sum(src_after_confirm - src_after_load))

  message("\n== Cleaned output sizes (rows) ==")
  print(c(cleaned_links = nrow(cleaned_links),
          cleaned_ast = nrow(cleaned_ast),
          specimen_dataset = nrow(built_spec$value),
          confirmed_links = nrow(links_confirmed)))

  results <- list(
    api = .api_flavour(),
    counts = counts,
    per_confirmation = per_confirmation,
    timings = timings,
    source_rows_after_load = src_after_load,
    source_rows_after_confirmations = src_after_confirm
  )
  out_csv <- file.path(tempdir(),
                       paste0("axis_bench_", results$api, ".csv"))
  readr::write_csv(timings, out_csv)
  message("\nTimings written to: ", out_csv)
  invisible(results)
}

if (!interactive() && identical(environment(), globalenv())) {
  bench_run()
}
