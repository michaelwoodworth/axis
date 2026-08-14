# Confirmation and export performance — measured results

Workstream B of `docs/HANDOFF_CEDAR_LINKING_2026-08.md`.

All numbers are wall-clock seconds measured with `inst/bench/bench_confirm.R`,
on one machine, with one ingestion batch loaded and every cohort present. The
benchmark prints row counts and elapsed seconds only; no specimen identifier,
participant identifier, or source value appears in it or in this document.

Before and after were measured on the same machine, from the same cached parsed
fixture, with the same five confirmations, using the same script. "Before" was
run from `main` at `6f64439`; "after" from this branch.

## How to reproduce

```bash
AXIS_TEST_DATA_DIR=/absolute/path/to/01.data Rscript inst/bench/bench_confirm.R
```

`AXIS_BENCH_N` sets how many single confirmations are timed (default 8; the runs
below used 5, the handoff minimum). `AXIS_BENCH_CACHE` points at a cached parsed
fixture so repeat runs skip parsing and matching. With no `AXIS_TEST_DATA_DIR`
the script generates a synthetic dataset of comparable shape, so it always runs,
but only the real batch is worth quoting.

## Dataset

| Table | Rows |
|---|---|
| `vitek_raw` | 2,750 |
| `vitek_ast` | 93,039 |
| `vitek_unique` | 1,936 |
| `specimens` | 11,540 |
| auto-matched candidates | 756 |
| needs-review candidates | 575 |
| no-match Vitek records | 985 |

## Headline

| Measure | Before | After |
|---|---|---|
| Single confirmation, median | **234.4 s** | **0.090 s** |
| Single confirmation, min–max | 230.8 – 242.9 s | 0.081 – 0.138 s |
| Source rows added by 5 confirmations | **536,645** | **0** |
| Full rebuild + export (CSV + XLSX + DuckDB) | 126.2 s | 62.5 s |
| Exports triggered by 5 confirmations | 5 | 0 |

The analyst-reported "about two minutes" per confirmation reproduces, and is
worse than two minutes once every cohort is loaded at once: about **four
minutes** per click on this batch.

## Where the time went, before

| Stage | Seconds |
|---|---|
| Persist source tables (once) | 104.7 |
| Single confirmation (median) | 234.4 |
| Read confirmed links + overrides | 0.05 |
| Build cleaned links | 0.10 |
| Build cleaned AST | 0.10 |
| Build specimen dataset | 64.1 |
| Write CSV (incl. its own specimen rebuild) | 63.2 |
| Write XLSX (incl. its own specimen rebuild) | 64.2 |
| Write DuckDB (incl. its own specimen rebuild) | 64.9 |
| Full export as the application ran it | 126.2 |

A single confirmation was almost entirely two things that have nothing to do
with the link being confirmed:

* **~105 s** re-appending the batch's parsed source rows — `vitek_raw`,
  `vitek_ast`, and `specimens` — to the landing tables. Nearly all of that is
  `.stringify_list_cols()` serialising the two OpenSpecimen blob columns across
  11,540 specimen rows, and it was paid again on every click.
* **~128 s** rebuilding the specimen hierarchy twice: once in the commit
  function and once more inside `export_cleaned_dataset()`, which rebuilt it
  for itself.

The remainder — writing the link row, the audit event, and the actual CSV/XLSX
bytes — was under a second.

The source-row growth is the more serious half. Five confirmations added
**536,645 rows** to the source tables: `vitek_ast` went from 93,039 rows to
558,234, six identical copies of the same batch. Twenty confirmations in a
review session would have left twenty-one copies.

## Where the time goes, after

| Stage | Seconds |
|---|---|
| Persist source tables (once per batch, Phase A) | 102.7 |
| Single confirmation (median) | 0.090 |
| Read confirmed links + overrides | 0.04 |
| Build cleaned links | 0.19 |
| Build cleaned AST | 0.14 |
| Build specimen dataset | 61.1 |
| Write CSV | 0.19 |
| Write XLSX | 0.89 |
| Write DuckDB | 0.51 |
| Full rebuild + export (Phase C) | 62.5 |

## Against the acceptance targets

| Target | Result |
|---|---|
| A typical single confirmation completes within 2 s | Met — median 0.090 s |
| No measured confirmation exceeds 5 s | Met — slowest 0.138 s |
| Confirming 20 records does not trigger 20 full exports | Met — confirmations never export; 0 exports across the timed run |
| Source-table row counts unchanged across manual confirmations | Met — 0 rows added by 5 confirmations |
| Full export runs once on explicit request, interface visibly busy | Met — one `Rebuild and export cleaned data` action, guarded by the existing `axis_busy_show` overlay |
| Final output values equivalent to the pre-change pipeline | Met — `tests/testthat/test-confirm-workflow.R` reruns the pre-change sequence on a second database and compares the exported CSVs and the DuckDB `cleaned_links` table |

## Remaining bottlenecks and the next change I would make

Confirmation is done. Two costs remain, both outside the confirmation path and
both paid once:

1. **`resolve_specimen_hierarchy()` — 61 s** on 11,540 specimen records, which
   is essentially the whole cost of the final export. It walks the parent chain
   row by row. The next change I would make is to resolve the hierarchy once per
   loaded specimen set and cache it against that set, rather than per export;
   the specimens do not change between exports of the same batch. Failing that,
   the walk itself can be replaced with an iterative join over a parent-key
   index.

2. **`.stringify_list_cols()` — ~103 s** for the two OpenSpecimen blob columns,
   which is now the whole of Phase A. It calls `capture.output(str(...))` once
   per cell, about 4.5 ms each across 23,080 cells. It can be made roughly free
   by serialising only distinct blob values and mapping them back, but that
   changes stored text for those two columns if done carelessly, so it wants its
   own change with its own equivalence test rather than riding along here.

Neither blocks the acceptance targets: both are one-time per-batch costs that
the analyst pays during ingestion or during a deliberate export, not on every
review decision.

## Note on the environment

These runs were made on a Linux container with 2 cores, R 4.3.3, and duckdb
1.5.5. Absolute seconds on an analyst laptop will differ; the ratios and the
source-row counts will not. Re-running `inst/bench/bench_confirm.R` on the
review machine before and after merge is worth the twenty minutes it costs.

## Update after the head-to-head with PR #14

Re-measured on the same machine and fixture after adopting a transaction around
the link and its audit event, and after replacing the ledger's skip-if-recorded
rule with a content fingerprint:

| Measure | Before | After |
|---|---|---|
| Single confirmation, median | 234.4 s | 0.081 s |
| Single confirmation, max | 242.9 s | 0.124 s |
| Source rows added by 5 confirmations | 536,645 | 0 |
| Phase A, once per batch | 104.7 s | 71.3 s |
| Full rebuild + export | 126.2 s | 49.5 s |

Phase A fell because a re-run of automerge over unchanged data now costs a
fingerprint instead of a full rewrite.

See `docs/REVIEW_WORKSTREAM_B_COMPARISON_2026-08.md` for the comparison against
PR #14, including the two defects that review found in this branch.
