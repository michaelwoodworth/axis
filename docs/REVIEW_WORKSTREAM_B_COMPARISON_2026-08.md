# Workstream B head-to-head: PR #14 vs `claude/link-confirm-performance`

Cross-review for `docs/HANDOFF_CEDAR_LINKING_2026-08.md`.

Codex implemented both workstreams. PR #13 (`1ec6083`) is Workstream A, the
SNT/APPS matching work the handoff assigned it. PR #14 (`1d94bf3`) is
Workstream B — the same brief this branch implements. So there are two
independent implementations of the confirmation/export split and one decision
to make.

**Disclosure.** I wrote one of the two branches under comparison, so treat my
reading of the code as interested. Everything below that matters is a
measurement rather than an opinion: both branches were run on the same machine,
against the same cached parse of the same `01.data` batch, through the same
harness. The published write-ups cannot be compared directly — PR #14's numbers
come from a machine roughly three times faster than the one used here, so its
73.8 s baseline and my 234.4 s baseline describe the same code on different
hardware.

## Method

1. **Structural** — both branches diffed against the merge base `6f64439`.
2. **Neutral conformance harness** — one test file, run unchanged against both
   implementations through a thin adapter that binds whichever function names
   the branch provides. Every case traces to a bullet the handoff states
   explicitly, so neither branch's own naming or design decides the outcome.
   The harness is not in either branch; it is reproduced in an appendix below.
3. **Performance** — `inst/bench/bench_confirm.R` extended to bind either API,
   run on both branches, same machine, same cached fixture, 5 timed
   confirmations, then a 200-confirmation scaling probe.

## Dataset

`vitek_raw` 2,750 · `vitek_ast` 93,039 · `vitek_unique` 1,936 ·
`specimens` 11,540 · auto-matched 756 · needs-review 575 · no-match 985.

## Conformance

Fourteen cases: the handoff's nine confirmation-persistence requirements (R1–R9),
its typo/provenance requirement (P1), and four Phase A requirements drawn from
"Source persistence must occur once per ingestion batch" and "If source
persistence is retried after a partial failure, it must be safe and must not
duplicate rows already written for that batch" (A1–A4).

| Case | PR #14 | This branch |
|---|---|---|
| R1 one confirmation inserts one logical link | pass | pass |
| R2 repeat inserts no duplicate | pass | pass |
| R3 audit event only when a new link is saved | pass | pass |
| R4 confirming appends no source rows | pass | pass |
| R5 confirming reaches no CSV/XLSX export code | pass | pass |
| R6 several confirmations before one rebuild | pass | pass |
| R7 links survive reopening the connection | pass | pass |
| R8 failed export keeps links and audit records | pass | pass |
| R9 export matches the pre-change pipeline | pass | pass |
| P1 typo: source preserved, override wins, log complete | pass | pass |
| A1 re-persisting an unchanged batch does not duplicate | pass | pass |
| A2 partially persisted batch retries safely | pass | pass |
| A3 re-parsed batch with more rows replaces, not appends | pass | **fail, then fixed** |
| A4 re-parsed batch with the same row count is not left stale | **fail** | **fail, then fixed** |

Both branches satisfy every requirement the handoff spells out. The
disagreements are in Phase A, and each branch found a real defect in the other.

**What PR #14 found in mine (two defects, both now fixed).**

*A3/A4.* My `persist_source_batch()` skipped any table already recorded in its
ledger unless the caller passed `force = TRUE`. The application did pass it, so
the app was correct — but the function's default silently kept stale rows when
a batch was re-parsed, and correctness depended on every future caller
remembering a flag. That is the kind of thing that survives review and breaks
two years later.

*A stuck busy overlay.* The click handler in `app.R` raised the full-screen
"Committing matched rows, rebuilding cleaned tables…" overlay for
`ingestion-commit_matched` and `linking-commit_matched`. I removed the server-side
`axis_busy_hide` from both paths without touching that handler, so clicking
**Commit matched only** on either tab would have covered the screen with an
overlay that nothing takes down — an app that looks frozen, which is precisely
the symptom this work exists to remove. PR #14 repointed the handler at
`linking-rebuild_export` and got this right. I have adopted that, and added an
explicit hide on every early return from the export observer.

**What mine found in PR #14 (one defect, unfixed).**

*A4.* `persist_source_batch_once()` decides whether a batch is already stored by
comparing the row count in the table against the row count in memory:

```r
if (identical(as.numeric(existing_n), as.numeric(nrow(tbl)))) next
```

A re-parse that changes content without changing row count is a no-op. The
database keeps the superseded rows, the application shows the corrected ones,
and nothing reports a discrepancy. Concretely: an analyst re-exports an
OpenSpecimen CSV after fixing an organism or an MDRO flag on existing records,
re-runs automerge, and the `specimens` landing table still holds the old values
— while `build_cleaned()` runs off the corrected in-memory copy. Cleaned output
and stored source then disagree, which is a data-provenance failure in a system
whose stated purpose is to keep source records auditable.

This branch now fingerprints each parsed table (`rlang::hash`, measured at
0.185 s across all three tables of this batch) and rewrites when the
fingerprint, the recorded row count, or the row count actually present in the
table disagrees with what is in memory. The fingerprint is taken before staging,
so a genuine no-op does not pay the specimen serialisation cost.

**Recommended fix for PR #14** if it is the branch you keep: replace the
row-count comparison with a content fingerprint, and keep the row-count check as
a cheap pre-filter.

## Performance

Same machine, same cached fixture, five timed confirmations.

| Measure | PR #14 | This branch |
|---|---|---|
| Confirmation, median | 0.101 s | 0.081 s |
| Confirmation, max | 0.264 s | 0.124 s |
| Source rows added by 5 confirmations | 0 | 0 |
| Phase A, once per batch | 75.3 s | 71.3 s |
| Full rebuild + export | 48.2 s | 49.5–54.1 s |

Scaling probe, 200 consecutive confirmations:

| | first 20, median | last 20, median | growth |
|---|---|---|---|
| PR #14 | 0.0890 s | 0.0970 s | 1.09× |
| This branch | 0.0725 s | 0.0800 s | 1.10× |

**Performance is a tie.** Both are roughly twenty times inside the two-second
target and neither degrades meaningfully over a long review session. PR #14
re-reads the whole `links_confirmed` table and the whole `edit_log` on every
confirmation where this branch appends to in-memory state; that shows up as
about 20 ms per click and does not grow dangerously at this scale, but it is
work that does not need doing. Run-to-run variation on this container is larger
than the gap between the branches. **Do not choose on these numbers.**

The one number worth acting on is shared: `resolve_specimen_hierarchy()` is
about 47 s of the roughly 49 s export on both branches. That is the next
performance change either way.

## Other differences

| | PR #14 | This branch |
|---|---|---|
| Link + audit atomicity | one transaction | now one transaction, adopted from PR #14 |
| Phase A staleness detection | row count | content fingerprint |
| Confirm-path database reads | two full table reads | none |
| Saved-vs-exported indicator | binary pending/current | count of unexported changes, plus a distinct failed state |
| Bulk commit and the review buckets | clears `matched` only | removes confirmed isolates from `matched`, `review`, and `none` |
| `write_ingested_tables()` | aliased to the replace-once function | kept append-only, superseded by `persist_source_batch()` |
| Benchmark harness | not committed | `inst/bench/bench_confirm.R`, reproducible with `AXIS_TEST_DATA_DIR` |
| Module-level tests | none | `shiny::testServer` coverage of confirm, export, failed export, double-confirm |
| Tests passing | 160 | 249 |

Two of these deserve comment rather than a row in a table.

**`write_ingested_tables()` in PR #14** is now an alias for
`persist_source_batch_once()`. Its documented contract said "These tables are
append-only landing tables" and that comment is still above the function; the
behaviour is now replace-once. Any caller that legitimately appended two
different parsed sets under one batch id would now silently lose the first. I
found no such caller in the repository, so this is a latent hazard rather than a
live bug, but the stale docstring should go.

**Bucket cleanup.** After a bulk commit PR #14 empties the `matched` bucket but
leaves those isolates in `review` and `none`. A confirmed isolate can therefore
still appear as an unresolved candidate elsewhere in the interface. This branch
anti-joins confirmed keys out of all three buckets. Neither behaviour is
specified by the handoff; I think removing from all three is right.

## Recommendation

Take this branch, with two carries from PR #14 — the transaction around the
link and its audit event, and the busy-overlay fix — both already applied here.
The deciding factors are the A4 provenance gap, which is unfixed in PR #14, and
test coverage: 249 passing against 160, including module-level tests that drive
the actual Shiny observers rather than the persistence functions alone.

If you prefer PR #14 for its smaller diff — 534 added lines against 1,949, and
about two-thirds of my difference is tests and the benchmark harness — then it
needs the fingerprint fix, the stale docstring removed, and I would port the
conformance harness and the `testServer` tests across before merging.

What I would not do is merge both. They occupy the same functions in the same
four files; the merge conflict is the whole change.

## Remaining risks, this branch

- `resolve_specimen_hierarchy()` at ~47 s dominates the export. One-time and
  deliberate, but it is what an analyst waits for.
- `.stringify_list_cols()` at ~70 s is now the whole of Phase A. It calls
  `capture.output(str(...))` once per cell. It can be made nearly free by
  serialising distinct blob values only, but that changes stored text for two
  columns and wants its own change with its own equivalence test.
- Inventory and downstream panels show the previous export until the analyst
  presses **Rebuild and export cleaned data**. Intended, and the indicator says
  so, but it is a real change in what those panels mean.
- The equivalence test compares against the pre-change pipeline reconstructed
  inside the test. If the pre-change pipeline was itself wrong about something,
  both sides are wrong together.

## Remaining risks, PR #14

- A4, above: source and cleaned data can disagree after a same-sized re-parse.
- Two full table reads per confirmation; harmless now, worth watching if
  `links_confirmed` grows into the hundreds of thousands.
- No committed benchmark, so the performance claim in
  `docs/link-confirm-performance.md` cannot be re-run by a reviewer.
- Confirmed isolates can linger in the `review` and `none` buckets after a bulk
  commit.
