# AXIS handoff: Cedar linking follow-up

## Purpose

This document defines the next AXIS changes prompted by analyst review of the
Manual Link and needs-review workflows. It is the shared implementation brief
for parallel work by Codex and Claude Cowork.

The goals are to:

1. link SNT VITEK records directly to the correct APPS or APPS 2 OpenSpecimen
   record when the studies overlap;
2. preserve original VITEK values while allowing audited corrections in the
   cleaned dataset; and
3. make manual confirmations fast by separating confirmation from the slower
   full-dataset export.

Do not commit private source exports, patient identifiers, local DuckDB files,
or generated specimen-level output to GitHub.

## Starting point

- Repository: `https://github.com/michaelwoodworth/axis`
- Manual Link fix: PR #12, merged into `main`
- Merge commit: `6f64439`
- Implementation work must start from `main` after this handoff document is
  merged.

### Manual Link fix already completed

PR #12 fixed the original dropdown problem in `R/mod_linking.R`.

Before the fix, Manual Link reused the automatic matcher's rule that excludes
ordinary OpenSpecimen aliquots. That hid valid SNT records from the analyst even
though the records were loaded. Cryopreserved Cells records remained visible,
which made the dropdown appear to contain only certain cohorts.

The merged behavior is:

- Manual Link shows every loaded OpenSpecimen record with a nonblank
  `os_identifier`, including ordinary aliquots.
- Automatic matching remains conservative and continues to exclude ordinary
  banked aliquots by default.
- `tests/testthat/test-linking.R` covers ordinary aliquots, Cryopreserved Cells,
  blank identifiers, and unavailable specimen data.

Do not undo this distinction between analyst-directed Manual Link and automatic
matching.

## Analyst questions and agreed behavior

### 1. SNT visits recorded under APPS or APPS 2

Some visits identified as SNT were recorded in an APPS OpenSpecimen collection
protocol, including the second APPS round. Once the appropriate Cryopreserved
Cells record is entered in OpenSpecimen, AXIS should link the SNT VITEK record
directly to that APPS OpenSpecimen record.

The same biological record must not be entered a second time under an SNT
collection protocol solely to make AXIS match it.

Required behavior:

- Treat SNT, Sentinel, APPS, APPS 2, and REACT protocol titles as members of a
  recognized SNT/APPS/REACT family for candidate selection.
- Accept punctuation, spaces, underscores, and case differences in known APPS
  round names. For example, `APPS 2`, `APPS_2`, and `APPS _2` should have the
  same family classification.
- Continue to use specimen label, participant, MDRO category, organism, date,
  and specimen type when scoring a candidate. A shared study family alone must
  never be enough to create an automatic match.
- Do not treat FAIR, ARRRRG, Pre-Alert, MEPSD, or an unknown collection protocol
  as part of this family.
- If more than one plausible record remains, send the record to needs-review
  rather than silently choosing an uncertain match.

### Current limitation

`R/data_parse_labid.R` assigns SNT, APPS, REACT, and related VITEK identifiers a
`cp_hint` of `SNT/APPS/React`.

`R/data_match.R::.cp_titles_overlap()` currently removes punctuation and then
checks whether one complete protocol title contains the other. This does not
recognize `SNT/APPS/React` and `APPS _2` as the same family. In addition,
`.score_one_vitek()` narrows candidates to matching collection protocols when
any match is found, so a valid APPS 2 record can be excluded when another
apparently matching protocol is loaded.

### Suggested implementation direction

Add one explicit, tested protocol-family normalization function rather than a
collection of one-off string replacements. Both candidate filtering and the
collection-protocol score should use the same function.

A reasonable result would classify known titles into stable internal values
such as `SNT_APPS_REACT`, while leaving unrelated and unknown titles distinct.
The implementation may use a different internal name if its behavior is clear
and tested.

## VITEK ID typo corrections and source preservation

Original VITEK imports are source records. AXIS must not silently overwrite
them. A typo such as `1o1` instead of `1of1` should be handled as follows:

1. Use Manual Link to connect the original VITEK record to the correct
   OpenSpecimen record.
2. After the link is confirmed, use Edit mode to enter the corrected cleaned
   lab ID.
3. Save an audit reason explaining the correction.

Expected data behavior:

- `vitek_raw` and the original `lab_id` remain unchanged.
- The correction is stored in `cleaned_overrides` for that confirmed `link_id`.
- The before and after values, analyst, and timestamp are stored in `edit_log`.
- `build_cleaned()` uses the latest override for `clean_lab_id` while retaining
  the original VITEK value in the linked output.
- Rebuilding or exporting the cleaned dataset does not remove the override.

Edit mode is not a substitute for linking. A VITEK no-match must first be
connected through Manual Link; edits to an unconfirmed staged record remain
disabled.

Silent source correction or automatic replacement of lookalike characters is
out of scope. A future fuzzy-match enhancement may suggest likely typo matches,
but it must not change raw values or automatically confirm an uncertain link.

## Why confirmation currently takes about two minutes

The approximately two-minute delay is analyst-reported and must be measured on
a representative dataset before and after changes. Code inspection identifies
a clear source of avoidable work.

Both **Confirm selected** and **Manual link** call
`R/mod_linking.R::commit_link_rows()`, which calls
`R/store.R::commit_matched_links()` synchronously. For every single
confirmation, that function currently:

1. writes the confirmed link and its audit event;
2. appends the loaded VITEK, AST, and OpenSpecimen source rows again;
3. reads all confirmed links and overrides;
4. rebuilds the complete cleaned link dataset;
5. rebuilds the complete linked AST dataset;
6. rebuilds the complete specimen dataset;
7. rewrites three CSV files;
8. rewrites the XLSX workbook; and
9. replaces the current batch rows in three DuckDB cleaned-output tables.

Most of this work grows with the size of the entire loaded dataset, not with the
single link being confirmed. It also runs in the Shiny session, which prevents
the interface from responding until the work finishes.

Re-appending source tables on every confirmation is unnecessary and can produce
duplicate source rows for the same batch. Source persistence must occur once per
ingestion batch, not once per manual decision.

Reviewing one cohort at a time may reduce the amount of work temporarily, but it
is not the desired solution. Analysts should be able to keep all relevant
cohorts loaded and produce one cohesive cleaned dataset.

## Planned confirmation and export workflow

Separate a quick confirmation operation from a deliberate full export.

### Phase A: load and prepare the ingestion batch

- Parse VITEK and OpenSpecimen inputs.
- Run automatic matching.
- Persist each source table once for the batch.
- Do not append the same batch's source rows again during later confirmations.
- Preserve the existing rule that only one process writes to a given DuckDB
  file at a time.

If source persistence is retried after a partial failure, it must be safe and
must not duplicate rows already written for that batch.

### Phase B: review and confirm links

For **Confirm selected** and **Manual link**:

- validate the selected VITEK and OpenSpecimen records;
- append only a new confirmed logical link when it does not already exist;
- append the manual-confirmation audit event;
- update the in-memory review buckets and visible link state;
- mark cleaned outputs as needing refresh; and
- return control to the analyst without rebuilding CSV, XLSX, or cleaned
  DuckDB output tables.

A repeated click or retry must not create duplicate logical links or duplicate
audit events for a link that was not newly inserted.

The interface must clearly show when confirmations have been saved but the
cleaned export has not yet been refreshed.

### Phase C: finalize and export

Provide one explicit action after review, using plain language such as
**Rebuild and export cleaned data**.

That action should:

- read the complete set of confirmed links and latest overrides;
- rebuild the cleaned link, AST, and specimen datasets once;
- write CSV, XLSX, and DuckDB cleaned outputs once;
- update the application state used by Inventory and other downstream panels;
- clear the "needs export" indicator only after all requested outputs succeed;
  and
- show a busy message or progress indicator while the full rebuild is running.

If an export fails, confirmed links and audit events must remain saved, the
"needs export" state must remain visible, and the analyst must be able to retry
without reconfirming records.

### Code organization expectation

`commit_matched_links()` currently combines source persistence, link
confirmation, cleaned-data rebuilding, and file export. Split these
responsibilities into clearly named functions. Exact function names are up to
the implementer, but the code should make these operations independently
testable:

- persist a source batch once;
- record one or more confirmed links and audit events;
- rebuild cleaned data in memory; and
- write final output formats.

Avoid a flag-heavy function where one Boolean changes many unrelated side
effects. Keep existing public behavior working where it is still used, or
migrate every caller and test in the same change.

## Testing requirements

All tests must use synthetic records. Do not copy analyst exports or local
DuckDB contents into test fixtures.

### SNT/APPS correctness tests

Add or extend tests in `tests/testthat/test-match.R` and, when appropriate,
`tests/testthat/test-parse.R`.

Required cases:

- `SNT/APPS/React` overlaps with `APPS 2`, `APPS_2`, and `APPS _2`.
- Matching is case-insensitive for known family names.
- An SNT VITEK record can select a strongly matching APPS 2 Cryopreserved Cells
  record without a duplicate SNT OpenSpecimen record.
- A FAIR, ARRRRG, Pre-Alert, or MEPSD record is not treated as an SNT/APPS/REACT
  family match.
- A same-family but wrong participant or specimen label is not automatically
  accepted merely because the collection protocols are related.
- Multiple similarly scored candidates remain in needs-review.
- Existing automatic-match tests continue to pass.

### Typo and provenance tests

Add coverage in `tests/testthat/test-clean.R`, `tests/testthat/test-store.R`, or a
focused new test file.

Required cases:

- A manually linked typo retains the original VITEK `lab_id`.
- An Edit mode override produces the corrected `clean_lab_id`.
- The edit log contains the before value, after value, analyst, and timestamp.
- Rebuilding the cleaned output preserves the correction.
- Source data are not modified by saving or reverting a cleaned override.

### Confirmation persistence tests

Required cases:

- One confirmation inserts one logical link.
- Repeating the same confirmation does not insert a duplicate logical link.
- A manual confirmation creates one audit event only when a new link is saved.
- Confirming a link does not append VITEK, AST, or specimen source rows again.
- Confirming a link does not call CSV or XLSX export code.
- Several confirmations can be saved before one final rebuild.
- Confirmed links survive reopening the DuckDB connection before export.
- A failed final export does not remove confirmed links or audit records.
- A successful final export matches the cleaned content produced by the current
  pipeline for the same confirmed links and overrides.

### Full validation

From the repository root, parse every R source file:

```bash
Rscript -e 'invisible(lapply(list.files("R", "[.]R$", full.names=TRUE), parse)); message("R source files parse")'
```

Run the complete automated test suite:

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
```

When private example exports are available locally, point tests to them without
copying them into the repository:

```bash
Rscript -e 'Sys.setenv(AXIS_TEST_DATA_DIR="/absolute/path/to/01.data"); testthat::test_dir("tests/testthat")'
```

Record pass, fail, warning, and skip counts in each pull request.

## Performance requirements

Measure performance; do not report that it is improved based only on code
inspection.

### Baseline

Before changing the confirmation path:

- record non-sensitive row counts for VITEK records, AST rows, OpenSpecimen
  records, and confirmed links;
- time at least five representative single confirmations;
- separately time the final CSV, XLSX, and DuckDB rebuild; and
- identify how much time is spent persisting links, writing source rows,
  rebuilding cleaned tables, and writing each output format.

Do not place specimen identifiers or source data in benchmark output committed
to GitHub.

### Acceptance targets

On the same machine and representative dataset used for the baseline:

- a typical single confirmation should complete within 2 seconds;
- no measured single confirmation should exceed 5 seconds without a documented
  environmental reason;
- confirming 20 records must not trigger 20 full exports;
- source-table row counts must remain unchanged across manual confirmations;
- the full export may take longer, but it must run once on explicit request and
  keep the interface visibly busy rather than appearing frozen; and
- final output row counts and values must be equivalent to the pre-change
  pipeline for the same inputs, confirmations, and overrides.

If the two-second target cannot be reached, report the measured bottleneck and
the next recommended change rather than weakening the test silently.

## Parallel work plan

Both agents must start from the same `main` commit that includes this handoff.
They must use separate clones or Git worktrees and separate DuckDB files.

### Workstream A: Codex — SNT/APPS matching correctness

Suggested branch:

```text
codex/snt-apps-linking
```

Primary ownership:

- `R/data_match.R`
- `R/data_parse_labid.R`, only if parsing changes are necessary
- `tests/testthat/test-match.R`
- `tests/testthat/test-parse.R`, only if parsing changes are necessary

Deliverables:

- documented protocol-family normalization;
- SNT-to-APPS 2 matching behavior;
- protection against unrelated cross-cohort matches;
- focused regression tests; and
- a pull request describing assumptions and unresolved data questions.

Codex should not redesign the confirmation/export workflow in this branch.

### Workstream B: Claude Cowork — confirmation performance

Suggested branch:

```text
claude/link-confirm-performance
```

Primary ownership:

- `R/store.R`
- `R/mod_linking.R`
- `R/data_clean.R`, only where needed to separate rebuilding from persistence
- `app.R`, only if shared application state needs a "needs export" value
- `tests/testthat/test-store.R`
- `tests/testthat/test-linking.R`
- new focused performance or workflow tests

Deliverables:

- measured baseline with non-sensitive row counts;
- lightweight confirmation persistence;
- one-time source persistence per batch;
- explicit final rebuild/export action;
- clear saved-versus-exported UI state;
- failure and retry behavior;
- automated persistence and equivalence tests; and
- before/after timing results in the pull request.

Claude should not change SNT/APPS protocol-family rules in this branch.

## Cross-review process

Neither agent should merge its own implementation without review.

1. Codex opens the SNT/APPS matching pull request.
2. Claude reviews it for overly broad family matching, ambiguous candidate
   handling, and missing edge cases.
3. Claude opens the confirmation-performance pull request.
4. Codex reviews it for duplicate writes, audit correctness, failure recovery,
   export equivalence, and adequate tests.
5. Each author addresses review findings on its own branch.
6. Merge the matching pull request first because its primary files are separate
   from the persistence work.
7. Rebase or merge updated `main` into the performance branch, rerun the full
   suite, and then merge the performance pull request.
8. From a fresh clone of the final `main`, rerun parsing, automated tests, and a
   manual end-to-end review with synthetic or approved local example data.

Cross-review should compare code and behavior, not merely confirm that tests
pass. Reviewers should explicitly list any remaining correctness, performance,
or data-provenance risks.

## Definition of done

The combined work is complete only when all of the following are true:

- The merged Manual Link behavior remains intact.
- SNT VITEK records can link directly to appropriate APPS 2 OpenSpecimen
  Cryopreserved Cells records.
- No duplicate SNT OpenSpecimen entry is required.
- Unrelated collection protocols are not treated as the same family.
- Original VITEK IDs remain unchanged and available for audit.
- Corrected cleaned IDs persist through rebuilding and export.
- Manual confirmations are saved quickly without a full export per click.
- Source rows are written once per batch and do not multiply during review.
- Analysts can review all cohorts together and run one cohesive final export.
- Export failures are recoverable without losing confirmed work.
- Performance results are measured and documented.
- All R files parse and the complete automated test suite passes, apart from
  clearly explained environment-dependent skips.
- Both implementation pull requests receive cross-review.

