# Undoing a confirmation, and why confirmed isolates kept coming back

Three defects, one root cause between two of them: AXIS treated a confirmed
link as an event rather than as a state. Nothing consulted it afterwards.

## 1. "Revert" never removed a link — and there was no way to remove one

`btn_revert` clears unsaved field edits in the reconcile panel. It has never
touched `links_confirmed`. There was no undo for a confirmation at all, so a
manual link to the wrong OpenSpecimen record was permanent.

**Fixed.** The button is now labelled **Discard edits**, which is what it does.
A separate **Remove link…** action sits on the detail rail for any confirmed
link. It opens a modal naming the isolate and the specimen, takes an optional
reason, and calls `retract_links()`.

`retract_links()` deletes the row from `links_confirmed` rather than flagging
it, and records the removal in `edit_log` as `link.retracted` with the
`os_identifier` that was removed and who removed it. Deleting matters: a
flagged row would still satisfy the duplicate check in `insert_new_links()`
and would silently refuse a corrected re-confirmation of the same pairing.
Field overrides keyed to the dead `link_id` go with it, so they cannot
reattach to a later link. Rows in the `cleaned_*` export snapshots are dropped
too, so the Inventory tab stops showing the isolate immediately rather than at
the next export.

## 2. Confirmed isolates reappeared in the review queue after a re-run

`bucket_results()` had no knowledge of `links_confirmed`. Re-running automerge
regenerated candidates for every isolate, including settled ones.
`drop_committed_from_buckets()` removed them in the live session only, and a
re-run overwrote that.

**Fixed.** `bucket_results()` takes `links_confirmed` and withholds those
isolates from `matched`, `review` and `none`, reporting them in a fourth
element, `$confirmed`, so the counts still sum to the batch. `run_match()`
reads confirmed links straight from DuckDB rather than trusting
`app_state$links_confirmed`, which the Linking tab populates lazily and which
is therefore empty if automerge is re-run before that tab is ever opened.

Measured on the current batch — 1,936 isolates, 11,540 specimens:

| | matched | review | no-match | confirmed |
|---|---|---|---|---|
| Before any confirmations | 751 | 232 | 953 | 0 |
| After confirming the auto-matches, re-run | 0 | 232 | 953 | 751 |

All 751 confirmed isolates were previously re-offered by a re-run. None are
now. The 232 needs-review and 953 no-match isolates are untouched.

## 3. Confirming a second specimen for one isolate silently kept both

This is the damage the first two defects caused together. `insert_new_links()`
deduplicated on `(lab_id, isolate_number, os_identifier)`, so a second, different
specimen for an already-linked isolate was not a duplicate — it was appended.
`dedup_confirmed_links()` used the same key, so both survived into the cleaned
export: one isolate, two rows, two different specimens, nothing marking it.

An analyst reached this by mis-clicking in Manual Link, or simply by confirming
an isolate that defect 2 had put back in the queue.

**Fixed** in three places:

- `insert_new_links()` withholds a link whose isolate already has a different
  confirmed specimen and returns it as a conflict. Re-confirming the *identical*
  pairing is still a silent no-op — that is a duplicate, not a conflict.
- `confirm_links()` returns `n_conflicted` / `conflicted`; both commit paths
  surface it: *"N isolates are already linked to a different OpenSpecimen record
  and were not changed."* A withheld confirmation that said nothing would look
  like a click that missed.
- `dedup_confirmed_links()` is now keyed on the isolate, so a database written
  by an older AXIS exports the most recent link only.
  `superseded_isolate_links()` returns the older rows, and the Linking tab
  reports them on load so they can be cleared rather than silently resolved.

## Correcting a wrong link

1. Open the link in the Linking tab.
2. **Remove link…**, give a reason, confirm.
3. The isolate returns to the no-match bucket, where **Manual link…** is
   available. Re-running automerge restores its candidates.
4. **Rebuild and export cleaned data** when the review is done — the export is
   stale until then, and AXIS marks it so.

## Tests

`tests/testthat/test-retract-link.R` (44 assertions) and four module-level
tests in `test-linking-workflow.R`, all on synthetic records. They cover the
round trip that matters: confirm, re-run automerge, retract, re-run again, and
confirm the corrected specimen.
