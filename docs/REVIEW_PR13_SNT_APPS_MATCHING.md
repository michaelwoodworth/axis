# Review of PR #13 — SNT/APPS protocol family matching

Step 2 of the cross-review process in `docs/HANDOFF_CEDAR_LINKING_2026-08.md`.
Reviewed at `1ec6083` against the merge base `6f64439`. Written to be pasted as
a PR review.

## Summary

The change does what the handoff asked and does it in the shape the handoff
suggested: one named, tested classification function, used by both candidate
filtering and the collection-protocol score, with unknown titles left
deliberately unclassified. All seven required matching cases are covered by
tests and all pass. The ambiguity margin in `bucket_results()` is a good
addition that the handoff called for and that I would have missed.

Two things to resolve before merge: the family list is an exact-match allowlist
that already excludes at least one protocol title this lab uses, and the change
cannot be validated end to end on the current exports — which is worth stating
in the PR rather than discovered later.

## Verified

`.protocol_family()` classifies as the handoff requires. Checked directly:

| Title | Normalised | Family |
|---|---|---|
| `SNT/APPS/React` | `SNTAPPSREACT` | `SNT_APPS_REACT` |
| `APPS 2`, `APPS_2`, `APPS _2`, `apps 2` | `APPS2` | `SNT_APPS_REACT` |
| `SNT`, `Sentinel`, `APPS`, `REACT` | — | `SNT_APPS_REACT` |
| `FAIR 618`, `ARRRRG 2.0`, `Pre-Alert`, `MEPSD`, `Unknown Study` | — | `NA` |

Case-insensitivity holds — `.norm_cp_title()` upcases before stripping
punctuation, so the `[^A-Z0-9]` character class is safe. I checked this
specifically because stripping before upcasing would have silently eaten every
lowercase letter, and the test uses `"react"`, which would have caught it.

The early return in `.cp_titles_overlap()` is the right call:

```r
if (!is.na(hf) || !is.na(cf)) {
  return(!is.na(hf) && !is.na(cf) && hf == cf)
}
```

Once either side is a recognised family member, an unrecognised counterpart is
rejected outright rather than falling back to substring containment. That is
what stops `Pre-Alert REACT` from matching an SNT hint, and I confirmed it does.

`bucket_results()`'s `ambiguity_margin` implements "If more than one plausible
record remains, send the record to needs-review rather than silently choosing an
uncertain match." Measured on the current batch: 26 isolates move from
auto-matched to needs-review (756 → 730), no isolate moves the other way. A 3.4%
increase in review volume for the removal of a silent row-order tiebreak is a
good trade.

## Finding 1 — the family list is an exact-match allowlist, and it already has a hole

`.protocol_family()` matches `norm %in% c("SNT","SENTINEL","APPS","REACT","SNTAPPSREACT")`
or `^APPS[0-9]+$`. Anything else is `NA`, and by the early return above, `NA`
against a known family is a hard `FALSE`. Titles that classify as unrelated
today:

| Title | Family | Overlaps `SNT/APPS/React`? |
|---|---|---|
| `Sentinel-REACT` | `NA` | FALSE |
| `Sentinel REACT` | `NA` | FALSE |
| `APPS II` | `NA` | FALSE |
| `APPS Round 2` | `NA` | FALSE |
| `REACT Extension` | `NA` | FALSE |

`Sentinel-REACT` is the one that concerns me: it is a cohort name already in use
in this group's analysis pipelines. If OpenSpecimen ever carries a protocol under
that title, an SNT Vitek record will not match it, silently, and the failure will
look exactly like the bug this PR was written to fix — a valid record that the
matcher refuses to see.

Note also that this is stricter than `main` for these titles. Under the old
substring rule `REACT` ⊂ `REACTEXTENSION` was an overlap; now it is not. That is
defensible and arguably intended by "leaving unrelated and unknown titles
distinct", but it is a behaviour change beyond the family list and it is not
called out in the PR description.

Suggested change: classify on a leading family token rather than exact equality,
so `APPS` anything and `SENTINEL` anything land in the family, and add
`SENTINELREACT` explicitly. Something like:

```r
known <- grepl("^(SNT|SENTINEL|APPS|REACT)([0-9]+|[IVX]+)?$", norm) |
         norm %in% c("SNTAPPSREACT", "SENTINELREACT")
```

Whatever form it takes, please add the real protocol titles from the current
OpenSpecimen exports to the test as a guard, so a future title change fails a
test rather than a match.

## Finding 2 — the change cannot be validated on the current exports

I re-ran `auto_match()` on the real batch under this branch and under `main`.
They produce **identical candidate rows** (2,213 both) and **zero** newly
auto-matched isolates. The no-match bucket is unchanged at 985 isolates. The
only measurable difference on this data is the 26-isolate ambiguity shift.

The reason is benign: in these exports the OpenSpecimen protocol is titled
`SNT/APPS/React`, character-for-character the same as the Vitek `cp_hint`, so
the old substring rule already matched it. There is no `APPS 2` protocol in the
loaded data at all — consistent with the handoff's premise that the Cryopreserved
Cells records still have to be entered.

This is not a defect in the PR. But it means the synthetic tests are the only
evidence the fix works, and nobody should read "no isolates moved out of
no-match" as the change having failed. Please say so in the PR description, and
if an APPS 2 record can be entered in OpenSpecimen before merge, re-run against
it — that is the only real confirmation available.

## Finding 3 — minor, ambiguity counting assumes unique candidates

```r
similarly_scored = sum(score >= best_score - ambiguity_margin, na.rm = TRUE)
```

counts candidate rows, not distinct specimens. If `auto_match()` ever emits two
rows for the same `(lab_id, isolate_number, os_identifier)` — a duplicated
`os_identifier` in the specimens table would do it — a single candidate would be
counted twice and the isolate pushed to needs-review for no reason. I checked
the current batch and there are zero duplicate candidate rows, so this is
theoretical today. A `dplyr::distinct(os_identifier, .keep_all = TRUE)` before
the count would close it cheaply.

## Requirement coverage

| Handoff requirement | Covered |
|---|---|
| `SNT/APPS/React` overlaps `APPS 2`, `APPS_2`, `APPS _2` | yes |
| Matching case-insensitive for known family names | yes (`"react"`) |
| SNT Vitek can select an APPS 2 Cryopreserved Cells record | yes |
| FAIR / ARRRRG / Pre-Alert / MEPSD not treated as family | yes |
| Same family but wrong participant or label not auto-accepted | yes |
| Multiple similarly scored candidates remain in needs-review | yes |
| Existing automatic-match tests continue to pass | yes, 160 passing, 0 failures |

## Verdict

Approve once Finding 1 is addressed — the allowlist should not be able to
silently drop a protocol the lab actually uses, and `Sentinel-REACT` is not
hypothetical. Findings 2 and 3 are a documentation change and a one-line guard.

The handoff has the matching PR merging first because its files are separate
from the persistence work; I confirmed that holds. PR #13 touches only
`R/data_match.R` and `tests/testthat/test-match.R`, neither of which either
Workstream B branch modifies.
