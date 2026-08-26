# Diagnosis: missing Enterococcus, duplicate aliquot candidates, and APPS label matching

Three reported problems, four root causes, all reproduced against the loaded
batch (2,750 Vitek rows, 1,936 deduplicated isolates, 11,540 OpenSpecimen
records). Every number below comes from running the shipped functions over that
data, not from reading the code.

None of these are regressions from the recent linking work. All four predate it.

---

## 1. REACT Enterococcus missing from the Inventory dashboard and Sankey

**Reported:** no Enterococcus isolates from the REACT Rush/RML site reach the
dashboard, even with *Include OpenSpecimen-only Enterococcus* ticked.

**Reproduced.** Filtering the Sankey to `RML Specialty Hospital` keeps **0 of
92** eligible OpenSpecimen-only Enterococcus records.

### 1a. The OS-only path never resolves the site (this is the reported bug)

`add_inventory_flow_fields()` — the *linked* path — derives the site from the
participant identifier:

```r
flow_site = display_flow_site(inv_site_label, project_id, cp_short_title)
```

`inv_site_label` comes from `derive_site_code()`, which reads characters 2–3 of
the participant id. For `rML01`, `aML001` that is `ML`, which the site
dictionary maps to **RML Specialty Hospital**.

`os_enterococcus_flow_rows()` — the *OpenSpecimen-only* path — hardcodes the
site as missing:

```r
flow_site = display_flow_site(NA_character_, project_id, cp_short_title)
```

With no site label it falls through to the collection protocol, so every
OS-only record is labelled **REACT**.

Measured on the 92 ML Enterococcus cryovials:

| Path | Site assigned |
|---|---|
| Linked | `RML Specialty Hospital` (92 of 92) |
| OpenSpecimen-only | `REACT` (all) |

The site dropdown is built from the linked rows, so it offers *RML Specialty
Hospital*. Selecting it discards every OS-only row, because theirs says *REACT*.
The same physical specimen has two different sites depending on which path it
travelled.

**Fix:** call `derive_site_code()` in the OS-only path as well, using
`participant_id` and `specimen_label`. The specimens table also carries
`custom_site` (`"Rush RML"` for these records), which is a more direct source
and worth preferring where present.

### 1b. The species whitelist excludes everything except two species

```r
.organism_norm %in% c("ENTEROCOCCUS FAECIUM", "ENTEROCOCCUS FAECALIS")
```

The export contains **8 REACT records recorded as `Enterococcus spp`**, one of
them at ML. They are silently dropped. Any *E. gallinarum* or *E. casseliflavus*
entered later would be too — species that matter for VRE surveillance.

**Fix:** match on the genus, keeping the recorded species for the display node.

### 1c. OpenSpecimen-only rows are suppressed when nothing is linked

```r
os_only_flow <- if (nrow(d) > 0 && include_os_only) { ... } else { flow_empty() }
```

If the linked set is empty — a fresh database, or a filter combination with no
confirmed links — the OS-only rows are not computed at all. Two lines later the
code handles `nrow(d) == 0 && nrow(os_only_flow) == 0`, which the guard makes
unreachable; the intent was clearly that OS-only rows can stand alone.

**Fix:** drop `nrow(d) > 0` from the condition.

---

## 2. Cryopreserved Cell aliquots appearing as separate review candidates

**Reported:** `ARG030_P1ESBL1` and `ARG030_P1ESBL1w/glycerol1` both appear as
100-point candidates, with many extras across FAIR and ARRRRG.

**Reproduced.** **93 normalised labels are shared by two kept specimens each,
creating 93 duplicate candidate rows** — 102 in ARRRRG 2.0 and 84 in FAIR 618,
matching the cohorts reported.

The two records are:

| Label | Class | Type | Lineage | Parent |
|---|---|---|---|---|
| `ARG030_P1ESBL1` | Cell | Cryopreserved Cells | **Derived** | `ARG030_P1` |
| `ARG030_P1ESBL1w/glycerol1` | Cell | Cryopreserved Cells | **Aliquot** | `ARG030_P1ESBL1` |

Two behaviours combine:

`.strip_cryo_glycerol_suffix()` normalises the glycerol label to exactly the
parent's label — deliberate, and correct for display.

`.axis_is_review_aliquot` then decides what to exclude:

```r
.axis_is_review_aliquot =
  grepl("^aliquot$", .axis_type_trim, ignore.case = TRUE) |
  (grepl("^aliquot$", .axis_class_trim, ignore.case = TRUE) &
     .axis_type_trim != "Cryopreserved Cells")
```

It tests `type` and `class` but never `lineage`. For these records `type` is
*Cryopreserved Cells* and `class` is *Cell*, so **0 of 1,994 glycerol records
are excluded**. Both survive, both carry the same normalised label, and both
score identically.

This is not a regression from the Manual Link change. That change affects the
Manual Link dropdown only; automatic matching has always used this filter.

**Fix:** treat `lineage == "Aliquot"` as an aliquot for candidate generation
when a specimen sharing its normalised label is also present. Where the parent
is absent from the export the aliquot must stay — it is then the only record of
that isolate, and dropping it would recreate the Manual Link problem.

---

## 3. APPS isolates matching the wrong OpenSpecimen record

**Reported:** second-round APPS isolates return only first-round records as
candidates; `APPS0028igCRE3of3` does not get `APPS0028_ig_dp_CRE#3of3`, though
that record appears as a candidate for a different Vitek record.

**Reproduced, and one root cause explains both halves.**

Scoring `APPS0028igCRE3of3` against the loaded specimens:

| Candidate | Score | label_score |
|---|---|---|
| `APPS0028_pr_dp_ESBL#1of1` | 60 | **0** |
| `APPS0028_pr_dp_CRE#1of3` | 60 | **0** |
| `APPS0028_ig_dp_ESBL#1of1` | 60 | **0** |
| … | | |
| `APPS0028_ig_dp_CRE#3of3` — **the correct record** | 55 | **0** |

The correct record ranks **8th of 1,617**, below several wrong ones.

`label_score` is **zero for every APPS candidate**, so the strongest signal in
the matcher — worth 60 of 100 points — contributes nothing across the entire
APPS cohort. Ranking then falls to participant, MDRO, organism and date, which
are identical for every isolate from the same participant. Which record wins is
effectively arbitrary, which is exactly why the correct record shows up against
a different Vitek isolate.

The cause is in `.norm_accession_label()`:

```r
x <- gsub("_+", "", x)   # underscores only
```

Only underscores are removed. APPS labels differ from their Vitek counterparts
in two further ways:

```
OpenSpecimen  APPS0028_ig_dp_CRE#3of3  ->  APPS0028IGDPCRE#3OF3
Vitek         APPS0028igCRE3of3        ->  APPS0028IGCRE3OF3
```

- the `#` before the aliquot ordinal survives normalisation — **106 records** in
  the SNT/APPS/React protocol carry one, and **no Vitek id contains `#`**;
- the OpenSpecimen label carries a `dp` token the Vitek id omits.

Neither string equals the other, and neither contains the other, so both the
exact and substring tests fail.

Round two is the same failure. Round-2 Vitek ids carry an `n2` token
(`APPS0017n2igCRE1of1`); with the label signal already dead, a round-2 isolate
is ranked purely on participant and MDRO, where round-1 records from the same
participant tie or beat it.

**Fix, in two parts.**

Strip all punctuation rather than underscores alone. That is strictly an
improvement — **6,315 normalised labels still contain punctuation** today — but
it is not sufficient on its own; measured across the batch it recovers exactly
one additional exact match, because of the `dp` token.

Add an ordered-subsequence test as an intermediate label score, below an exact
match. Measured over the 153 APPS isolates that currently have no exact partner:

| Outcome | Count |
|---|---|
| Unique subsequence partner found | **49** |
| Ambiguous (more than one partner) | **0** |
| No partner in the export | 104 |

Zero ambiguity across the cohort, and Cedar's example resolves. The 104 with no
partner are consistent with round-2 records still being entered.

This sits in the matching workstream rather than the linking one, and the
handoff is explicit that a fuzzy enhancement must not auto-confirm an uncertain
link — so a subsequence hit should score below an exact match and land in
needs-review, never in auto-matched.

---

## Recommended order

| Fix | Risk | Effect |
|---|---|---|
| 1a site derivation in the OS-only path | Low | Unblocks the Rush/RML isolate list |
| 1c drop the `nrow(d) > 0` guard | Low | One line |
| 2 exclude aliquot-lineage duplicates | Low | Removes 93 spurious candidates |
| 1b widen the Enterococcus whitelist to the genus | Low, clinical sign-off | Recovers 8 records |
| 3 label normalisation and subsequence scoring | Medium | Recovers ~49 APPS pairs; needs its own tests |

Items 1 and 2 are unambiguous defects with one obvious correct behaviour. Item 3
changes matching behaviour for every cohort and deserves the same treatment the
SNT/APPS family change got: an explicit function, its own tests, and a measured
before/after on bucket counts.

## Reproducing

All figures come from the shipped functions run over the cached fixture
described in `docs/AXIS_DEV_ENVIRONMENT.md`. No identifiers appear in this
document beyond the specimen labels already quoted in the reports.

---

# Fixes applied, and what they measured

All three fixes are in this branch. Measured over the same batch.

## Inventory

| | Before | After |
|---|---|---|
| OS-only Enterococcus rows | 772 | 834 |
| Surviving a `RML Specialty Hospital` filter | **0** | **92** |
| `Enterococcus spp` rows included | 0 | 9 |

Sites now resolve for every cohort — RML Specialty Hospital, Emory LTAC,
Emory University Hospital, A.G. Rhodes, Good Shepherd Penn Partners — using the
same derivation as the linked path, so the two can no longer disagree about
where a specimen came from. A test asserts that equality directly rather than
checking each path in isolation.

## Matching

| | Before | After |
|---|---|---|
| Candidate rows | 2,213 | 2,135 |
| Auto-matched isolates | 730 | **751** |
| Needs-review rows | 805 | 637 |
| No-match isolates | 985 | **953** |
| Isolates that left no-match | — | **32** |
| Isolates that entered no-match | — | **0** |
| Duplicate glycerol candidates | 93 | **0** |
| Auto-matched on a subsequence-only label | — | **0** |

Cedar's example, `APPS0028igCRE3of3`:

| | Score | Rank | Label signal |
|---|---|---|---|
| Before | 55 | 8th of 1,617 | 0 |
| After | **85** | **1st** | 30, `subsequence` |

It lands in needs-review, not auto-matched, so she confirms it deliberately.

### The aliquot bug was also suppressing correct matches

21 isolates newly auto-match. All 21 had a duplicated specimen label among
their candidates, and all 21 had a runner-up within 5 points — the glycerol
aliquot was tying with its own parent and tripping the ambiguity rule added in
PR #13. All 21 now match on an **exact** label. So the duplicate records were
not merely noise in the review queue; they were holding correct links out of
auto-match, and every one of those had to be confirmed by hand.

Nothing regressed: no isolate moved into no-match, and no subsequence-only
label reached the auto-matched bucket.

Test suite: 388 passing, 0 failures, 0 warnings, 3 environment-dependent skips.
