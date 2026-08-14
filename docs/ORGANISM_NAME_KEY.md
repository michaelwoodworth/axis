# Curated organism names in the cleaned dataset

VITEK2 reports abbreviated organism names — `Esch.coli`, `Psdes.vulneris`,
`K.pneum.ozaenae` — and the spelling changes between card versions. In the
current batch the same organism code, `EEE`, appears as both `Ent.aerogenes`
and `K.aerogenes`, so one organism is two different strings in every count.

`inst/extdata/vitek_organism_key.csv` maps the stable VITEK2 **organism code**
to a reviewed organism name, its genus, species, subspecies, and rank. The
cleaned dataset carries the curated name; the raw VITEK value stays untouched.

## Where names come from

Names follow [LPSN](https://lpsn.dsmz.de) and its List of Recommended Names for
bacteria of medical importance (LoRN). Where LPSN flags a newer combination as
*not recommended for medical use*, the key keeps the recommended name.

Five entries differ in genus from the VITEK display name. All were reviewed and
accepted 2026-08:

| VITEK2 | Cleaned name | Basis |
|---|---|---|
| `Ent.aerogenes` / `K.aerogenes` (EEE) | *Klebsiella aerogenes* | Tindall et al. 2017; on LoRN |
| `Raou.planticola` (EKV) | *Klebsiella planticola* | LPSN correct name; *Raoultella planticola* is a synonym not recommended for medical use |
| `Ps.stutzeri` (PPS) | *Stutzerimonas stutzeri* | Lalucat et al. 2022; on LoRN, explicitly recommended for medical use |
| `Psdes.vulneris` (ECU) | *Pseudescherichia vulneris* | Alnajar & Gupta 2017; on LoRN |
| `Rzb.radiobacter` (PHC) | *Agrobacterium radiobacter* | LPSN correct name; *Rhizobium radiobacter* is a synonym not recommended for medical use |

Four were considered and deliberately left alone, because LPSN flags the newer
combination as not recommended for medical use or the VITEK name is already
current: *Pseudomonas mendocina* (not *Ectopseudomonas*), *Staphylococcus
lentus* (not *Mammaliicoccus*), *Pseudomonas luteola*, *Pseudomonas
oryzihabitans*.

**`Klebsiella planticola` moves one isolate into the *Klebsiella* genus
totals.** That is the only change with an effect on group-level counts.

## Columns added to the cleaned dataset

| Column | Meaning |
|---|---|
| `clean_organism` | Curated name, or the analyst override if one exists |
| `clean_organism_genus` | Genus, for grouping |
| `clean_organism_species` | Species epithet; `NA` for complexes, groups and genus-only IDs |
| `clean_organism_subspecies` | Subspecies where VITEK reported one |
| `clean_organism_rank` | `species`, `subspecies`, `complex`, `group`, `genus`, `unidentified`, `unmapped`, or `override` |
| `v_organism_code` | The raw VITEK2 code, so any cleaned name can be traced back |

`v_organism` continues to hold the raw VITEK name, and `vitek_raw` is never
modified.

## Precedence

1. Analyst override saved in Edit mode (`cleaned_overrides`, field `organism`)
2. This key, matched on VITEK2 `organism_code`
3. The raw VITEK name, when the code is not in the key
4. The OpenSpecimen `custom_organism`, when there is no VITEK organism at all

An override sets `clean_organism_rank` to `override` and clears the parsed
genus, species, and subspecies, so a downstream join on
`clean_organism_genus` can never contradict `clean_organism`.

## Rules the key follows

- **Subspecies collapse to the species.** `K.pneum.ozaenae` becomes
  *Klebsiella pneumoniae* with `clean_organism_subspecies = ozaenae`. In the
  current batch this consolidates 503 *K. pneumoniae* isolates that were
  previously three separate strings.
- **Complexes, groups and `spp` entries keep their granularity** and are never
  resolved to a species. `Ent.cloacae complex` becomes *Enterobacter cloacae
  complex*, rank `complex`, with no species epithet.
- **`Low Discrim Organism` gets no name at all** — `clean_organism` is `NA`,
  rank `unidentified`. It is a statement that VITEK made no identification, not
  a missing value to be filled from OpenSpecimen.
- **An unknown code is never silently blanked.** The raw VITEK name carries
  through with rank `unmapped`, so it can be found and the key extended.

## Extending the key

Add a row to `inst/extdata/vitek_organism_key.csv`. `tests/testthat/test-organism-key.R`
checks the file's shape — unique codes, valid ranks, species rows whose
genus and epithet reconstruct the name — so a malformed edit fails the suite
rather than an export.

To find codes needing review after an ingestion:

```r
unmapped_organism_codes(app_state$vitek_raw)
```

## Coverage as reviewed

68 VITEK2 codes, 69 display names, 2,750 VITEK rows, 1,936 deduplicated
isolates. Zero unmapped codes. 67 distinct organism strings become 64.

| Rank | Isolates |
|---|---|
| species | 1,510 |
| complex | 227 |
| subspecies | 154 |
| genus | 37 |
| group | 5 |
| unidentified | 3 |
