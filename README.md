# AXIS

AXIS is a private internal research-group R Shiny app for linking synthetic VITEK2 susceptibility exports to synthetic OpenSpecimen inventory exports. It summarizes linked inventory by MDRO category, species, specimen type, study, and site.

This scaffold contains only synthetic fixture data. Do not commit PHI, real specimen accession IDs, real isolate IDs, real patient data, or real OpenSpecimen/VITEK exports.

## Repository Layout

- `app.R`: Shiny entry point.
- `R/import_vitek.R`: VITEK2 CSV import and normalization.
- `R/import_openspecimen.R`: OpenSpecimen CSV import and normalization.
- `R/link_results.R`: Deterministic linking between susceptibility and inventory rows.
- `R/mdro_categories.R`: MDRO category rules.
- `R/summarize_inventory.R`: Inventory summary helpers.
- `tests/fixtures/`: Synthetic CSV fixtures only.
- `docs/`: Data dictionary and MDRO rule notes.

## Run Locally

Install the minimal runtime dependencies:

```r
install.packages(c("shiny", "testthat"))
```

Start the app from the repository root:

```r
shiny::runApp(".")
```

Run tests:

```r
testthat::test_dir("tests/testthat")
```

## Data Handling

Use synthetic or de-identified development data only. Real exports should remain outside the repository and should be ignored by git. If local paths are needed, keep them in `.env` or `.Renviron`, not in committed files.
