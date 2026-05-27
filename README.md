# AXIS

AXIS is an internal R Shiny app for linking VITEK2 antimicrobial susceptibility exports to OpenSpecimen inventory exports. It supports multi-file ingestion, accession-centered candidate linkage, cleaned CSV/AST export, review queues, and inventory summaries by site, study, specimen flow, MDRO category, and species.

Do not commit PHI, real specimen accession IDs, real isolate IDs, real patient data, real VITEK/OpenSpecimen exports, DuckDB runtime databases, or cleaned exports.

## Repository Layout

- `app.R`: Shiny entry point.
- `R/data_parse_vitek.R`: VITEK2 export parsing.
- `R/data_parse_os.R`: OpenSpecimen CSV/ZIP export parsing.
- `R/data_match.R`: Candidate linkage scoring and bucket assignment.
- `R/data_clean.R`: Cleaned link and AST export generation.
- `R/mod_ingestion.R`: Ingestion, automerge, and export UI.
- `R/mod_inventory.R`: Inventory summary, Sankey flow, and site map UI.
- `R/mod_linking.R`: Linking and cleaned-field review UI.
- `R/store.R`: Local DuckDB persistence layer.
- `tests/testthat/`: Synthetic-unit tests only.
- `docs/`: Data dictionary and MDRO rule notes.

## Run Locally

Start the app from the repository root:

```r
shiny::runApp(".")
```

The app installs missing R packages on startup. To run tests:

```r
setwd("tests/testthat")
testthat::test_dir(".")
```

## Local Data

Real exports should stay outside git-tracked paths. The app defaults to:

```text
../01.data/01.openspecimen_exports
```

Override with `AXIS_OS_DIR` or an R option in a local, uncommitted profile:

```r
options(axis.os_data_dir = "/path/to/openspecimen_exports")
```
