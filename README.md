<p align="center">
  <img src="docs/assets/axis_logo_v1.png" alt="AXIS logo" width="900">
</p>

# axis
Shiny app to link OpenSpecimen inventories with Vitek2 data

# SOP: Install, Clone, and Run the AXIS Shiny App

## Purpose

This SOP describes how to install dependencies, clone the AXIS GitHub repository, and run the AXIS Shiny app from a local command line.

AXIS is a Shiny app. It does not run as a standalone HTML file. A local R process must be running while the app is open in a browser.

## Prerequisites

Install these before cloning the repository:

1. R, version 4.3 or later preferred: https://cran.r-project.org/
2. Git: https://git-scm.com/downloads
3. Optional but recommended: RStudio or Positron for interactive R use.

On macOS, if package installation fails because compilers are missing, install Apple command line tools:

```bash
xcode-select --install
```

On Windows, if package installation fails because compilers are missing, install Rtools matching the installed R version:

```text
https://cran.r-project.org/bin/windows/Rtools/
```

## Clone the Repository

Choose a local project directory. This can be a OneDrive-shared directory if the team wants a shared working copy, but each active analyst should avoid running the same DuckDB file at the same time.

From Terminal, macOS/Linux:

```bash
cd "/path/to/your/project/folder"
git clone https://github.com/michaelwoodworth/axis.git
cd axis
```

From PowerShell, Windows:

```powershell
cd "C:\path\to\your\project\folder"
git clone https://github.com/michaelwoodworth/axis.git
cd axis
```

Confirm that the current directory contains the Shiny app entry point:

```bash
ls app.R
```

If `app.R` is not found and the clone contains a nested `axis/` folder, move into that app folder:

```bash
cd axis
ls app.R
```

All remaining commands below assume the working directory is the folder that contains `app.R`.

## Install R Package Dependencies

The app attempts to install missing packages automatically at launch. To install them explicitly first, run:

```bash
Rscript -e 'install.packages(c("shiny","bslib","readxl","fuzzyjoin","duckdb","DBI","dplyr","purrr","lubridate","readr","shinybusy","uuid","stringdist","tibble","DT","echarts4r","leaflet","tidyr"), repos="https://cloud.r-project.org")'
```

If using RStudio, the same command can be run in the R console:

```r
install.packages(
  c(
    "shiny", "bslib", "readxl", "fuzzyjoin", "duckdb", "DBI", "dplyr",
    "purrr", "lubridate", "readr", "shinybusy", "uuid", "stringdist",
    "tibble", "DT", "echarts4r", "leaflet", "tidyr"
  ),
  repos = "https://cloud.r-project.org"
)
```

## Run the App Locally

From the app root, the folder containing `app.R`:

```bash
Rscript -e 'shiny::runApp(host="127.0.0.1", port=3838, launch.browser=TRUE)'
```

If the browser does not open automatically, open:

```text
http://127.0.0.1:3838/
```

To stop the app, return to the terminal running Shiny and press:

```text
Ctrl+C
```

## Run on a Different Port

If port `3838` is already in use:

```bash
Rscript -e 'shiny::runApp(host="127.0.0.1", port=3839, launch.browser=TRUE)'
```

Then open:

```text
http://127.0.0.1:3839/
```

## Optional: Run for Same-Network Access

For controlled internal testing on the same network only:

```bash
Rscript -e 'shiny::runApp(host="0.0.0.0", port=3838, launch.browser=FALSE)'
```

Then another user on the same network may be able to open:

```text
http://YOUR_COMPUTER_IP:3838/
```

Use this only on a trusted network. Do not expose PHI, specimen-level data, or internal inventories on an open/public network.

## Data and Working Files

The app creates and uses local runtime files under:

```text
data/
data/axis.duckdb
data/exports/
```

These files should generally not be committed to GitHub.

Important OneDrive note: avoid having multiple users run the same `data/axis.duckdb` file from a synced/shared OneDrive folder at the same time. DuckDB uses file locks, and OneDrive syncing can create conflicts. The safest model is:

```text
GitHub: shared versioned code
Each analyst's local app folder: local DuckDB/runtime data
Shared project folder: curated exports or reviewed outputs
```

## Pull Updates Later

From the app root or repository root:

```bash
git pull
```

Then restart the app:

```bash
Rscript -e 'shiny::runApp(host="127.0.0.1", port=3838, launch.browser=TRUE)'
```

## Run Tests

From the app root:

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
```

If `testthat` is missing:

```bash
Rscript -e 'install.packages("testthat", repos="https://cloud.r-project.org")'
```

## Troubleshooting

If packages fail to install, verify R is installed and has internet access:

```bash
Rscript -e 'R.version.string'
```

If the app says port 3838 is busy, use another port:

```bash
Rscript -e 'shiny::runApp(host="127.0.0.1", port=3839, launch.browser=TRUE)'
```

If the app cannot open or write DuckDB, close other running AXIS app sessions and restart. Only one R process should write to the same DuckDB file at a time.

If the app cannot find expected OpenSpecimen exports, either upload exports through the Ingestion tab or set `AXIS_OS_DIR` before launching:

```bash
export AXIS_OS_DIR="/path/to/openspecimen_exports"
Rscript -e 'shiny::runApp(host="127.0.0.1", port=3838, launch.browser=TRUE)'
```
