# Link confirmation performance

Measured on 2026-08-13 on the same development machine, using approved local
example exports outside the repository. Benchmark output contained aggregate
counts and timings only; no identifiers or source records were retained.

## Representative workload

| Input | Rows |
|---|---:|
| Deduplicated Vitek records | 1,936 |
| AST rows | 93,039 |
| OpenSpecimen records | 11,540 |

## Before

Five representative manual confirmations used the legacy
`commit_matched_links()` path. Each click persisted the source batch, rebuilt
the full cleaned datasets, and wrote CSV, XLSX, and DuckDB outputs.

| Confirmation | Seconds |
|---|---:|
| 1 | 73.843 |
| 2 | 76.219 |
| 3 | 72.605 |
| 4 | 75.429 |
| 5 | 72.302 |

Median: **73.843 seconds**. Range: **72.302–76.219 seconds**.

Isolated component timings were 0.005 seconds for link persistence, 30.408
seconds for source persistence, and 20.407 seconds for the cleaned-data
rebuild. Independently invoking each output-format path took 20.286 seconds for
CSV, 20.535 seconds for XLSX, and 22.228 seconds for DuckDB; these format paths
also materialize the specimen dataset, which is the dominant shared cost.

After five confirmations, the source tables contained six copies of the
representative source rows in the benchmark database (five confirmation writes
plus one isolated component write): 16,500 Vitek raw rows, 558,234 AST rows,
and 69,240 specimen rows.

## After

The source batch was persisted once during preparation. Twenty representative
manual confirmations then used `record_confirmed_links()` without rebuilding
or exporting cleaned data.

| Metric | Seconds |
|---|---:|
| Minimum confirmation | 0.056 |
| Median confirmation | 0.058 |
| Maximum confirmation | 0.130 |
| One-time source persistence | 31.421 |
| Explicit combined CSV/XLSX/DuckDB export | 21.848 |

The 20 confirmations produced 20 logical links and 20 audit events. Repeating
one confirmation inserted zero new links. Vitek raw, AST, and specimen source
row counts were unchanged across all confirmations, and no confirmation
invoked an export.

The explicit final export matched the content produced by the existing cleaned
builders for the same links and inputs. Its measured component paths were
21.385 seconds for rebuilding, 0.012 seconds for CSV, 0.372 seconds for XLSX,
and 0.171 seconds for DuckDB. The rebuilt specimen dataset is passed into each
output writer rather than materialized again.

## Result

The typical confirmation target of under two seconds was met with substantial
margin, and no measured confirmation exceeded five seconds. The full rebuild
remains deliberately separate and is run once through **Rebuild and export
cleaned data** while the application displays a busy state.
