# AXIS Synthetic Data Dictionary

## Synthetic VITEK2 Fixture

File: `tests/fixtures/synthetic_vitek2.csv`

| Column | Description |
| --- | --- |
| `isolate_id` | Synthetic isolate identifier. |
| `specimen_accession_id` | Synthetic specimen accession-like label used for linking. |
| `species` | Organism species reported in the susceptibility result. |
| `specimen_type` | Source specimen type from the susceptibility export. |
| `study` | Synthetic study label. |
| `site` | Synthetic collection or study site. |
| `collection_date` | Synthetic specimen collection date. |
| `test_date` | Synthetic susceptibility test date. |
| `antibiotic` | Antibiotic tested. |
| `interpretation` | Susceptibility call such as `S`, `I`, or `R`. |
| `mdro_hint` | Optional synthetic source category label. |

## Synthetic OpenSpecimen Fixture

File: `tests/fixtures/synthetic_openspecimen.csv`

| Column | Description |
| --- | --- |
| `specimen_id` | Synthetic OpenSpecimen-style row identifier. |
| `specimen_label` | Synthetic specimen label used for linking. |
| `species` | Inventory organism label. |
| `specimen_type` | Inventory specimen type. |
| `study` | Synthetic study label. |
| `site` | Synthetic collection or study site. |
| `collection_date` | Synthetic specimen collection date. |
| `inventory_status` | Synthetic inventory state. |
| `quantity` | Numeric inventory quantity. |
| `unit` | Inventory unit. |
| `mdro_hint` | Optional synthetic source category label. |
