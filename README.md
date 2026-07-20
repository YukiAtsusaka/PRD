# Political Rankings Database (PRD) — Replication Package

Atsusaka & Singh (2026). Standardized ranking data from published political
science studies.

This package reproduces the standardized ranking datasets in `standardized/`
from the raw replication files in `raw/`, using the scripts in `R/`.

## Structure

```
code-atsusaka-singh-2026/
├── R/              # one Standardize_*.R per study + finalize_schema.R
├── raw/            # raw replication inputs (one or more per study)
├── standardized/   # standardized output CSVs (the database)
├── .here           # marks the package root for the `here` package
└── README.md
```

## How to reproduce

Every `Standardize_*.R` script reads its raw input(s) from `raw/`, writes a
standardized CSV to `standardized/`, and then calls `finalize_csv()` (defined in
`R/finalize_schema.R`) to enforce the common schema and add the `ranking`
column. Paths are resolved with the `here` package relative to this folder (the
`.here` file marks the root), so run scripts with this folder as the working
directory:

```r
setwd("path/to/code-atsusaka-singh-2026")
# reproduce a single study:
source("R/Standardize_Costa2020.R")
# or reproduce everything:
for (f in list.files("R", pattern = "^Standardize_.*\\.R$", full.names = TRUE)) source(f)
```

`finalize_schema.R` is a shared post-processing step, not an analysis helper: it
is sourced by every standardize script and is required for reproduction.

## Standardized schema

Each file in `standardized/` has: `unit` (row id), one column per ranking item
(named after the item), a `ranking` summary string, `treat` (0-indexed
treatment; 0 = control), and study-specific covariates. Ranks are 1 = most
preferred. Unranked items in partial rankings are `NA`.

## Dependencies

```r
install.packages(c("dplyr", "tidyr", "stringr", "readr", "here",
                   "haven", "readxl", "magrittr"))
```

R 4.x. `haven` reads Stata (`.dta`) and SPSS (`.sav`); `readxl` reads Excel.

## Known limitations — raw data not redistributable

Two studies' standardized outputs are included in `standardized/`, but their
raw inputs are **not** in `raw/` (not available for redistribution here), so
their scripts cannot be re-run without obtaining the original files:

| Study | Script | Missing raw input |
|-------|--------|-------------------|
| Doshi, Kelley & Simmons (2019) | `Standardize_Doshi2019.R` | `Doshi2019_WorldBank.dta` (World Bank Ease-of-Doing-Business panel) |
| Malhotra & Kuo (2008) | `Standardize_MalhotraKuo2008.R`, `Standardize_MalhotraKuo2008_Splits.R` | `Katrina_Blame_Data.sav` (Harvard Dataverse hdl:1902.1/16325) |

All other studies reproduce from the included raw files.
