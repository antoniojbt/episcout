# Brief

Spec ID: `008-univariate-stats-eda-alignment`  
Status: Implemented  
Owner: Antonio Berlanga-Taylor  

## Problem

The active `epi_stats_*` univariate functions and specification-first EDA summaries duplicate calculations, apply inconsistent edge policies, and expose different subsets of numeric, categorical, text and temporal information. EDA currently omits text, date and datetime variables without a documented skip.

## Goal

Create shared unexported univariate summary cores, retain the existing EDA contract as version `v1`, add an opt-in complete `v2` contract, and align the active public statistics wrappers without renaming or deprecating them.

## Non-goals

- Correlation, contingency, epidemiological outcome and presentation redesigns.
- Switching the default EDA summary contract from `v1` to `v2` in this release.
- Adding package dependencies.

## Candidate Files

- `R/summary_cores.R`
- `R/eda_summaries.R`
- `R/epi_stats_summary.R`
- `R/run_eda.R`
- `R/eda_report.R`
- `tests/testthat/test-summary-cores.R`
- `tests/testthat/test-eda-summaries-v2.R`

## Risks

- Existing consumers depend on exact `v1` list components and CSV schemas.
- Public statistics wrappers have historical output shapes that must remain stable while their calculations are corrected.
- Temporal character parsing must reject invalid non-missing values rather than silently coercing them.
