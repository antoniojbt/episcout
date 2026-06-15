# Software Design

Spec ID: `003-large-data-backend-strategy`  
Status: Draft  

## Scope

Define how the specification-first EDA workflow should later support larger
datasets while preserving the current data-frame/tibble API.

## Design Direction

- Keep data frames and tibbles as the baseline backend.
- Add backend support incrementally and behind stable user-facing functions.
- Compute summaries on full data where practical.
- Collect small, deterministic samples for plotting.
- Cache machine-readable outputs so reports do not recompute expensive steps.
- Keep optional backend dependencies optional unless implementation proves they
  should move to `Imports`.

## Candidate Sequence

1. Document backend interface expectations.
2. Add data.table-backed summaries if profiling shows a need.
3. Add DuckDB support for local SQL-style analysis.
4. Add Arrow support for multi-file Parquet datasets.
5. Add cache invalidation and sampled plotting contracts.

## Compatibility

Existing calls to `epi_eda_run()`, `epi_eda_profile_missing()`,
`epi_eda_profile_summaries()` and `epi_eda_profile_plots()` should continue to
work on ordinary data frames.

## Out Of Scope

No backend implementation in the first design PR.
