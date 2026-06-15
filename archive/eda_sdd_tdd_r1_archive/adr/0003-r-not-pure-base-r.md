# ADR 0003: Use R, but not pure base R

## Status

Proposed.

## Context

The package is intended for epidemiology and biomedical research workflows. R is well suited to statistical summaries, tabular analysis, plotting, reports and package-based reuse.

A pure base R implementation would reduce dependencies but increase implementation effort and reduce compatibility with modern research workflows.

## Decision

Use R as the implementation language.

Do not restrict the package to pure base R.

Use base R where it is simple and stable. Use selected dependencies for tabular data, plotting, reporting and large-data backends.

## Consequences

Positive:

- aligns with epidemiology and biostatistics practice;
- supports reproducible reports;
- supports tidy workflows;
- allows integration with `data.table`, Arrow and DuckDB later;
- improves developer productivity.

Negative:

- dependency management matters;
- some functions may need optional dependency checks;
- package startup and installation may be heavier than pure base R.

## Dependency policy

Prefer few hard dependencies.

Potential hard dependencies:

- `rlang`
- `tibble`
- `dplyr`
- `purrr`
- `ggplot2`

Potential suggested dependencies:

- `data.table`
- `arrow`
- `duckdb`
- `quarto`
- `targets`
- `skimr`
- `naniar`
- `visdat`
- `pointblank`
- `validate`
- `simstudy`

Heavy tools should remain in `Suggests` unless required for the core API.
