# PostgreSQL backend for specification-driven EDA in `episcout`

## Status

Incorporated into active spec `003-large-data-backend-strategy` on 2026-08-04. This file remains a design input, while the active brief, SDD, TDD, acceptance contract, manifest and review are implementation authority. This plan is independent of Vive Feliz. Vive Feliz supplies a representative large-data workload and a later performance check, but no project-specific names, schemas, credentials, dictionaries, fixtures, or output conventions belong in `episcout`.

## Purpose

Add PostgreSQL as the first database backend for specification-driven EDA while preserving the existing `episcout` dictionary, summary, plotting, and result contracts. The backend must operate on large tables without collecting complete row-level datasets into R.

## Required design

Follow active `future/specs/003-large-data-backend-strategy` before changing package code. Its reviewed first implementation is PostgreSQL-only; Arrow, DuckDB, data.table, and generic DBI support remain outside this version.

The established public EDA functions remain the analytical interface:

- `epi_eda_check_schema()`
- `epi_eda_profile_missing()`
- `epi_eda_profile_summaries()`
- `epi_eda_profile_plots()`

Extend these functions to accept a PostgreSQL-backed source while preserving their behaviour for data frames. Add a small `epi_eda_db_run()` orchestrator only to validate the source, call the existing `epi_*` functions, and write their returned artifacts. Do not create a second family of database-only summary or plotting APIs.

Refactor existing internals only where this allows the data-frame and PostgreSQL paths to share:

- statistical definitions and canonical output schemas;
- plot-data contracts and plot renderers;
- specification validation;
- artifact naming, writing, and manifest handling.

PostgreSQL-specific code is limited to catalog inspection, safe SQL construction, and translation of the established calculations into aggregate queries. Use DBI identifier quoting and parameter binding. Do not log SQL containing sensitive specification values or read row-level identifiers into package artifacts.

## Summary behaviour

Process every variable in the supplied specification. PostgreSQL results must use the existing canonical components and definitions:

- `variables`
- `numeric`
- `categorical`
- `text`
- `temporal`
- `skipped`

Database results must match the in-memory results for the same reviewed fixture, including missing sentinels, finite and infinite numeric values, type-7 quantiles, spread and shape statistics, outlier fences and counts, declared and unexpected categorical levels, text diagnostics, and temporal summaries. A bounded collection is acceptable only when an existing calculation intrinsically requires a small vector, such as Shapiro testing within its current observation limit.

Variables whose reviewed role is `id` or `identifier` receive aggregate QA rather than ordinary type summaries. Record missingness, observed count, distinct count, repeated-value count, duplicate excess, and maximum frequency without returning values or producing plots. Keep this policy explicit in the canonical variable status and in a separate identifier-QA artifact.

## Plot behaviour

Do not introduce independent PostgreSQL plotting code. Separate the existing plotting path into compact plot-data preparation and shared rendering where necessary:

- the data-frame backend prepares plot data from vectors;
- the PostgreSQL backend prepares equivalent bins, counts, or quantiles with aggregate SQL;
- both backends pass those objects to the same `episcout` renderers.

Use the existing variable-type dispatch and improve it consistently for both backends when needed. The intended outputs are:

- numeric and integer: histogram and box/quantile plot;
- categorical and binary: frequency bar plot;
- text: length-distribution plot rather than a plot of raw text values;
- date and datetime: temporal distribution plot;
- identifier: no plot.

When a categorical variable has too many levels for a readable plot, retain its complete frequency summary and collapse only the displayed plot to the configured leading levels plus an explicit remainder.

## Written artifacts

`epi_eda_db_run()` writes an owned output bundle containing:

- normalized specification and run metadata;
- schema and missingness tables;
- all six canonical summary CSV files;
- identifier QA;
- individual SVG plot files;
- a plot inventory linking variables, plot types, counts, and relative paths;
- warnings or skipped reasons;
- query timings;
- a manifest with relative paths and checksums.

Do not write source rows, prepared rows, row previews, raw identifier values, credentials, or connection details. Reuse the package's existing staged-write and manifest-validated overwrite behaviour rather than establishing a second filesystem policy.

## Execution and performance

Begin with sequential client execution and rely on PostgreSQL's query planner and server-side parallelism. Use deterministic specification order for queries and outputs. Add bounded multi-connection execution only if the representative largest workload cannot complete within five minutes; parallel support must reuse the same analytical functions and must not introduce alternate results.

## Verification

Create a small independently checked fixture containing every supported variable type and the important edge cases: standard and sentinel missing values, all-missing variables, empty data, infinities, constant values, outliers, declared empty levels, unexpected levels, text, dates, datetimes, Unicode or non-syntactic names, and identifier duplication.

Run the fixture through both the data-frame and PostgreSQL paths and compare every canonical component. Expected values must be independently derived rather than generated by either production path. Test SQL quoting, invalid or missing sources, read-only operation, deterministic order, connection cleanup, safe output collisions, manifest integrity, and failure cleanup.

Reconcile representative SVG bins, counts, labels, and exclusions against the fixture and inspect the rendered files. Verify that neither returned objects nor written artifacts contain row-level or identifier values.

Before delivery, run focused tests, the complete package suite, documentation generation, lint, local package checks, and CRAN-style checks using the repository's documented R wrapper. Benchmark the largest Vive Feliz view as an external acceptance workload without copying its schema, dictionary, data, or terminology into the package.

## Exclusions

This implementation does not add Arrow, DuckDB, data.table, generic DBI dispatch, source-data preparation, database mutation, stratified summaries, cross-table analysis, HTML reporting, disclosure control, or project-specific orchestration.
