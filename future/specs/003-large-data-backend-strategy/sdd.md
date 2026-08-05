# Software Design

Spec ID: `003-large-data-backend-strategy`
Status: Draft; revision required before activation

## Scope

Define a PostgreSQL-first extension to specification-first EDA that preserves the current data-frame/tibble interfaces and canonical output contracts without collecting complete row-level datasets into R. `future/scratch/episcout_postgresql_backend_plan.md` is the current design input; this draft must be expanded and reviewed before activation.

## Design Direction

- Keep data frames and tibbles as the baseline backend and preserve the established `epi_eda_check_schema()`, `epi_eda_profile_missing()`, `epi_eda_profile_summaries()` and `epi_eda_profile_plots()` interfaces.
- Add PostgreSQL as the only first backend and use a small `epi_eda_db_run()` orchestrator only for source validation, composition and owned artifact writing.
- Share statistical definitions, canonical output schemas, specification validation, plot renderers and artifact policy between data-frame and PostgreSQL paths.
- Restrict PostgreSQL-specific code to catalogue inspection, safely quoted or bound aggregate SQL and connection handling.
- Keep row-level data, identifiers, credentials and connection details out of returned and written artifacts.
- Begin with sequential client execution and rely on PostgreSQL planning and server-side parallelism; consider bounded multi-connection execution only if the approved external workload misses the reviewed threshold.

## Required Revision Sequence

1. Fix the source contract, supported PostgreSQL types, canonical parity rules and identifier-QA contract.
2. Define aggregate SQL, bounded-collection and missing-sentinel semantics for every supported specification type.
3. Define compact plot-data contracts shared by both backends and prohibit raw identifier plots.
4. Define the owned output bundle, staged writes, manifest, collision handling and failure cleanup.
5. Define independently calculated parity fixtures, privacy checks and the external performance acceptance method.
6. Record baseline verification and obtain owner review before activating implementation.

## Compatibility

Existing calls on ordinary data frames must remain compatible. PostgreSQL output must use the existing `variables`, `numeric`, `categorical`, `text`, `temporal` and `skipped` components and the established statistical definitions. Backend differences must be explicit rather than hidden behind approximate results.

## Out Of Scope

No package implementation is authorised by this draft. Arrow, DuckDB, data.table, generic DBI dispatch, database mutation, source preparation, stratified summaries, cross-table analysis, HTML reporting, disclosure control and project-specific orchestration remain outside the first PostgreSQL implementation.
