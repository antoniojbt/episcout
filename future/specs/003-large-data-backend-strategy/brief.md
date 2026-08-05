# Brief

Spec ID: `003-large-data-backend-strategy`
Status: Active
Owner: Antonio Berlanga-Taylor

## Problem

The specification-first EDA interfaces operate on in-memory data frames. The package therefore cannot run its established schema, missingness, canonical summary and plot workflow against a large PostgreSQL relation without first collecting complete row-level data into R. That collection can exceed client memory, increase disclosure exposure and prevent work on restricted sources that must remain in the database.

## Goal

Add PostgreSQL 17 or later as the first server-backed EDA source while preserving ordinary data-frame behaviour, the six canonical summary components and the public profiling interfaces. Perform row-wise work in PostgreSQL, collect only bounded test vectors and aggregate plot/summary data, and provide a thin orchestrator that writes a privacy-classified, manifest-owned bundle.

## User Need

A user with a reviewed episcout specification and read access to one PostgreSQL table or view can run the same descriptive EDA contract used for a data frame, obtain traceable aggregate artifacts without materialising the source relation in R, and demonstrate that full rows and identifier values were not collected or written.

## Observable Outcome

- `epi_eda_postgres_source()` creates a validated, read-only reference to exactly one schema-qualified PostgreSQL relation without accepting SQL text or credentials.
- `epi_eda_check_schema()`, `epi_eda_profile_missing()`, `epi_eda_profile_summaries()` and `epi_eda_profile_plots()` accept either an ordinary data frame or that source object; data-frame calls retain their released arguments, return schemas and ordering.
- PostgreSQL results follow the current canonical definitions for every supported specification type, with documented numeric tolerances where server and R floating-point reduction order can differ.
- Explicit `id` and `identifier` roles produce aggregate identifier QA and no ordinary type summary or value-bearing plot.
- `epi_eda_db_run()` executes all database stages against one stable read-only snapshot and publishes a staged, checksummed aggregate-only bundle.
- Neutral parity fixtures provide independently reviewable correctness and privacy evidence, while a fixed synthetic PostgreSQL scale fixture guards against severe end-to-end performance regressions without claiming production representativeness.

## Success Measures

| ID | Measure | Acceptance |
| --- | --- | --- |
| M-001 | Canonical parity | Every neutral fixture row and component agrees exactly where representation is discrete and within the TDD tolerance where floating-point aggregation order may differ. |
| M-002 | Data locality | Instrumented tests show no unrestricted row-level fetch; every client result is one scalar row, a complete categorical frequency table, a fixed 30-bin table, or a Shapiro vector of at most 4,999 finite values. |
| M-003 | Privacy boundary | Returned objects, conditions, query diagnostics, SVGs and bundle files contain no source rows, raw text, observed identifier values, credentials, connection attributes or executable SQL. |
| M-004 | Snapshot consistency | Schema, counts, summaries, plot data and bundle metadata reconcile to one PostgreSQL repeatable-read, read-only snapshot. |
| M-005 | Synthetic scale runtime | After one warm-up, the median of three complete runs over the fixed one-million-row, eight-variable PostgreSQL 17 fixture is less than 120 seconds in the dedicated PostgreSQL CI job. This is a regression gate, not a production-runtime promise. |
| M-006 | Compatibility | The existing data-frame suite passes unchanged except for separately approved tests that make identifier exclusion and text-length plotting explicit for both backends. |

## Scope

- One caller-supplied PostgreSQL connection and one schema-qualified ordinary table, partitioned table, view, materialized view or foreign table.
- PostgreSQL catalogue inspection, safe identifier quoting, bound specification values, aggregate SQL, bounded collection, shared canonical builders and shared plot renderers.
- Schema, missingness, numeric/integer, categorical/binary, text, date/datetime, identifier QA and deterministic SVG artifacts.
- Sequential client execution; PostgreSQL may use its own query planner and server-side parallelism.
- Neutral unit fixtures, mandatory disposable PostgreSQL integration tests, a deterministic synthetic scale gate, documentation and package verification.

## Non-goals

- Arrow, DuckDB, data.table, SQLite, generic DBI or dbplyr dispatch.
- Database writes, source preparation or coercion, schema creation, indexes, grants, role management, query-plan tuning, server configuration, backup or log management.
- Stratified summaries, Table 1, correlations, cross-table analysis, pseudonymisation, HTML reporting, publication approval or disclosure control.
- Full-row collection, raw previews, raw text plots, identifier plots or automatic inference that a field is identifying.
- Multi-connection execution; failure of the synthetic scale gate requires investigation and owner review rather than approximation, sampling, concurrency or changed statistics.
- Project-specific data, names, schemas, dictionaries, credentials, fixtures, output conventions or terminology in the repository.

## Candidate Files

- `R/eda_postgres_source.R`
- `R/eda_postgres_queries.R`
- `R/eda_schema.R`
- `R/eda_missing.R`
- `R/eda_summaries.R`
- `R/summary_cores.R`
- `R/eda_plots.R`
- `R/eda_db_run.R`
- `R/eda_intake.R`
- `tests/testthat/test-eda-postgres-source.R`
- `tests/testthat/test-eda-postgres-parity.R`
- `tests/testthat/test-eda-postgres-benchmark.R`
- `tests/testthat/test-eda_plots-fixtures.R`
- `.github/workflows/r-cmd-check.yml`
- `.github/workflows/test-coverage.yaml`
- `README.md`
- `NEWS.md`
- `vignettes/specification-first-eda.Rmd`
- `man/`
- `NAMESPACE`

Candidate files are planning guidance, not permission for unrelated refactoring. Reuse the PostgreSQL validation and intake bundle helpers where their current contracts genuinely fit; extract shared filesystem internals only when tests first protect the existing intake behaviour.

## Principal Risks

- PostgreSQL and R may differ in type conversion, collation, character length, timestamp, infinity, quantile or floating-point reduction semantics.
- A naive implementation could collect high-volume or sensitive values, expose bound sentinels or identifiers through diagnostics, or log connection details.
- Independent queries under read-committed isolation could produce internally inconsistent artifacts while source rows change.
- Complete categorical frequencies can themselves be sensitive or too large even though they are aggregate output.
- Reusing manifest logic could accidentally broaden overwrite authority or regress the existing intake workflow.
- A synthetic timing gate could be misrepresented as evidence of production performance or become flaky if it lacks a fixed workload, dedicated uninstrumented job and substantial runner headroom.

## Assumptions And Open Questions

- M-005 establishes only a reproducible synthetic regression ceiling. Workload construction is excluded from timing; complete orchestration, SVG rendering, checksum validation, owned overwrite and publication are included. No claim is made about a particular production relation, host or runtime.
- PostgreSQL 17 is the minimum tested server because the existing repository integration path already uses PostgreSQL 17. Supporting later versions is expected but must be evidenced in recorded runtime metadata.
- PostgreSQL output privileges, network security, statement logging and aggregate disclosure review are caller/infrastructure responsibilities. The package must minimise client artifacts but cannot claim the server does not log statements or that small cells are safe to publish.
- No blocking design question remains. Any implementation need for approximate statistics, sampling, database mutation, generic DBI dispatch, multi-connection execution or a changed canonical schema requires owner review and a spec amendment before proceeding.
