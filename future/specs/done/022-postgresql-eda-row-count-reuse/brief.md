# Brief

Spec ID: `022-postgresql-eda-row-count-reuse`
Status: Active
Owner: repository-owner

## Problem

`epi_eda_db_run()` obtains one relation-wide row count inside its read-only repeatable-read transaction and passes it to the summary workflow. Each categorical or binary variable nevertheless calls `eda_postgres_row_count()` again solely to calculate `p_total`. A run with `k` categorical/binary variables therefore records `1 + k` row-count queries against the same transaction snapshot.

The value-free investigation in `future/scratch/episcout_postgres_eda_performance_issue.md` observed 38 redundant recounts across three approved relations. Results remained correct; this is a bounded noncritical performance defect.

## Goal

Pass the already checked transaction-local `n_total` into the internal categorical-summary helper and remove its recount. One exported database EDA run must record exactly one `row_count` timing entry while preserving every aggregate result and publication contract.

## Non-goals

- Consolidating missingness, basic-count, typed-summary, identifier-QA, plot or catalogue queries.
- Changing public function signatures, output schemas, snapshots, privacy boundaries or bundle ownership.
- Making portable runtime or speedup claims from private workloads.
- Changing data-frame EDA, report rendering, CURP behaviour or CRAN work.

## Candidate Files

- `R/eda_postgres_queries.R`
- `tests/testthat/test-eda-postgres-source.R`
- `tests/testthat/test-eda-postgres-parity.R`
- `future/README.md`
- `future/TODOs.md`
- `future/changelog.md`

## Risks

- Passing the wrong denominator would change `p_total` and inferred missing counts.
- Removing a timing entry without removing the query would make instrumentation untruthful.
- Broad query refactoring could unintentionally change failure isolation or aggregate-only collection.
