# Test Design

Spec ID: `048`
Status: Completed

## Test Files

- `tests/testthat/test-eda-longitudinal-qc.R`
- `tests/testthat/test-eda-longitudinal-postgres.R`

## Independently Justified Fixture

A runtime-generated neutral four-period entity set will encode memberships by fixture indices, then generate non-retained text identifiers at execution time. Hand-derived sets define the expected adjacent retention, entry and exit counts; all six pair intersections; one-period, repeated, persistent and gapped/reappearing histories; and simple/composite complete-key duplicates. Null and blank entities and incomplete keys are added as explicitly excluded rows. Expected aggregates are stated directly in tests rather than calculated through production helpers.

## Behaviour Tests

- [x] Exact class, component order, data-frame column order and scalar types are stable.
- [x] Reversing named source order changes period positions and directional adjacent evidence while preserving the corresponding undirected overlap counts.
- [x] Period row/entity summaries and simple/composite record-key evidence match hand-derived expectations.
- [x] Adjacent retention, exits and entries use explicit source/target denominators.
- [x] Pairwise output includes adjacent and non-adjacent increasing pairs in deterministic index order.
- [x] History output covers persistent, one-period, repeated, gapped and reappearing entities without returning values.
- [x] Null and applicable blank entities are excluded from membership; incomplete keys are excluded from key distinct/duplicate counts.
- [x] Empty-period and zero-denominator results use typed zeroes and `NA_real_` proportions.
- [x] Only the four frozen warning issue codes occur, in deterministic order and the typed seven-column schema.
- [x] Repetition is deterministic, creates no database objects and leaves the caller connection open and idle.
- [x] A controlled second connection proves one stable repeatable-read snapshot, and a forced query failure proves rollback and connection reuse.

## Boundary And Compatibility Tests

- [x] Exact decimal counts accept `2^53 - 1` and reject `2^53` before conversion.
- [x] Unnamed, singly sized or duplicate-labelled lists; modified sources; mixed connections; missing/unsupported/incompatible columns; and nondeterministic textual collations fail hard.
- [x] Ordinary, partitioned, view, materialised-view and foreign-table source objects can participate without broadening the identity-universe API.
- [x] Temporary relations and catalogue drift continue to fail through the existing source contract.
- [x] Quoted identifiers are handled safely.
- [x] Returned objects, printed output and sanitised failures contain no runtime-generated entity or record-key values.
- [x] Existing public APIs and scalar-count helpers remain unchanged.

The PostgreSQL behaviours above are executable integration tests gated by `EPISCOUT_TEST_POSTGRES=1`. They passed locally against a disposable PostgreSQL/PostGIS 17 container; hosted PostgreSQL execution remains required integration evidence.

## Acceptance Commands

```bash
scripts/check-workflow-state.sh --offline
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-longitudinal', reporter = 'summary')"
scripts/check-local.sh
```
