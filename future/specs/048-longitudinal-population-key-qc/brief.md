# Brief

Spec ID: `048`
Status: Active
Owner: Antonio J. Berlanga-Taylor
Tracking issue: #346

## Problem

The package can audit identifiers in one PostgreSQL relation and compare an unordered metadata-declared identity universe, but it has no aggregate operation for population membership and optional record-key quality across caller-ordered completed periods.

## Goal

Add `epi_eda_longitudinal_qc(sources, entity_id, record_key = NULL)` as one PostgreSQL-only, aggregate-only, value-free operation. Caller list order defines period order. The complete result is calculated in one read-only repeatable-read snapshot and contains stable metadata, period, adjacent, all-pairs, history and technical-issue components.

## Non-goals

- Do not resolve identities, pseudonymise data, construct completed periods, clean data, inspect variable drift or make scientific judgements.
- Do not change identifier QC, identity-universe, pseudonymisation, general EDA or PostgreSQL-source contracts.
- Do not collect or return entity identifiers, tokens or record-key values.
- Do not create database or local output objects.

## Candidate Files

- `R/eda_longitudinal_qc.R`
- `tests/testthat/test-eda-longitudinal-qc.R`
- `tests/testthat/test-eda-longitudinal-postgres.R`
- `README.md`
- `vignettes/longitudinal-pseudonymisation.Rmd`
- `PROJECT_MAP.md`
- `.github/workflows/r-cmd-check.yml`

## Risks

- Ordering mistakes would change retention, entry, exit and gap histories.
- Direct bigint conversion above base R's exact-integer double boundary would silently lose count precision.
- Native database failures or returned identifiers could cross the value-free boundary.
- Partial validation or multiple snapshots could make period comparisons internally inconsistent.

## Successor or Terminal Outcome

- Successor issue: not selected in this checkout-only implementation.
- Terminal reason if no successor is needed: to be decided through the live issue and pull-request record.
