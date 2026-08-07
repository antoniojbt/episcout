# Review Notes

Spec ID: `019-postgresql-catalogue-missingness`
Status: Implemented

## Root Cause

The cardinality query used `COUNT(DISTINCT column)`, which excludes PostgreSQL
NULL, while the grouped frequency query included NULL as an additional
`source_value = NA` row. The gate and output therefore counted different
domains, and the normalised catalogue validator correctly refused that missing
source value.

## Findings

No unresolved local implementation finding. Profiling now returns a fixed
`epi_db_catalogue_profile` list. `values` contains only non-NULL source values
and counts; `missing` contains one aggregate `n_missing` row per selected source
column. Empty and all-NULL sources retain the missing summary without inventing
a catalogue value. Existing eligibility/privacy gates and `max_levels` semantics
remain unchanged.

The result-shape change is deliberate and limited to the pre-release database
dictionary API. Help, NEWS, the longitudinal guide and the runnable walkthrough
show the separate review paths. PostgreSQL NULL is not automatically converted
into a catalogue missing code.

## Verification Evidence

- The pre-code `db-dictionary` suite passed and source inspection independently
  established the mismatched PostgreSQL NULL domains.
- Focused dictionary tests pass exact class/component/column schemas,
  non-missing values, aggregate missing counts, all-NULL output, SQL filtering,
  limit refusal, privacy gates and empty selections.
- The PostgreSQL integration test is discovered locally and skips only because
  the environment has no PostgreSQL client/service or container runtime.
- The live test is named for the mandatory `eda-postgres` CI filter and covers
  zero rows, all NULL, exact limit plus NULL, and limit plus one on PostgreSQL 17.
- The first PR PostgreSQL 17 job passed all live cases in 3m09s. Ubuntu and
  macOS stopped only at the newer CI linter's 30-character limit for one private
  test-helper name; shortening that name changed no behavior and was followed by
  focused local tests and lint before the replacement CI run.
- Package-loaded lint returned no findings.
- The full package suite passed with expected environment-dependent skips.
- `scripts/check-local.sh` exited successfully; its known generated-help and
  skipped-snapshot cleanup side effects were restored exactly.
- `git diff --check` passed.

## Closeout Notes

Software verification found direct public-boundary unit tests and independent
live PostgreSQL assertions. Truth/semantics review confirmed that NULL remains
absence rather than a source catalogue value, missing counts remain exact
aggregates and the non-missing limit retains its documented meaning. Copy-edit
and render/release review aligned help, NEWS, the longitudinal guide and the
runnable walkthrough. Mandatory PostgreSQL execution and pull-request CI remain
open, so this implemented spec stays active until owner acceptance and merge.
