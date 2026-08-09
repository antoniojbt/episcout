# Test Design

Spec ID: `030-simplify-core-eda-controls-geo-outputs`
Status: Completed

## Test Files

- New `tests/testthat/test-eda-maps.R`
- New `tests/testthat/test-eda-maps-postgres.R`
- Update specification, scaffold, dictionary, preparation, summary, plot, stratification, intake, report and database-bundle suites
- Update linkage and live pseudonymisation suites

## Workflow Prerequisite

- [x] Online and offline workflow checks pass in the canonical checkout.
- [x] A fresh checkout whose actual path contains spaces passes both modes.
- [x] The audit detects malformed manifests and GitHub lifecycle drift.

## Breaking Schema Tests

- [x] Lean frame scaffolds have exactly 15 columns for ordinary, zero-column and factor/logical frames.
- [x] Storage-derived types and level metadata are deterministic and contain no observed-count/candidate evidence.
- [x] Removed scaffold fields receive a targeted migration error.
- [x] Extended PostgreSQL dictionaries omit four governance fields, contain all geo fields and round-trip them into EDA specs.
- [x] Catalogue metadata omits `validation_status`; explicit three-key profile selectors are unique, exact and bounded.
- [x] Old combined dictionaries, catalogue rows, linkage objects and six-column core manifests fail with migration guidance.

## Core Behaviour Tests

- [x] Preparation and stratification ignore roles and contain no review-state gates.
- [x] Identifier and coordinate variables receive declared summaries and ordinary plots for data frames and PostgreSQL.
- [x] Identifier and coordinate QA remain present and independently reconciled.
- [x] Intake-generated specifications continue through factual audit/complete/blocked outcomes and are saved/returned.
- [x] Core manifests have exactly five columns while specialised security manifests retain their existing schema.

## Independent Map Fixture

Use hand-authored frames rather than production helpers to state expected row classifications and rendered values. Cover valid EPSG:4326 boundaries, projected coordinates, x-only/y-only/both missing, declared sentinels, `NaN`, both infinities, out-of-range values, zero rows, multiple pairs, non-syntactic names, exactly 10,000 and 10,001 rows.

Themes cover numeric, integer, factor, logical, character, ordinary missing and declared missing codes. Expected pair/theme cross-products, map IDs, paths, counts and reasons are literal test fixtures.

## Data-frame Map Tests

- [x] Public formals, defaults, result components and typed empty inventory are exact.
- [x] `maps = FALSE` creates no objects, directory or SVG; `map_vars` then errors before profiling.
- [x] Invalid, duplicate, absent or temporal selectors and invalid bounds fail before processing.
- [x] One geometry map per ready pair and one thematic map per requested pair/variable are produced in deterministic order.
- [x] Failed QA, zero rows and over-limit pairs create no partial geometry/map/file and have `n_mapped = 0`.
- [x] Missing themes use the existing missing colour and text/categorical values are not collapsed.
- [x] Inputs are unchanged and rendered point/theme values reconcile independently.

## PostgreSQL Map Tests

- [x] Data-frame and PostgreSQL QA/inventory results match independently loaded fixtures.
- [x] No observation query occurs when maps are disabled, no pair is ready or the snapshot count exceeds the bound.
- [x] Collection occurs inside the same read-only repeatable-read snapshot as QA.
- [x] The projection contains only ready-pair coordinates and explicitly requested themes, with exact quoted identifiers.
- [x] Exactly 10,000 rows collect and 10,001 do not; no query truncates.
- [x] Snapshot changes, catalogue drift, incompatible types and query failures remain value-free and leave the connection idle/usable.
- [x] Map collection timings record the bound and row count without SQL or observations.

## Publication And Report Tests

- [x] Data-frame outputs, intake bundles and database bundles use deterministic `maps/` SVG paths.
- [x] Intake and database manifests include exact map checksums and reconcile every regular file.
- [x] Overwrite accepts an exact unchanged new-contract bundle and rejects old, changed, unowned, symlinked or settings-mismatched bundles.
- [x] Standard and intake HTML render geometry, numeric, categorical, missing-theme, empty and skipped-map states.
- [x] PostgreSQL bundles do not add HTML rendering.

## Security Tests

- [x] Linkage scaffolds/specs require exact column policy coverage and reject old three-component objects.
- [x] Policy retains current identifier bridge/drop, retained-column, record-key and validation guarantees.
- [x] Audit/apply, crosswalk, exact duplicate, conflict, lock, rollback, replacement and sensitive-diagnostic cases remain green.
- [x] Output dictionaries/catalogues have semantic schemas and pass directly into `epi_eda_dictionary_spec()`.
- [x] Security result manifests and restricted-data wording remain unchanged.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-(spec|dictionary|intake|prepare|summaries|stratified|geo|maps|report)', reporter = 'summary')"
EPISCOUT_TEST_POSTGRES=1 PGHOST=127.0.0.1 PGPORT=55432 PGDATABASE=synthetic_records PGUSER=postgres scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-(maps-postgres|geo-postgres|postgres-parity)|sec-(linkage|pseudonymise)', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
scripts/check-local.sh
scripts/check-cran.sh
scripts/check-workflow-state.sh
git diff --check
```
