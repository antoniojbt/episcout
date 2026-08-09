# Test Design

Spec ID: `029-eda-reviewed-coordinate-roles`
Status: Review

## Test Files

- `tests/testthat/test-eda-geo.R`
- `tests/testthat/test-eda-geo-postgres.R`
- focused compatibility updates in existing specification, scaffold, intake, run and database-bundle tests

## Independent Fixture Strategy

Use a small neutral hand-authored table with two reviewed pairs and independently stated row classifications. Include complete rows, x-only missing, y-only missing, both missing, `NaN`, each infinity sign, EPSG:4326 boundaries and out-of-range values. Expected counts are written directly in tests rather than generated through production helpers. Use distinctive synthetic coordinate tokens only for byte-absence assertions and never place them in expected output, snapshots, errors or documentation.

Create an owned PostgreSQL schema and ordinary numeric table for live parity. Do not create PostGIS objects or call PostGIS functions. Instrument query kinds and fetched row counts to prove one aggregate row per pair and absence of coordinate collection.

## Baseline Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-(spec|api|intake|postgres-source|postgres-parity)', reporter = 'summary')"
scripts/check-workflow-state.sh
```

## Specification Tests

- [x] Older specifications without the three fields remain valid and produce a typed empty geo component.
- [x] Partial coordinate-field columns, unknown roles, orphan metadata, incomplete/duplicate roles, unequal CRS text, unresolvable CRS and non-numeric declared types fail before profiling.
- [x] Valid one- and multi-pair metadata normalize roles and whitespace while preserving pair and variable order.
- [x] Scaffolds add blank character `geo_role`, `geo_pair` and `geo_crs` fields for coordinate-like and ordinary names/values without inference.
- [x] CSV round trips preserve blank fields and reviewed values exactly.

## Data-frame Behaviour Tests

- [x] Public formals and fixed zero-row schema are exact.
- [x] Independent expected counts cover complete, each missing pattern, non-finite, boundary/range and zero-row cases.
- [x] Reviewed numeric sentinel codes contribute to missing counts and never appear in output.
- [x] Multiple pairs retain specification order and remain independent.
- [x] Missing/incompatible columns and unsupported storage fail with value-free conditions.
- [x] Inputs remain identical and no geometry, bbox, map, row index or coordinate vector appears in the returned object.

## PostgreSQL Tests

- [x] Data-frame and PostgreSQL results are identical for the neutral fixture.
- [x] Integer, numeric, real and double-precision source families follow the reviewed type contract.
- [x] One aggregate query returns one row per pair inside a read-only repeatable-read transaction.
- [x] Quoted/non-syntactic identifiers work; absent columns, incompatible types and catalogue drift fail closed.
- [x] The implementation neither checks for PostGIS nor uses spatial SQL.
- [x] Query failure rolls back owned work and leaves the caller connection open and idle.

## Workflow, Bundle And Privacy Tests

- [x] `epi_eda_run()`, `epi_eda_intake_run()` and `epi_eda_db_run()` return reconciled `geo` components and write `geo_qa.csv` with the declared sensitivity/checksum contract.
- [x] Existing non-geo component schemas and values remain unchanged.
- [x] Empty and blocked workflows retain truthful component/artifact states.
- [x] Overwrite validation accepts an exact prior new-contract bundle and rejects changed/unowned artifacts.
- [x] Serialized R returns plus every published CSV/HTML/SVG/manifest byte sequence omit distinctive coordinate values, WKT/WKB markers, bounds and feature maps.
- [x] Documentation says eligibility is neither disclosure approval nor scientific validation and routes feature-level work to explicit geo APIs.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-geo', reporter = 'summary')"
EPISCOUT_TEST_POSTGRES=1 PGHOST=127.0.0.1 PGPORT=55432 PGDATABASE=synthetic_records PGUSER=postgres scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-geo-postgres', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
scripts/check-local.sh
scripts/check-cran.sh
scripts/check-workflow-state.sh
git diff --check
```
