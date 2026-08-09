# Test Design

Spec ID: `031-canonical-eda-delivery`  
Status: Active

## Test Files

- New `tests/testthat/test-eda-db-report.R`
- Extend database run, overwrite, map and live PostgreSQL parity suites
- Update API, README, NEWS and installed-walkthrough checks

## Baseline Commands

```bash
scripts/check-workflow-state.sh
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-(postgres-parity|maps-postgres|report)|db-dictionary', reporter = 'summary')"
```

## Public And Compatibility Tests

- [ ] Exact new formals/defaults are documented; current data-frame renderer formals remain unchanged.
- [ ] Default database runs retain their existing result, manifest and relative-path schemas.
- [ ] Delivery mode returns the same analytical tables/counts and the exact canonical tree.
- [ ] Layout and render settings participate in overwrite fingerprints;
  default bundle metadata schemas remain exact while delivery metadata records
  the new contract separately.

## Validation And Atomicity Tests

- [ ] A valid result object and relocated bundle path render identically without a connection.
- [ ] The renderer rejects missing/incomplete status, old sensitivity manifests, malformed five-column manifests, duplicate artifacts/paths, rooted/traversal/backslash paths, case collisions, symlinks, non-regular files, missing/unowned files, checksum drift and incompatible CSV schemas.
- [ ] Mocked template, parse, render, checksum, stage-swap and restore failures preserve every original file and checksum.
- [ ] Existing reports require explicit overwrite; unrelated files are never adopted or removed.
- [ ] Database/DBI calls are mocked to fail if invoked during rendering and remain uncalled.

## Artifact And Report Tests

- [ ] Every retained file appears once in the manifest; all non-manifest checksums reconcile.
- [ ] README identifies the report first and explains each folder without governance language.
- [ ] HTML independently reconciles representative metadata, missingness, summary, QA, plot/map inventory and timing cells against literal fixture expectations.
- [ ] Geometry, numeric/categorical/missing-theme and skipped map states use only owned relative SVG paths.
- [ ] Moving the complete root preserves all links; moving the HTML alone is not promised.
- [ ] Plot-data CSVs reconcile to compact aggregate plot inputs and exclude map coordinates/themes and source rows.
- [ ] Repeated rendering from identical inputs produces stable paths/content apart from explicitly documented renderer metadata; no external assets or absolute paths occur.

## Live PostgreSQL Tests

- [ ] Delivery mode performs the same read-only repeatable-read query set and aggregate/map bounds as default mode.
- [ ] Rendering begins only after the snapshot closes and performs no later database calls.
- [ ] A full synthetic delivery bundle and a rendered existing flat bundle pass exact ownership, relocation and visual artifact inspection.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-(db-report|postgres-parity|maps-postgres)|db-dictionary', reporter = 'summary')"
EPISCOUT_TEST_POSTGRES=1 scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-(db-report|postgres-parity|maps-postgres)', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
scripts/check-local.sh
scripts/check-cran.sh
scripts/check-workflow-state.sh
git diff --check
```
