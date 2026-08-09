# Test Design

Spec ID: `022-postgresql-eda-row-count-reuse`
Status: Implemented

## Test Files

- `tests/testthat/test-eda-postgres-source.R`
- `tests/testthat/test-eda-postgres-parity.R`

## Baseline Evidence

- Package-loaded `eda-postgres` tests passed on 2026-08-09 with the ten gated live/benchmark cases skipped as expected.
- The complete live PostgreSQL 18.4 parity selection passed against a disposable local container before code changes.
- Source inspection confirms that `epi_eda_db_run()` obtains and passes `n_total`, while `eda_pg_categorical_summary()` executes another `eda_postgres_row_count()` for each categorical/binary variable.

## Baseline Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-postgres', reporter = 'summary')"
EPISCOUT_TEST_POSTGRES=1 PGHOST=127.0.0.1 PGPORT=55432 PGDATABASE=synthetic_records PGUSER=postgres scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-postgres-parity', reporter = 'summary')"
```

## Behaviour Tests

- [x] A mocked internal categorical summary accepts `n_total`, does not call `eda_postgres_row_count()`, and preserves exact counts and proportions.
- [x] An exported live PostgreSQL run with one categorical and one binary variable records exactly one `row_count` timing entry.
- [x] Existing independently stated categorical/binary counts, `p_total`, `p_observed`, declaration flags and canonical variable counts remain exact.
- [x] Existing bundle artifacts, checksums, identifiers, plots and overwrite protections remain compatible.

## Edge-case Tests

- [x] The existing zero-row storage matrix retains declared zero-count categorical/binary rows and unavailable proportions.
- [x] Multiple categorical/binary variables do not increase the row-count timing count.
- [x] Direct summary profiling retains its single fallback row-count query.

## Failure Tests

- [x] The unit test replaces `eda_postgres_row_count()` with a failure sentinel, proving the categorical helper does not invoke it.
- [x] Existing transaction cleanup and reconciliation failure tests remain green.

## Results

- Before implementation, the unit test failed because the helper rejected the supplied `n_total`, and the live exported run observed three `row_count` entries for one categorical plus one binary variable.
- After implementation, both focused suites passed and the same exported run observed exactly one `row_count` entry while retaining its existing exact aggregate and bundle assertions.
- The gated PostgreSQL 18.4 million-row benchmark, catalogue, EDA and identity-universe suites passed. The unrelated longitudinal pseudonymisation suite reproducibly retained two registry-metadata errors on PostgreSQL 18.4; the branch does not touch that workflow and upstream CI currently runs PostgreSQL 17.
- Package-loaded lint returned no findings. `scripts/check-local.sh` completed with 0 errors, 0 warnings and 0 notes. Generated documentation and disabled visual-snapshot cleanup side effects were restored.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-postgres-source', reporter = 'summary')"
EPISCOUT_TEST_POSTGRES=1 PGHOST=127.0.0.1 PGPORT=55432 PGDATABASE=synthetic_records PGUSER=postgres scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-postgres-parity', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
scripts/check-local.sh
git diff --check
```
