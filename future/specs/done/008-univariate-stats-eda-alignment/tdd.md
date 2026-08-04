# Test Design

Spec ID: `008-univariate-stats-eda-alignment`  
Status: Implemented  

## Test Files

- `tests/testthat/test-summary-cores.R`
- `tests/testthat/test-eda-summaries-v2.R`
- Existing univariate statistics, EDA workflow and report tests.

## Baseline

- On 2026-07-25 the full package test suite passed with the two known environment/snapshot skips and no failures or warnings.
- On 2026-07-25 `devtools::check(manual = FALSE)` completed with 0 errors, 0 warnings and the existing NOTE for bundled project-template `.gitkeep` files.

## Behaviour Tests

- [x] EDA v1 retains exact list names, schemas, values and file inventory.
- [x] EDA v2 covers numeric, integer, categorical, binary, text, date and datetime variables.
- [x] Every specification variable is summarised or represented in `skipped` with a reason.
- [x] Typed `epi_stats_summary()` returns the v2 components while current mode remains compatible.
- [x] Existing public wrapper schemas remain unchanged.

## Edge-case Tests

- [x] Sentinel codes, `NA`, `NaN`, infinities, empty/all-missing vectors, zero rows, constants and zero denominators.
- [x] Declared zero levels, unexpected levels, literal `"NA"`, empty and whitespace-only text.
- [x] Date, IDate, POSIXct, POSIXlt and ISO character temporal values, timezones and invalid values.
- [x] Non-syntactic and missing variable names and incompatible observed classes.
- [x] V2 CSV round-tripping and populated/empty report sections.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); testthat::test_file('tests/testthat/test-eda-summaries-v2.R', reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::document()"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); lintr::lint_package()"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
```
