# Test Design

Spec ID: `001-phase-1-helper-stabilization`  
Status: Ready for activation  

## Baseline Commands

Run before implementation where practical:

```bash
scripts/rscript_env_caller.R -e "devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::check(manual = FALSE)"
scripts/rscript_env_caller.R -e "covr::package_coverage()"
```

## Test Files

- `tests/testthat/test-epi_stats_numeric.R`
- `tests/testthat/test-stats.R`

Use a narrower new test file only if it makes the edge-case contracts clearer.

## Required Tests

- [ ] `epi_stats_numeric()` handles all-missing numeric vectors.
- [ ] `epi_stats_numeric()` handles zero-length numeric vectors.
- [ ] `epi_stats_numeric()` returns stable coefficient-of-variation output when
      mean is zero.
- [ ] `epi_stats_numeric()` handles insufficient usable values for normality,
      skewness and kurtosis.
- [ ] `epi_stats_na_perc()` rejects invalid `margin` with a clear error.
- [ ] `epi_stats_na_perc()` handles mixed-type data frames without
      coercion-related count errors.
- [ ] `epi_stats_na_perc()` keeps stable row-mode and column-mode return shapes.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "testthat::test_file('tests/testthat/test-epi_stats_numeric.R')"
scripts/rscript_env_caller.R -e "testthat::test_file('tests/testthat/test-stats.R')"
scripts/rscript_env_caller.R -e "devtools::test(reporter = 'summary')"
```
