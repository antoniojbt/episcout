# Test Design

Spec ID: `009-repository-lint-style-cleanup`  
Status: Implemented  

## Baseline

- Package-loaded lint: 163 findings.
- Unloaded-package lint: 319 findings, including 156 cross-file false positives.
- Full tests and package check passed at spec 008 closeout with two known skips and the existing `.gitkeep` NOTE.

## Behaviour Tests

- [x] Every current `epi_stats_summary()` class/action mode retains its schema and values.
- [x] Correlation outputs retain `Var1` and `Var2` schemas.
- [x] Plotting, reading/writing, palettes and sub-sampling retain public contracts.
- [x] No native pipe or minimum-R build warning is introduced.

## Static Gates

- [x] Package-loaded `lintr::lint_package()` returns zero findings.
- [x] Targeted styler dry run is stable for the 33-file cleanup set.
- [x] `git diff --check` passes.
- [x] Local and CI lint commands fail on a non-empty lint result.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); stopifnot(length(findings) == 0L)"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
```
