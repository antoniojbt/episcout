# Test Design

Spec ID: `<id>`  
Status: Draft  

## Test Files

- `tests/testthat/test-<feature>.R`

## Baseline Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
```

## Behaviour Tests

- [ ] TBD.

## Edge-case Tests

- [ ] TBD.

## Failure Tests

- [ ] TBD.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
```
