# Test Design

Spec ID: `005`  
Status: Implemented

## Test Files

- `tests/testthat/test-epi_sec_pseudonym.R`

## Baseline Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
```

## Behaviour Tests

- [x] Returns a tibble with columns `participant_id` and `token_id`.
- [x] Preserves participant ID order and row count.
- [x] Generates unique tokens for representative input.
- [x] Uses requested prefix and expected hex length.
- [x] Writes CSV output matching the returned bridge when `bridge_path` is supplied.

## Edge-case Tests

- [x] Accepts numeric participant IDs.
- [x] Accepts factor participant IDs.
- [x] Uses 192-bit tokens by default.
- [x] Allows overwriting only when `overwrite = TRUE`.

## Failure Tests

- [x] Errors on duplicate participant IDs.
- [x] Errors on missing participant IDs.
- [x] Errors when `n_bytes < 16`.
- [x] Errors when output file exists and `overwrite = FALSE`.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::document()"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
```
