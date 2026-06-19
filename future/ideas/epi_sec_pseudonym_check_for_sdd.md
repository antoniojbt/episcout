# Add `epi_sec_pseudonym()` Secure Bridge Helper

## Summary

Add a narrow v1 security helper that creates a pseudonymisation bridge table only: `participant_id <-> token_id`. It will use cryptographic random bytes via `openssl`, generate non-derivable tokens, optionally write the bridge file, and document that the bridge remains re-identifying data that must be stored separately.

Because this is security-sensitive and already listed in `future/TODOs.md`, first create a numbered spec, e.g. `future/specs/005-pseudonymisation-bridge/`, before package-code changes.

## Public API

Add exported function:

```r
epi_sec_pseudonym(
  participant_id,
  n_bytes = 24,
  prefix = "P",
  bridge_path = NULL,
  overwrite = FALSE
)
```

Behavior:

- Accept character, numeric, or factor participant IDs.
- Require non-missing, unique participant IDs.
- Require `n_bytes >= 16`; default `24` gives 192-bit tokens.
- Generate lowercase hexadecimal token IDs as `"{prefix}_{hex}"`.
- Return a tibble with columns `participant_id` and `token_id`, preserving input order.
- If `bridge_path` is supplied, write CSV with `utils::write.csv(..., row.names = FALSE)`.
- Refuse to overwrite existing bridge files unless `overwrite = TRUE`.
- Do not support seeds or deterministic output.

Add `openssl` to `Imports`.

## Implementation Changes

- Add `R/epi_sec_pseudonym.R` with roxygen2 docs, examples, and explicit security notes.
- Add `@importFrom openssl rand_bytes` and `@export`.
- Regenerate `NAMESPACE` and `man/epi_sec_pseudonym.Rd` with `devtools::document()`.
- Update `NEWS.md` with the new helper.
- Update `future/TODOs.md` to reference the activated spec and narrow v1 scope.

## Test Plan

Add `tests/testthat/test-epi_sec_pseudonym.R` covering:

- Returns a tibble with expected columns and same row count/order as input.
- Tokens are unique for a representative input vector.
- Tokens use the requested prefix and expected hex length.
- Default token length is `2 * n_bytes` hex characters after the prefix separator.
- Errors on duplicate participant IDs.
- Errors on missing participant IDs.
- Errors when `n_bytes < 16`.
- Optional `bridge_path` writes a CSV matching the returned bridge table.
- Existing file is not overwritten unless `overwrite = TRUE`.

Run:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::document()"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::check(manual = FALSE)"
```

## Assumptions

- v1 does not pseudonymise full datasets, detect PII columns, remove identifiers, or write analysis datasets.
- v1 does not implement chunked bridge generation or database-backed bridge storage.
- The bridge file is treated as sensitive re-identifying data; documentation will say it must be access-controlled and stored separately from analysis data.
