# Test Design

Spec ID: `020-data-frame-writer-delimiter-contract`
Status: Implemented

## Test Files

- `tests/testthat/test-epi-write-df.R`

## Baseline Evidence

The existing focused tests passed. A direct byte probe established the defect:
`suffix = "csv"` produced `a\\tb\\n1\\t2\\n`.

## Behaviour Tests

- [x] CSV and TSV extensions produce comma and tab bytes respectively.
- [x] Upper-case recognized suffixes use the corresponding delimiter.
- [x] The function returns the platform-native expected path and emits its
  existing success message.
- [x] Rewriting the same path replaces the prior content.
- [x] Missing values, headers, row names and quoting retain documented defaults.

## Edge-case Tests

- [x] A zero-row data frame still writes its header.
- [x] A missing output directory fails clearly and is not created.

## Failure Tests

- [x] Unsupported, empty, missing or multi-value suffixes fail.
- [x] Empty, missing, multi-value or path-bearing filenames fail.
- [x] Contradictory or malformed explicit separators fail.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'epi-write-df', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/check-local.sh
git diff --check
```
