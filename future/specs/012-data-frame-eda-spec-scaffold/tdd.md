# Test Design

Spec ID: `012-data-frame-eda-spec-scaffold`
Status: Completed

## Test Files

- `tests/testthat/test-eda-spec-scaffold.R`
- existing `tests/testthat/test-eda_spec-fixtures.R`
- existing `tests/testthat/test-eda-summaries.R`

## Independent Basis

Expected rows, types, counts, candidate classifications and error cases must be hand-authored from the approved SDD. Tests must not call the production scaffold or another production inference path to generate their expected values. Existing code and outputs establish compatibility context only.

## Baseline

On 2026-08-03, before spec 012 package-code changes, `scripts/check-local.sh` passed with zero errors, zero warnings and zero notes. The complete test suite had the two known environment skips, and generated artifacts affected by the check were restored after inspection.

## Behaviour Tests

- [ ] The exported function has formals `data` and `max_candidate_levels` with default `20L`.
- [ ] A mixed data frame returns the exact 21-column contract in fixed order and one row per source column in source order.
- [ ] Core fields use exact names, `label = name`, blank semantic strings, typed `NA` requiredness and conservative initial types.
- [ ] Integer, base numeric, logical, factor, ordered factor, character, Date, IDate, POSIXct and POSIXlt storage receive the approved initial types and full observed class strings.
- [ ] Logical and factor metadata populate safe core `levels`; `candidate_levels` remains blank for every v1 row and observed values are never enumerated.
- [ ] Factor level order and unused levels are preserved, including a safe literal `"NA"` level.
- [ ] Standard `NA` and `NaN` counts, observed counts and cardinalities match independently enumerated expectations; strings resembling sentinels remain observed.
- [ ] Numeric infinities remain observed, contribute to cardinality and prevent a whole-valued integer candidate.
- [ ] Whole finite base doubles receive an integer candidate without changing their numeric initial type.
- [ ] Exactly two observed integer or non-temporal character values within the configured threshold receive a binary candidate; other non-empty values within the threshold receive a categorical candidate without changing the initial type.
- [ ] Strict ISO date and datetime character vectors receive the matching temporal candidate and mixed or invalid calendar values do not.
- [ ] Candidate type precedence is deterministic and temporal candidates are not also labelled categorical.
- [ ] Every non-empty row is marked `review_required`, and reasons contain structural evidence but none of the observed test values.
- [ ] The result is an ordinary data frame, preserves non-syntactic names exactly and leaves the source object identical to a saved copy.
- [ ] The function emits no messages, warnings, files or directories during a successful in-memory call.

## Empty And Boundary Tests

- [ ] A zero-column data frame returns zero rows with the exact stable column types and passes `epi_eda_validate_spec()`.
- [ ] Zero-row supported columns retain their storage-derived initial type, report zero counts and receive no value-derived candidate.
- [ ] All-missing supported columns report their storage-derived type, zero observed and unique counts and no value-derived candidate.
- [ ] Constant integer and character columns use the documented non-empty low-cardinality policy rather than binary classification.
- [ ] `max_candidate_levels = 1L` and a value above observed cardinality produce the exact approved boundary results.
- [ ] Non-syntactic names and Unicode names survive output and CSV round-trip without repair.
- [ ] Datetime timezone evidence is class-safe and contains no observed timestamps.

## Failure Tests

- [ ] Non-data-frame input fails with an actionable data-frame error.
- [ ] Missing, zero-length, non-numeric, non-finite, non-whole and non-positive `max_candidate_levels` values fail clearly.
- [ ] Empty, whitespace-only, missing and duplicate source names fail before a scaffold is returned.
- [ ] List, nested-data-frame, matrix, complex, raw and arbitrary unsupported columns fail before a partial result.
- [ ] Decorated numeric classes such as `difftime` and a locally constructed labelled numeric class fail instead of being treated as base numeric.
- [ ] Every blocking column name and observed class appears in an aggregated structural error where multiple unsupported columns are present.
- [ ] Factor levels containing semicolons, empty strings, leading or trailing whitespace or missing metadata fail without printing the unsafe level itself.
- [ ] POSIXlt is accepted before generic list-like rejection.
- [ ] Failure leaves the source data unchanged and creates no filesystem output.

## Privacy And Encoding Tests

- [ ] Low-cardinality character and integer fixtures use recognisable sensitive-looking test tokens, and neither `candidate_levels`, `review_reason`, warnings nor errors contain those tokens.
- [ ] A factor with safe declared metadata exposes it only in core `levels`; unused declared levels retain their order and observations never add undeclared metadata.
- [ ] Delimiter-sensitive factor metadata is refused rather than escaped or silently split.
- [ ] No review reason includes observed minima, maxima, frequencies or example values.

## CSV And Integration Tests

- [ ] `utils::write.csv(..., row.names = FALSE)` followed by `epi_eda_spec()` preserves names, row order, core strings, logical requiredness and extra evidence columns for a representative safe scaffold.
- [ ] A safe factor whose sole metadata level is the literal `"NA"` uses the approved `NA;` representation and recovers the literal level through the supported round trip.
- [ ] After explicit human-equivalent edits to role, type, levels and requiredness, the reviewed draft passes `epi_eda_spec()` and `epi_eda_run()`.
- [ ] The integration result exposes the canonical `variables`, `numeric`, `categorical`, `text`, `temporal` and `skipped` components without changing the existing contract.
- [ ] Existing database dictionary scaffold and typed summary regression tests remain unchanged and pass.

## Documentation Tests And Inspection

- [ ] Roxygen output exports and documents `epi_eda_spec_scaffold()` with its privacy and review limits.
- [ ] README, vignette and NEWS use the public formals and observed output contract exactly.
- [ ] The vignette or an equivalent realistic invocation is executed and its draft, reviewed spec and canonical EDA result are inspected directly.
- [ ] Generated Rd and NAMESPACE changes come from roxygen rather than manual edits.

## Focused Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-spec-scaffold|eda_spec|eda-summaries', reporter = 'summary')"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::document()"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); lintr::lint_package()"
```

## Acceptance Commands

```bash
scripts/check-local.sh
scripts/check-cran.sh
git diff --check
```

Inspect the actual scaffold, its CSV round-trip, the reviewed specification and the canonical EDA result in addition to recording command exit codes. Record failures, skips, generated-file effects and any unverified platform behaviour in `review.md`.
