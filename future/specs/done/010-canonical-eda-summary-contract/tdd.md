# Test Design

Spec ID: `010-canonical-eda-summary-contract`  
Status: Completed

## Test Files

- Rename or replace `tests/testthat/test-eda-summaries-v2.R` with an unversioned canonical-summary test file.
- Update `tests/testthat/test-summary-cores.R` and `tests/testthat/test-epi_stats_numeric.R` for the authorised no-observed-total correction.
- Update `tests/testthat/test-eda_summaries-fixtures.R` and `tests/testthat/test-penguins-raw-fixtures.R` only where their expectations are independently justified.
- Update `tests/testthat/test-run_eda-fixtures.R` and `tests/testthat/test-eda_report.R` for the single output contract.
- Extend `tests/testthat/test-db-dictionary.R` with dictionary-to-summary integration behaviour.
- Retain focused regression coverage for `epi_stats_summary(output = "current")` and `output = "typed"`, permitting only the specified all-missing numeric sum correction in current numeric output.

## Baseline Commands

Run and record these before package-code changes:

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-summaries|run_eda|eda_report|db-dictionary', reporter = 'summary')"
scripts/check-local.sh
```

## Independent Expected Values

Use small hand-authored data and explicit calculations rather than generating expected fixtures from the production summary functions. At minimum, independently verify these cases:

- A numeric vector containing finite values, a sentinel, `NA` and infinity, reconciling `n`, `n_missing`, `n_observed`, `n_infinite`, `n_finite`, sum, location, spread, fences, outlier numerator and denominator.
- An all-missing and an infinite-only numeric vector, requiring `sum = NA` and unavailable analytical statistics rather than zero.
- A categorical vector with declared observed levels, a declared zero-count level, an unexpected observed value, a configured sentinel and `NA`, reconciling counts, `p_total` and `p_observed` by hand.
- A factor containing an unused factor level absent from the specification, proving that factor metadata does not create a declared or observed result row.
- Text values containing an empty string, whitespace-only string, ordinary text, a sentinel and `NA`.
- Strict date and datetime examples with independently known minima, quantiles, maxima, timezone display and ranges.

## Behaviour Tests

- [ ] `epi_eda_profile_summaries(data, spec)` has no version argument and returns exactly `variables`, `numeric`, `categorical`, `text`, `temporal` and `skipped` in documented order.
- [ ] Every specification row appears once in `variables` and exactly once across its successful type component or `skipped` outcome.
- [ ] Component columns, types and zero-row schemas are stable and explicitly asserted.
- [ ] `epi_eda_run()` always returns the canonical summary and writes exactly the six deterministic summary CSV files when requested.
- [ ] `epi_eda_render_report()` presents canonical coverage, skipped reasons, denominator and unit context, renders every non-empty component, identifies empty components explicitly and contains no version selection.
- [ ] `epi_stats_summary(output = "typed")` uses the same canonical component semantics with global sentinels, while `output = "current"` retains its class/action-specific schemas and unrelated values.
- [ ] `epi_stats_numeric()` and numeric current-mode summary output return `sum = NA` when no finite observations exist and retain the established output schema.
- [ ] A specification produced by `epi_eda_dictionary_spec()` flows into the canonical summary with its order, declared levels and missing codes preserved.

## Edge-case Tests

- [ ] Zero-row, all-missing, sentinel-only, infinite-only, constant and single-finite-value numeric inputs.
- [ ] Zero denominators, declared zero-count levels, unexpected levels, literal `"NA"`, unused factor metadata and non-syntactic names.
- [ ] Missing required and optional variables, with unavailable counts and explicit reasons.
- [ ] Incompatible numeric, categorical, text and temporal observed classes.
- [ ] `Date`, `IDate`, `POSIXct`, `POSIXlt`, supported ISO character input and invalid non-missing temporal strings.
- [ ] Empty and whitespace-only text retained as observed unless explicitly configured as missing.

## Failure Tests

- [ ] Non-data-frame input, malformed or duplicate specification rows and invalid specification types fail clearly.
- [ ] Invalid output directories fail before writing partial workflow output.
- [ ] Unsupported top-level requests fail without silently selecting an alternate summary contract.
- [ ] Invalid non-missing temporal input appears as a skipped variable with a reason rather than being counted as missing.

## Documentation And Static Checks

- [ ] Active `R/`, `tests/testthat/`, `README.md`, `NEWS.md`, `vignettes/`, `inst/report-template/` and generated `man/` contain no active EDA v1/v2 or `summary_version` interface references; historical completed specifications may retain their record of prior decisions.
- [ ] Roxygen usage and return documentation match the observed public formals and component schemas.
- [ ] README and vignette examples execute the single canonical path.
- [ ] Report values are traced to the returned canonical object, with display-only formatting tested separately from calculation.

## Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-summaries|run_eda|eda_report|db-dictionary', reporter = 'summary')"
scripts/check-local.sh
scripts/check-cran.sh
git diff --check
```

No tag or release command belongs to this specification.
