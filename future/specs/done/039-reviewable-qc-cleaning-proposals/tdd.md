# Test Design

Spec ID: `039-reviewable-qc-cleaning-proposals`

Status: Completed

## Test Files

- new `tests/testthat/test-eda-qc-proposals.R`
- focused compatibility coverage in existing specification, dictionary, summary and PostgreSQL suites where necessary

## Independent Fixture Basis

Use one hand-authored neutral in-memory fixture and construct its PostgreSQL equivalent at test runtime. The key map uses opaque values such as `var_0000000000000001`; expected evidence and proposal rows are literal test data derived independently from the production path.

The fixture includes: exact `0`/`1` numeric and integer variables; an asymmetric finite numeric vector with one hand-calculated Tukey tail; a numeric variable containing infinity; a constant; declared-code and standard missingness; all-missing numeric and text columns; zero-row typed columns; incompatible declared/storage types; an explicitly declared identifier; high-cardinality neutral text; factor metadata; and a source value canary that must not appear in any result or condition.

For the finite vector `c(1, 2, 3, 100)`, independently fix type-7 `Q1 = 1.75`, `Q3 = 27.25`, `IQR = 25.5`, lower fence `-36.5`, upper fence `65.5`, zero below and one above. Do not call a package helper to create these expected values. Use a second asymmetric PostgreSQL parity vector if needed to distinguish interpolation or tolerance errors.

## Public Contract Tests

- [x] Public formals are exactly `data`, `spec`, `variable_keys` with no approval, apply, output or threshold argument.
- [x] The result has class `c("epi_eda_qc_proposals", "list")`, exact components `evidence` and `proposals`, and the exact fixed schemas/types in the SDD.
- [x] Evidence has exactly one row per specification row in specification order; proposals have at most one row per variable in the same relative order.
- [x] Every generated evidence state is `descriptive` and every generated proposal state is `pending`; no result field or value says approved or executable.
- [x] Empty specifications return both exact typed zero-row schemas.
- [x] Repeated calls return identical objects and the default print reports only class and table row counts.

## Variable-key Tests

- [x] Exact one-to-one name coverage links each specification row to its opaque key without returning the input `name` column.
- [x] Missing, extra, duplicate, reordered-duplicate, missing/blank, non-character and pattern-invalid key maps fail before profiling with fixed field-level errors.
- [x] Conditions never print names, keys or unmatched values, including recognisable canaries placed in every invalid input position.
- [x] The package does not derive keys from names; renaming a source/spec/key-map lookup while retaining the caller's opaque key retains the output key.
- [x] The same key map produces matching variable keys and ordering for equivalent data-frame and PostgreSQL sources.

## Evidence Tests

- [x] Literal counts reconcile `n_missing + n_observed = n`, `n_unique <= n_observed` and, for numeric/integer rows, `n_finite + n_infinite = n_observed`.
- [x] Standard missing values and reviewed missing codes contribute to missingness without exposing the code or excluded values.
- [x] Numeric extrema, fences and tail counts match independent literal expectations and use finite values only.
- [x] Semantic `min` and `max` values do not change observed extrema, fences or proposal fields and remain identical in the saved specification.
- [x] Supported zero-row and all-missing columns use the exact profiled codes, zero counts and typed unavailable statistics without inventing zero extrema.
- [x] Missing, incompatible and unsupported variables use fixed `not_profiled` codes and typed `NA` aggregates without observed classes or values.
- [x] Explicit reviewed identifier roles are not profiled and return no counts, extrema or proposal.
- [x] High-cardinality character/factor inputs return scalar counts only; no observed value, level, frequency row or example appears.
- [x] General numeric/integer, categorical, text and temporal variables never return value lists.

## Proposal-rule Tests

- [x] A profiled numeric or integer variable with exactly both 0 and 1 observed, no other non-missing value and no infinity receives `candidate_type = "binary"`, allowed levels `0;1` and the exact rationale code.
- [x] All-zero, all-one, all-missing, `{0, 1, 2}`, `{-1, 1}`, `{0, 1, Inf}` and incompatible cases do not receive a binary candidate.
- [x] The binary candidate leaves the semantic type and source values unchanged and suppresses a Tukey screening proposal even when class imbalance would put one level beyond a fence.
- [x] The independent tail fixture receives both exact fence prompts, `screening_basis = "tukey_1_5_iqr"` and one fixed rationale code.
- [x] A no-tail, empty, all-missing, infinite-only and exact-binary variable receives no screening bounds.
- [x] A zero-IQR fixture with a finite tail has inspectable bounds and counts under the fixed descriptive rule.
- [x] Numeric/integer/date/datetime rows with blank units receive `units_review_required = TRUE` and a blank candidate unit; reviewed units suppress only that prompt.
- [x] Candidate units and missing codes are blank for every generated row.
- [x] General observed categorical/text/integer values never populate candidate allowed levels; `0;1` is the only generated level string.
- [x] Infinity produces only the fixed non-finite rationale unless another independent prompt applies; it is never recoded or proposed as a missing code.
- [x] Rationale codes are deduplicated and use the fixed priority order.

## State, Non-mutation And Safety Tests

- [x] Evidence and proposal tables are separate objects and have different state fields; neither matches the future approved-rule shape described by issue-272.
- [x] Editing `proposal_state` after return has no package execution path and cannot be passed to any current preparation function as an executable rule.
- [x] Data frame, data.table, factor metadata, specification and key map remain identical after success and structural/per-variable failures.
- [x] Successful and failed calls create no files/directories, messages, warnings, cleaned objects or partial results.
- [x] The complete returned object, print/structure output and every captured condition exclude source names, relation identity, row canaries, categorical/text values, declared missing-code values, SQL and connection attributes.

## PostgreSQL Tests

- [x] A runtime-created neutral PostgreSQL relation produces identical discrete states, codes, counts, candidate strings and proposal decisions to its in-memory equivalent; numeric evidence matches the declared combined tolerance.
- [x] The PostgreSQL path uses one read-only repeatable-read snapshot, revalidates the catalogue and leaves the caller's connection open and idle after success and failure.
- [x] Query instrumentation proves every QC query returns one scalar row and no categorical-frequency, catalogue-value, Shapiro-vector, plot-data or general observation query is called.
- [x] High-cardinality text increases only `n_unique` and never client rows; exact `0`/`1` detection uses aggregate predicates only.
- [x] Zero-row, all-NULL, numeric infinity, unsupported types, nondeterministic collation, local timestamp and catalogue-drift cases have fixed safe outcomes.
- [x] Native notices and simulated database failures are sanitised and do not disclose schema, relation, column, SQL or values.
- [x] The source relation is byte/value equivalent before and after the operation, and no table, view, file or rule artifact is created.

## Compatibility And Documentation Tests

- [x] Existing lean scaffold, extended dictionary, canonical summary, preparation and PostgreSQL parity tests pass without weakened expectations.
- [x] Existing public function formals and semantic dictionary/specification schemas remain exact.
- [x] Roxygen, README, NEWS, vignette, generated help and NAMESPACE agree with the observed function and schemas.
- [x] Documentation labels observed extrema and Tukey fences as descriptive review signals, not scientific or executable validity limits.
- [x] Documentation shows neutral opaque keys, contains no project-specific names/results and states that aggregate output may still require disclosure review.

## Baseline Evidence

At canonical `master` `a1eabaf77907f31ed1f6af9c1a51a8867b82950a`, the focused offline dictionary, scaffold, canonical summary and PostgreSQL parity selection passes with the nine expected live-PostgreSQL skips. The local host has PostgreSQL 18.4 client tools but no responding server at `127.0.0.1:5432`; live baseline execution is therefore deferred to implementation verification or CI rather than represented as completed evidence.

## Focused Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-(qc-proposals|spec-scaffold|summaries|postgres-source)|db-dictionary', reporter = 'summary')"
EPISCOUT_TEST_POSTGRES=1 scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'eda-(qc-proposals|postgres-parity)', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
```

## Acceptance Commands

```bash
scripts/check-local.sh
scripts/check-cran.sh
scripts/check-workflow-state.sh
git diff --check
```

Record exact commands, PostgreSQL version, expectations, skips, warnings, direct object inspection, input/source reconciliation, privacy-canary searches and generated-file effects in `review.md`.
