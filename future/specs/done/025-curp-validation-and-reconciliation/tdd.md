# Test Design

Spec ID: `025-curp-validation-and-reconciliation`
Status: Completed and accepted through PR #231

## Baseline Evidence

- `devtools::test(filter = "curp_misc")` passed 20 existing expectations on 2026-08-09.
- A direct two-element call to the documented vector interface failed with `the condition has length > 1`.
- An 18-character non-numeric value passed the length gate, emitted a coercion warning and returned sliced fields with an unavailable year.
- Existing tests cover only two example extractions and one short-input error. They do not establish official structure, check digit, vector behaviour, impossible dates, century boundaries, missing values or value-free diagnostics.

## Proposed Test Files

- `tests/testthat/test-curp-validation.R`
- `tests/testthat/test-curp_misc.R`

## Independent Fixture Rules

- Structural success fixtures are independently constructed from the accepted position classes, supported date domain and pinned catalogue; their final digit is syntactically numeric but is not asserted as a valid checksum.
- Production code and tests do not calculate expected check digits in this slice.
- No fixture may be copied from user data, a real record, screenshots or an uncited internet generator.
- Fixtures that could be valid assigned identifiers are treated as restricted even when synthetic-looking; tests and failures must never print them.

## Contract Tests

- [x] Public formals and fixed result component/column schemas are exact.
- [x] Scalar, empty, length-one and vector inputs preserve one output record per element and stable `input_index` alignment.
- [x] Missing input is distinct from malformed input.
- [x] Lowercase, whitespace, punctuation, Unicode and wrong-length behaviour matches the owner-approved normalisation rule.
- [x] Every position class is tested independently without reusing production validation logic.
- [x] Position 17 follows the approved year-domain contract, including 1999/2000 boundaries and explicit pre-1900 behaviour.
- [x] Impossible dates, leap-year boundaries and future dates follow the accepted contract.
- [x] `H`, `M`, `NE` and every pinned birthplace code reconcile to the official source; unknown codes fail deterministically.
- [x] Every structurally valid record reports checksum status `not_verified`; no local checksum claim or calculation is made.

## Comparison Truth Table

For birth date, sex code, birthplace and initials, independently cover:

- [x] match;
- [x] mismatch;
- [x] reference missing;
- [x] CURP field unavailable because validation failed; and
- [x] field not requested.

No test may infer that a mismatch identifies which source is correct.

## Compatibility Tests

- [x] Existing valid scalar `epi_clean_curp()` output retains the exact 13-column Spanish schema.
- [x] The documented vector interface returns one row per element or follows an explicitly approved migration path.
- [x] Any stricter legacy invalid-input behaviour is documented in NEWS and help with an explicit compatibility rationale.

## Privacy And Failure Tests

- [x] Printed audit objects contain aggregate counts but no CURP, derived date, birthplace, initials or reference values.
- [x] Captured errors, warnings and messages contain stable fixed guidance and no supplied values.
- [x] Issues contain input indices and codes only.
- [x] No file or network operation occurs.
- [x] Examples, snapshots and test reporters contain no uncited real or screenshot-derived identifier.

## Implementation Evidence On 2026-08-09

- The pre-implementation focused run failed because the documented legacy vector path evaluated a vector in a scalar condition and because `epi_clean_curp_audit()` did not exist.
- The post-implementation focused run passed 31 compatibility expectations and 69 structural, comparison and privacy expectations.
- Package lint reported no findings.
- The complete `devtools::test()` run passed with only the repository's documented opt-in PostgreSQL and graphics skips.
- `scripts/check-local.sh` completed documentation generation, lint, the complete tests and `R CMD check` with 0 errors, 0 warnings and 0 notes.
- `git diff --check` passed after the final test changes.
- PR #231 passed macOS, Ubuntu, PostgreSQL integration, coverage, both Codecov gates and CodeFactor before merging to canonical `master` as `7e42f228969dbc62060f0660c43119882140052f`.

## Later Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'curp', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/check-local.sh
git diff --check
```
