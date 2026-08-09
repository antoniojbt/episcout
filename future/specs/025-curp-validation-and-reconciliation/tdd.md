# Test Design

Spec ID: `025-curp-validation-and-reconciliation`
Status: Draft for owner review

## Baseline Evidence

- `devtools::test(filter = "curp_misc")` passed 20 existing expectations on 2026-08-09.
- A direct two-element call to the documented vector interface failed with `the condition has length > 1`.
- An 18-character non-numeric value passed the length gate, emitted a coercion warning and returned sliced fields with an unavailable year.
- Existing tests cover only two example extractions and one short-input error. They do not establish official structure, check digit, vector behaviour, impossible dates, century boundaries, missing values or value-free diagnostics.

## Proposed Test Files

- `tests/testthat/test-curp-validation.R`
- `tests/testthat/test-curp_misc.R`

## Independent Fixture Rules

- Every valid success fixture must come from a cited official example or be independently constructed from the accepted official algorithm and catalogue.
- Production code must not generate its own expected check digits.
- No fixture may be copied from user data, a real record, screenshots or an uncited internet generator.
- Fixtures that could be valid assigned identifiers are treated as restricted even when synthetic-looking; tests and failures must never print them.

## Contract Tests

- [ ] Public formals and fixed result component/column schemas are exact.
- [ ] Scalar, empty, length-one and vector inputs preserve one output record per element and stable `input_index` alignment.
- [ ] Missing input is distinct from malformed input.
- [ ] Lowercase, whitespace, punctuation, Unicode and wrong-length behaviour matches the owner-approved normalisation rule.
- [ ] Every position class is tested independently without reusing production validation logic.
- [ ] Position 17 follows the approved year-domain contract, including 1999/2000 boundaries and explicit pre-1900 behaviour.
- [ ] Impossible dates, leap-year boundaries and future dates follow the accepted contract.
- [ ] `H`, `M`, `NE` and every pinned birthplace code reconcile to the official source; unknown codes fail deterministically.
- [ ] The accepted official check-digit vectors pass and single-position mutations fail.

## Comparison Truth Table

For birth date, sex code, birthplace and initials, independently cover:

- [ ] match;
- [ ] mismatch;
- [ ] reference missing;
- [ ] CURP field unavailable because validation failed; and
- [ ] field not requested.

No test may infer that a mismatch identifies which source is correct.

## Compatibility Tests

- [ ] Existing valid scalar `epi_clean_curp()` output retains the exact 13-column Spanish schema.
- [ ] The documented vector interface returns one row per element or follows an explicitly approved migration path.
- [ ] Any stricter legacy invalid-input behaviour is documented in NEWS and help with an explicit compatibility rationale.

## Privacy And Failure Tests

- [ ] Printed audit objects contain aggregate counts but no CURP, derived date, birthplace, initials or reference values.
- [ ] Captured errors, warnings and messages contain stable fixed guidance and no supplied values.
- [ ] Issues contain input indices and codes only.
- [ ] No file or network operation occurs.
- [ ] Examples, snapshots and test reporters contain no uncited real or screenshot-derived identifier.

## Later Acceptance Commands

```bash
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(filter = 'curp', reporter = 'summary')"
scripts/rscript_env_caller.R -e "devtools::load_all(quiet = TRUE); findings <- lintr::lint_package(); print(findings); if (length(findings) > 0L) quit(status = 1L)"
scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); devtools::test(reporter = 'summary')"
scripts/check-local.sh
git diff --check
```
