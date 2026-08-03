# Test Design

Spec ID: `014-stratified-descriptive-summaries`
Status: Completed

Use hand-derived fixtures; production output must not generate expected values.

## Calculation

- [x] Exact formals, classes, component/column order and types.
- [x] Two declared groups across numeric/integer/categorical/binary/date/datetime/text and identifier variables.
- [x] Group numeric fields and canonical Overall equality, including NA/NaN/Inf/-Inf, all-missing and zero variance.
- [x] Declared empty groups/levels, unexpected group/level ordering and flags.
- [x] Standard/sentinel missing strata included/excluded with exact omitted-row reconciliation.
- [x] Explicit categorical missing rows, literal `"NA"`, total/observed denominators and zero-denominator NA proportions.
- [x] Temporal output is stable across process timezone and requires safe prepared semantics.
- [x] Text contains aggregates only; identifiers, nested, absent and extra variables remain skipped with reasons.
- [x] Zero rows/all-missing strata and include_overall false retain stable schemas.
- [x] Invalid strata, options, input/spec names and strata types error actionably.
- [x] Inputs/spec remain identical and no files/options are changed.

## Table 1

- [x] Exact long-form schema/order and deterministic formatting.
- [x] Every display value traces to machine numeric/categorical/temporal/text fields.
- [x] Denominators distinguish ordinary categorical levels from missing rows.
- [x] Overall, missing/unexpected group notes, unexpected levels and infinity exclusions are visible.
- [x] No p-values, raw text values, implicit suppression or disclosure claim.
- [x] Invalid/non-stratified input errors.

## Regression And Documentation

- [x] Canonical and preparation tests remain unchanged and pass.
- [x] README/vignette/NEWS/Rd/NAMESPACE agree with behavior and v1 exclusions.
- [x] Worked receive/spec/prepare/stratify/Table 1 flow is executable.
- [x] Focused tests, lint, local/CRAN checks and `git diff --check` pass or external notes are recorded.
