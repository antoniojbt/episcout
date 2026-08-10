# Test Design

Spec ID: `032-eda-denominator-gap-assessment`
Status: Active design assessment

## This Contribution

- [x] Inspect exact canonical, stratified, Table 1, compact-plot, intake-report and database-report schemas.
- [x] Trace current percentages to their count and denominator sources without executing a new production calculation.
- [x] Confirm PostgreSQL presentation can be derived after the snapshot from retained aggregate artifacts.
- [x] Run online workflow reconciliation and whitespace checks before handoff.
- [x] Change no R source, tests, dependency metadata, generated documentation or package output.

## Required Successor Fixtures

Issue #253 must use hand-derived categorical fixtures covering two observed groups, Overall, an empty declared group, declared and unexpected levels, ordinary missing values, declared missing codes and a zero denominator. For every basis, expected numerators, denominators and proportions must be authored independently of production helpers.

## Required Successor Tests

- Exact column, row and overall denominators, including whether Overall participates in each cross-group denominator.
- Compatibility-default Table 1 bytes/fields and opt-in basis output traced to the shared calculation.
- Frequency companion rows before and after level collapse, with collapsed counts and proportions reconciling.
- Canonical, stratified, intake and database report cells using the same numerator/denominator/proportion rows.
- No additional PostgreSQL query, coordinate/theme collection or source-row artifact.
- Empty/all-missing/zero-row behaviour, no division warnings, numeric `NA` for unavailable proportions and clear basis metadata.
- Input/result non-mutation, value-free errors, deterministic ordering and manifest checksums.
- Focused tests followed by lint, local, CRAN-oriented and required GitHub checks.

## Design Verification Commands

```bash
scripts/check-workflow-state.sh
git diff --check
```
