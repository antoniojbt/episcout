# Acceptance

Spec ID: `033-categorical-denominator-presentation`
Status: Active

## Planning And Activation

- [x] Issue #253 and roadmap #249 identify this as the sole ready-next package slice.
- [x] Completed assessment spec 032 supplies an independently bounded gap and required fixtures.
- [x] The four basis, Overall, missingness, zero-denominator and compatibility rules are explicit before package changes.
- [x] Planning PR #257 is green at `362a94d` with no actionable review feedback before implementation activation.

## Calculation And Consumers

- [x] One public aggregate-only function validates and calculates the fixed categorical display schema.
- [x] Hand-derived fixtures prove every basis, missing level, empty group/level and zero denominator.
- [x] Default Table 1 remains byte-compatible with `362a94d` and opt-in bases consume the shared calculation.
- [x] Frequency companions and all EDA report families consume the shared numeric fields.

## Database And Publication

- [x] PostgreSQL adds no query or observation collection and remains snapshot-owned.
- [x] Default flat bundles remain exact; delivery companions are additive, manifest-owned and versioned.
- [x] Valid prior delivery bundles remain renderable; malformed/tampered companions fail atomically.
- [x] Core five-column manifests, checksums, deterministic paths and analyst-ownership language remain intact.

## Verification And Closeout

- [x] Focused, live PostgreSQL, lint and local checks pass; the CRAN-oriented check retains only its inherited incoming-feasibility NOTE.
- [x] Delivered CSV and HTML values are independently reconciled, report structure is inspected from the rendered HTML, and the exact categorical count plot is visually inspected at 1280 x 800.
- [ ] Required PR checks are green with no actionable review feedback.
- [ ] Canonical merge, issue closure, roadmap/TODO/changelog and local/fork state are reconciled.
- [ ] Manifest is completed and spec 033 is moved under `future/specs/done/`.
- [ ] A successor or explicit terminal reason is recorded.
