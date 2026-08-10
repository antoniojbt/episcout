# Acceptance

Spec ID: `033-categorical-denominator-presentation`
Status: Draft

## Planning And Activation

- [x] Issue #253 and roadmap #249 identify this as the sole ready-next package slice.
- [x] Completed assessment spec 032 supplies an independently bounded gap and required fixtures.
- [x] The four basis, Overall, missingness, zero-denominator and compatibility rules are explicit before package changes.
- [ ] Planning PR is green with no actionable review feedback before implementation activation.

## Calculation And Consumers

- [ ] One public aggregate-only function validates and calculates the fixed categorical display schema.
- [ ] Hand-derived fixtures prove every basis, missing level, empty group/level and zero denominator.
- [ ] Default Table 1 remains compatible and opt-in bases consume the shared calculation.
- [ ] Frequency companions and all EDA report families consume the shared numeric fields.

## Database And Publication

- [ ] PostgreSQL adds no query or observation collection and remains snapshot-owned.
- [ ] Default flat bundles remain exact; delivery companions are additive, manifest-owned and versioned.
- [ ] Valid prior delivery bundles remain renderable; malformed/tampered companions fail atomically.
- [ ] Core five-column manifests, checksums, deterministic paths and analyst-ownership language remain intact.

## Verification And Closeout

- [ ] Focused, live PostgreSQL, lint, local and CRAN-oriented checks pass with reconciled evidence.
- [ ] Delivered CSV, SVG and HTML values are independently reconciled and visually inspected.
- [ ] Required PR checks are green with no actionable review feedback.
- [ ] Canonical merge, issue closure, roadmap/TODO/changelog and local/fork state are reconciled.
- [ ] Manifest is completed and spec 033 is moved under `future/specs/done/`.
- [ ] A successor or explicit terminal reason is recorded.
