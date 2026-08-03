# Acceptance

Spec ID: `013-specification-guided-data-preparation`
Status: Completed

## Planning And Baseline

- [x] Brief, SDD, TDD and acceptance semantics are complete before package-code changes.
- [x] The repository owner approved implementation through the explicit instruction to implement issue #182 and dependent work.
- [x] The inherited final spec 012 check is recorded as a clean same-commit baseline.
- [x] A focused current baseline is run and recorded before production edits on this branch.

## Public Contract

- [x] `epi_eda_prepare()` is exported with the exact safe defaults and documented return class/components.
- [x] Audit mode returns original data and no `schema_after`.
- [x] Apply mode is deterministic, resolves every blocker before mutation and never returns partial transformation.
- [x] Required `NA`/absent requiredness is optional/unasserted rather than blocking.
- [x] Scaffold evidence must be explicitly marked `reviewed` before apply; audit and specifications without scaffold evidence remain available.
- [x] Row count/order and documented column ordering/extra policies are preserved.

## Transformation Correctness

- [x] Standard and sentinel missingness match canonical semantics and produce correctly typed missing values.
- [x] Character numeric/integer parsing is blocked without an explicit future parse contract.
- [x] Integer conversion rejects non-whole, non-finite and out-of-range values.
- [x] Categorical declarations preserve order/zero levels; append is deterministic and explicitly flags specification divergence.
- [x] Binary unexpected values block even under append policy so binary output never has more than two levels.
- [x] Factor-to-text uses labels and text blanks remain observed unless declared missing.
- [x] Date/datetime parsing is strict; offset/Z normalises UTC and local input requires a reviewed valid timezone.
- [x] Invalid, unexpected and unsupported observations are counted but never silently discarded or converted to missing.
- [x] `min`/`max` remain non-mutating descriptive metadata and the v1 missing-code encoding limitation is documented.

## Audit, Privacy And Dataset Integrity

- [x] Audit and metadata schemas are stable, reconcilable and cover every variable/stage and dataset finding.
- [x] Dataset-level audit rows use reserved `.dataset.` names.
- [x] Missing variables, extras, unsupported columns, zero shapes and duplicate rows follow explicit policies.
- [x] No audit, metadata, error, warning or message exposes raw observed, sentinel, unexpected or invalid values.
- [x] The input data and specification are unchanged in audit, blocked apply and successful apply.
- [x] No row-level data or audit artifact is written by default.

## Integration And Verification

- [x] `schema_before` matches the current schema function exactly and successful `schema_after` is compatible.
- [x] Prepared data flow into canonical EDA with dimensions and missingness reconciled to the audit.
- [x] Hand-authored unit, privacy, boundary, non-mutation and integration tests pass.
- [x] Roxygen, generated documentation, README, vignette and NEWS match observed behaviour.
- [x] Applicable software, truth, analysis and copy-edit checklist evidence is recorded in `review.md`.
- [x] Focused tests, package lint, `scripts/check-local.sh`, `scripts/check-cran.sh` and `git diff --check` pass or unrelated limitations are recorded.
- [x] No tag or release is created.
