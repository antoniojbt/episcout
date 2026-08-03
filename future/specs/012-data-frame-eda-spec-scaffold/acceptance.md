# Acceptance

Spec ID: `012-data-frame-eda-spec-scaffold`
Status: Completed

## Planning And Baseline

- [x] The brief, SDD, TDD and acceptance contract are complete before package-code changes.
- [x] The repository owner approved implementation through the explicit instruction to implement issue #181 and its dependent work.
- [x] Baseline `scripts/check-local.sh` passed on 2026-08-03 with zero errors, zero warnings and zero notes, with two known test skips and generated artifacts restored.
- [x] Privacy-safe candidate-level and declared-level encoding policies are explicit.

## Public Contract

- [x] `epi_eda_spec_scaffold(data, max_candidate_levels = 20L)` is exported and documented.
- [x] The function returns one deterministic ordinary data-frame row per supported source column in source order.
- [x] The exact core specification and evidence fields have stable types, including the zero-column result.
- [x] The returned draft passes `epi_eda_validate_spec()` and safe CSV round-trip through `epi_eda_spec()`.
- [x] Initial types follow supported storage metadata, while value-based findings remain candidates only.
- [x] Roles, units, groups, descriptions, bounds, sentinel missing codes and requiredness are not invented.

## Privacy, Integrity And Failure

- [x] Observed character and integer values never populate `candidate_levels`, reasons, warnings or errors.
- [x] Factor and logical class metadata appears only in core `levels`; v1 `candidate_levels` remains blank.
- [x] Semicolon and boundary-whitespace-unsafe declared levels are refused without exposing their values.
- [x] Unsupported classes, invalid names and unsafe level metadata fail before a partial result.
- [x] Empty and all-missing columns do not receive vacuous candidates.
- [x] The function does not mutate data, coerce columns, read or write files, create directories or rely on hidden state.

## Evidence And Integration

- [x] Counts and candidate classifications are tested against hand-authored expectations independent of production logic.
- [x] Mixed classes, zero rows, zero columns, all-missing values, infinities, strict temporal shapes, non-syntactic names, unsafe metadata and unsupported columns are covered.
- [x] A realistic draft-to-human-review-to-`epi_eda_run()` invocation succeeds and its actual objects are inspected.
- [x] Existing database dictionary, specification and canonical summary contracts remain unchanged and their relevant regression tests pass.

## Documentation And Verification

- [x] Roxygen, generated Rd, NAMESPACE, README, vignette and NEWS agree with observed behaviour and use British English.
- [x] Applicable software-verification, truth-and-semantics, analysis-and-statistics and copy-edit checklist evidence is recorded in `review.md`.
- [x] Focused tests and package-loaded lint pass.
- [x] `scripts/check-local.sh`, `scripts/check-cran.sh` and `git diff --check` pass, or every unrelated external limitation is recorded.
- [x] Worktree and staged diffs contain only spec 012 changes and no confidential, personal or machine-specific data.
- [x] No tag or release is created.
