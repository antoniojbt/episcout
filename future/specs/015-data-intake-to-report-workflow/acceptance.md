# Acceptance

Spec ID: `015-data-intake-to-report-workflow`
Status: Completed

- [x] The issue, completed component specifications, relevant public APIs and repository checklists were reviewed before package implementation.
- [x] SDD and TDD define fixed stage, status, return, message, artifact, manifest, privacy and filesystem contracts.
- [x] One documented `epi_eda_intake_run()` entry point composes existing scaffold, validation, preparation, canonical, optional stratified and report components without duplicating their semantics.
- [x] A call without a specification writes an editable scaffold/review guide and returns `review_required` before schema, preparation or analysis.
- [x] A generated or partially reviewed scaffold is never treated as approved; state is `reviewed` only when every scaffold row is exactly reviewed, while legacy caller-asserted specifications remain visibly distinguished.
- [x] Schema/preparation blockers prevent partial transformation and downstream analysis; audit and apply modes have distinct statuses and artifacts.
- [x] `prepare = "none"` proceeds only for a blocker-free audit requiring no planned transformation.
- [x] Successful apply retains before/after schemas and a value-free variable audit, summarizes only the in-memory prepared result and writes no prepared rows.
- [x] Canonical missingness and six summary components have exact type/status membership, deterministic values and reconciled counts before completion.
- [x] Optional stratification exports every machine-readable component; group denominators, exact Overall fields and categorical missing rows reconcile with canonical outputs and do not change them.
- [x] Table 1 failure blocks presentation without discarding already reconciled stratified components.
- [x] The stable run object includes the fixed top-level names, value-free long input metadata, explicit spec/report lists, structured messages and one-row run metadata for review, blocked, audit-complete and complete outcomes.
- [x] Processing stage is exactly intake, audit, preparation, canonical summary or stratified summary; optional report state never obscures the last completed data stage.
- [x] The fixed 26-row manifest has exact `artifact`, `type`, `path`, `status`, `sensitivity`, `checksum_md5` columns, created/not-created truth, relative paths and checksums.
- [x] Default collision handling makes no changes; authorised non-empty overwrite requires exact manifest-created files with matching checksums and refuses missing/unowned/modified files, directories, symlinks and special files before staging.
- [x] A complete sibling staging bundle is published by directory swap; prior output is held as a backup and restored where possible if final replacement fails, while failed staging leaves the prior target unchanged.
- [x] Calculation/report failures never leave the affected artifact marked created or return a misleading complete status.
- [x] The default bundle and return object contain no source/prepared rows, raw free-text examples, observed values from explicitly declared identifier variables, bridge tables, secrets or sensitive absolute paths; caller-supplied sensitive metadata remains classified and visibly review-required.
- [x] Base-R HTML rendering consumes saved CSV components after final timestamp/status metadata are written, escapes content, uses relative links, clearly marks incomplete/disclosure states, and performs no independent statistics or template-engine work.
- [x] Report failure preserves an underlying review/audit/blocked gate and changes only otherwise complete analysis to blocked.
- [x] Synthetic end-to-end tests cover initial review, audit, blocker, none/apply, canonical, stratification, overwrite, recovery and rendering paths with independent expected values.
- [x] User documentation explains the two-call review flow, statuses, exact files, recovery, privacy, disclosure and separate pseudonymisation boundary.
- [x] Independent implementation review finds no unresolved correctness, filesystem-safety, privacy or semantic blocker.
- [x] Focused tests, lint, `scripts/check-local.sh`, `scripts/check-cran.sh` and `git diff --check` pass or external limitations are recorded.
- [x] Checks require no network service, Codecov credential, tag or release.
