# Review Notes

Spec ID: `034-retained-epi-sec-technical-contract`
Status: Active design inventory

## Findings

- The technical core is internally coherent: cryptographic generation, exact typed identity, deterministic collation, immutable registry mappings, crosswalk reconciliation, record-key conflict detection, destination ownership/dependencies, advisory locks, transaction ownership, rollback and row-count checks do not depend on privacy classifications.
- Governance coupling is concentrated in portable confirmation/status fields, `privacy_class`, automatic direct-identifier exclusion, schema/table `PUBLIC` checks and revocations, ownership-as-access policy, `blocked`/`blocking`/governance conditions, audit-first next actions, sensitivity-bearing manifests and the redacting diagnostic class.
- `epi_sec_pseudonym()` requires no behavioural refactor. Its current validation and overwrite rules are technical, and its random token properties are shared by registry creation and enrolment.
- The released `0.3.0` contract includes five of the seven exports. Identity-universe interfaces are development-only, while the current four-component linkage schema is also newer than `0.3.0` and already has a breaking migration boundary.
- Audit currently can be called independently, and apply/materialise already repeats validation; the approval coupling is in statuses, metadata gates, privilege policy and prose rather than a technical prerequisite that must be removed from transaction flow.
- Removing `PUBLIC` checks is safe only if native permission failures remain sanitised and the package retains exact target, object-kind, ownership-for-replacement and rollback checks. The design keeps those controls.

## Compatibility Judgement

Preserve successful status values and result classes wherever they do not carry policy meaning. Do not preserve `blocked`, `blocking`, `schema_restricted`, privacy/sensitivity fields or redacting diagnostic behaviour merely for compatibility. Use temporary adapters only where a legacy field maps deterministically to syntax; never let legacy approval/classification values change execution.

## Evidence And Limitations

The review used repository source/tests/docs/history and live owner issue decisions; no production database or external consumer code was inspected. Current PostgreSQL integration assertions were read but not executed because this design contribution requires no database access and the focused live suite is explicitly gated. The set/count expectations in the existing universe fixture are independently checkable, but this design does not independently validate PostgreSQL or cryptographic implementations. This is a self-review, not an independent implementation review.

## Open Questions

No product or architecture question remains for the first implementation slice. Successor issue #278 has been created from the reviewed draft and is intentionally not dispatch-ready until PR #277 is merged and issue #276 closeout is complete.

## Local Verification

- The focused pseudonym, linkage and identity-universe unit selection passed every enabled expectation; the opt-in live PostgreSQL universe test skipped because `EPISCOUT_TEST_POSTGRES` was not set.
- `scripts/check-workflow-state.sh` passed online against `antoniojbt/episcout@master` with spec 034 as the sole active design and no active implementation.
- `git diff --check` and an explicit trailing-whitespace scan covering the new untracked spec files passed.
- The software-verification, truth-and-semantics and copy-edit checklists were applied as self-review. No code or rendered documentation changed, so realistic package invocation and render/release checks are not applicable to this design-only diff.

## Lifecycle

- Tracking issue: #276 under owner roadmaps #274 and #275.
- Design branch: `agent/issue-276-design-inventory-governance-coupling-and-spe` at canonical `master` baseline `4d55a20`.
- First implementation tracker: #278, prepared from `first-implementation-issue.md` and intentionally not dispatch-ready until design closeout.
- Pull request: #277. CI, canonical merge and closeout remain pending.
