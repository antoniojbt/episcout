# Review Notes

Spec ID: `038-longitudinal-pseudonymisation-documentation`
Status: Active

## Baseline And Authority

The isolated clean task branch and fetched `origin/master` match exact authorised `commit-d79dd3c05ea1c64db2acd27b5b0af6b8193646b6`, the merge commit of completed documentation predecessor `PR-295`. GitHub confirms `issue-269` is open and the final behavioural evidence merged through `PR-291` plus closeout `PR-292`. The issue explicitly authorises this terminal documentation reconciliation and requires `spec-038` before substantive guide edits.

## Contract Reconciliation

Source and focused tests confirm exactly seven retained exports. Identity-universe results use `audit_complete`, `not_written` and `complete`; registry results use `initialisation_required`, `incompatible` and `ready`; pseudonymisation results use `audit_complete`, `not_written` and `complete`. Linkage schemas contain explicit output actions without confirmation/privacy authority. Registry and pseudonymisation operations use configured PostgreSQL permissions without querying or changing privileges. Exact identity, crosswalk, duplicate, stable-token, transaction, lock, rollback, recovery and EDA-handoff behaviour is fixed by current source and focused tests.

The initial guide mismatch was interpretive language: it called itself a governance guide, used approved/reviewed prerequisites and operations, prescribed restricted schema semantics, and framed downstream use/sharing through mandatory review language. Those statements exceeded the final package authority boundary even where their technical cautions were sensible. The registry roxygen/help and project map also used a package-level `restricted personal data` classification; the required factual warning needs only to state that registry mappings are re-identifying and pseudonymised data are not anonymous or automatically disclosure-controlled.

## Baseline Verification

- `scripts/check-workflow-state.sh` found structurally consistent manifests and no active/review implementation, then exited 2 because the disposable clone had only canonical `origin` and no remote named `upstream`.
- `git fetch origin master` confirmed `origin/master` remains exact `commit-d79dd3c`.
- Read-only GitHub checks confirmed the open tracker and exact merged predecessor/behavioural commits.
- Runtime introspection confirmed the seven exports and current formals.
- Focused offline tests passed; only the four `EPISCOUT_TEST_POSTGRES=1` cases skipped as designed.

## Implementation Findings

- The guide now includes a concise map of all seven public entry points, returned classes/components and neutral statuses, while referring readers to generated help for exact formals and nested columns.
- Example schema names are explicitly caller-selected rather than package requirements. The identity-universe and crosswalk examples use neutral `identity_data`, and the dictionary/catalogue example now selects the synthetic `entity_kind` column that actually exists.
- Identity-universe audit/materialisation, registry audit/initialisation and pseudonymisation audit/apply are distinguished by their actual transaction, snapshot, lock and write behaviour. In particular, pseudonymisation audit no longer claims one snapshot, apply documents bounded session-lock acquisition and transfer to transaction locks, and recovery distinguishes `not_written`, preflight errors and errors after transaction start.
- Stable-token scope now requires the same stored registry assignment, including namespace and crosswalk resolution. Duplicate report/drop, conflicting-key behaviour, non-cascading replacement, source non-mutation and EDA handoff remain exact.
- Approval, mandatory audit-first, restricted-schema, publication-safe and replacement-authority wording has been removed. Reversibility, pseudonymisation-not-anonymisation, no automatic disclosure control and external credentials/permissions/operational responsibility warnings remain explicit.
- `R/epi_sec_registry.R` and generated `man/epi_sec_identity_registry_init.Rd` now state the narrower verified warning without a package privacy classification. No other directly related help changed.
- `spec-038` and the synchronized active-lifecycle records identify the exact base, completed documentation and behavioural evidence, verification/recovery plan, terminal no-successor disposition and merge/closeout-pending state.

## Verification

- Focused offline `epi_sec_*` tests passed after the guide edit; the four explicit PostgreSQL cases skipped only because the live gate was unset for that invocation.
- Both relevant live test files passed against a disposable local PostgreSQL 18.4 instance at `127.0.0.1:55432`. Cleanup checks reported zero residual runtime schemas, roles and advisory locks.
- The vignette rendered to temporary HTML. Structural and text inspection found 18 headings, four tables, 18 code blocks, the required technical/confidentiality statements and none of the targeted stale phrases. No graphical browser is installed, so graphical layout inspection was unavailable.
- Extracted vignette R code parsed successfully. Runtime introspection confirmed exactly seven `epi_sec_*` exports and their current formals.
- `devtools::document()` generated the intended registry Rd change. It also exposed unrelated pre-existing author-markup drift in non-`epi_sec_*` Rd files; those check-created changes were removed from the scoped diff.
- `scripts/check-local.sh` passed: package lint had no findings, the full test suite passed with 26 documented environment-gated skips, the package and vignettes built, and `R CMD check` finished with `0 errors`, `0 warnings` and one inherited NOTE because current time could not be verified. Check-created visual-snapshot deletions and unrelated Rd drift were restored to the baseline.
- Final `scripts/check-workflow-state.sh` passed online against `antoniojbt/episcout@master` with `spec-038` as the sole active implementation. Final vignette render/inspection, registry tests, registry Rd validation, changed-file lint and `git diff --check` also passed after the help reconciliation.

## Compatibility

No public formal, class, component, status, error, database object or runtime side effect changes. The documentation now describes the already implemented neutral authority boundary. The only generated-help change narrows an unsupported privacy classification while preserving the stronger factual re-identification and non-anonymisation warning.

## Checklist Self-Review

- Software verification: public formals, result builders, database effects, errors, transaction/lock code and focused unit/live assertions were traced; rendered examples, generated help, the focused suites and complete local check were exercised.
- Truth and semantics: technical statuses are not permission outcomes; pseudonymisation audit is not conflated with one snapshot; missing/unmatched identifiers, duplicate counts, stable-token scope, reversibility and EDA boundaries retain their implemented meanings.
- Copy edit: the full guide was read in context, uses British English, retains established technical terms, removes unsupported authority wording and keeps prose paragraphs/table rows unwrapped.
- Render and release: temporary HTML was generated from current source and inspected structurally and textually; no retained or published artifact was created. Graphical-browser inspection was unavailable.

No unresolved correctness, confidentiality, compatibility or documentation query remains. This review is a self-review, not an independent implementation review.

## Lifecycle

- Tracking issue: `issue-269` under owner programme `issue-255` and roadmaps `issue-274`/`issue-275`.
- Documentation predecessor: completed `issue-268`/`PR-295`.
- Behavioural evidence: completed `issue-285`/`spec-037`/`PR-291` and closeout `PR-292`, supported by completed `spec-034` through `spec-036`.
- Working branch: `agent/issue-269-documentation-reconcile-the-longitudinal-pse` from exact `commit-d79dd3c`.
- Terminal disposition: no automatic package-code successor.
- Current state: active implementation; pull request, merge and closeout remain pending.
