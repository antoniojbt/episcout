# TODOs

- Keep all future work here with a priority order.
- Do not create additional files such as `backlog.md` or equivalent.
- Centralise task list here for clarity and for easy human review.
- Decide which future spec to activate first.
- Convert any new candidate work into a numbered spec before coding.
- If a task in this file does not have the needed SDD-TDD approach and a numbered spec, prompt me to clarify and create. Tasks that require multiple steps, substantial judgement calls, or may compromise existing code must have an SDD-TDD specification. If unclear ask first before proceeding with any write work.
- For the activated spec, record baseline package test/check status before package-code changes.
- Keep implementation work scoped to one numbered spec at a time.
- Review fixture anti-circularity guardrails before adding new expected outputs.
- Reconcile this file at the start and end of every change that affects `future/`, and at periodic checkpoints during long-running or multi-step work.
- Whenever a task, spec or review changes status, update this file in the same change. Keep active priority sections free of completed checkboxes and move completed tasks immediately to the 'Done' section in reverse chronological order under a completion-date heading.

- Use this template and place the task under the appropriate priority heading:

```markdown
- Short title:
    - Problem:
    - Goal:
    - User need:
    - Proposed scope:
    - Out of scope:
    - Candidate files:
    - Risks:
    - Suggested spec ID:
```

- For simple tasks only use e.g. `- [ ] xxx`
- Keep README and `NEWS.md` aligned with user-facing workflow changes.
- Record material completed work in `changelog.md` as well as retaining a concise entry under 'Done' here.
- Check this `TODOs.md` file and active specs under `future/specs/` are aligned.
- Keep only draft and active specs under `future/specs/`; move accepted completed specs to `future/specs/done/`.
- Keep only open cross-spec reviews directly under `future/reviews/`; move completed reviews to `future/reviews/done/`.

## Task list

### Open GitHub issue map

Reviewed against the complete open issue queue on 2026-08-07. Roadmap issue [#204](https://github.com/antoniojbt/episcout/issues/204) records the owner-approved sequence. Question-labelled issues do not authorise implementation until their scope is resolved.

| Issue | Priority | Tracked next action |
| --- | --- | --- |
| [#204](https://github.com/antoniojbt/episcout/issues/204) | Roadmap | Execute the non-deferred items below in the approved order, with one active implementation spec and one focused branch/PR at a time. |
| [#208](https://github.com/antoniojbt/episcout/issues/208) | Item 3 blocker, ready next | Implement spec `023-package-source-hygiene` without changing package behaviour, then inspect the exact source archive. |
| [#209](https://github.com/antoniojbt/episcout/issues/209) | Item 3 blocker, queued | Implement spec `024-external-fixture-provenance` after #208 while keeping routine tests offline. |
| [#81](https://github.com/antoniojbt/episcout/issues/81) | Item 4 release umbrella | Start only after #208, #209 and the owner-gated historical Codecov release disposition are resolved. |
| [#196](https://github.com/antoniojbt/episcout/issues/196) | Deferred item 5 | Do not activate spec `018-database-eda-report-rendering` until the owner revises the roadmap. |
| [#61](https://github.com/antoniojbt/episcout/issues/61) | Question | Obtain a concrete dependency target and compatibility objective before planning refactoring. |
| [#62](https://github.com/antoniojbt/episcout/issues/62) | Question | Inventory the named functions and desired cleanup outcome before planning changes. |
| [#65](https://github.com/antoniojbt/episcout/issues/65) | Question | Define the intended SIAP/alluvial workflow, inputs and reusable package boundary before planning plots. |

### Approved execution order under issue #204

Do not create all branches in advance. Synchronise from authoritative `upstream/master`, activate only the next numbered spec, and create one focused draft PR when that item starts.

#### Item 3 blockers before release work

- [ ] Resolve issue #208 through spec `023-package-source-hygiene` on `bugfix/package-source-hygiene`:
    - Problem: The canonical local-then-CRAN check sequence can package generated `Rplots.pdf`, obsolete developer paths and an unused workbook.
    - Goal: Make the exact source archive contain only demonstrated package material without changing public or analytical behaviour.
    - Candidate files: `.Rbuildignore`, focused test/build guardrails, four legacy test comments and `vignettes/R_datasets.xlsx`.
    - Risks: Removing intended visual references, masking test detritus instead of fixing its boundary or weakening package checks.
    - Suggested spec ID: `023-package-source-hygiene`.

- [ ] Resolve issue #209 through spec `024-external-fixture-provenance` on `refactor/external-fixture-provenance` after #208:
    - Problem: Fixture bytes match their declared upstream objects, but immutable source checksums and the complete redistribution/attribution record are not committed.
    - Goal: Make source identity, licence, extraction, transformation, local checksums and independent truth status reviewable while keeping tests offline.
    - Candidate files: fixture `SOURCE.md` records, the manual generator and focused guardrail tests.
    - Risks: Network-dependent tests, silently changing fixture bytes, circular expected values or retaining clinical data without an authoritative redistribution basis.
    - Suggested spec ID: `024-external-fixture-provenance`.

- [ ] Obtain the explicit owner decision required for the historical Codecov release blocker after #208 and #209; do not activate conditional spec 011, expose values, alter token policy or rewrite history without that instruction.

#### Item 4 — release 0.3.0

- [ ] Complete issue #81 and `future/scratch/release-0.3.0-plan.md` on `feature/release-0.3.0` only after item 3 blockers are resolved:
    - [ ] Complete the human live walkthrough from an isolated installed candidate; no agent may mark the human acceptance gate complete.
    - [ ] Carry out reviewed changes needed from the human walkthrough through appropriately scoped branches/specs.
    - [ ] Prepare and verify the exact `0.3.0` artifact, then stop at every tag, GitHub release and CRAN owner-approval gate.

#### Item 6 — multi-table PostgreSQL identifier universe

- [ ] Promote `future/scratch/2026-08-06_issue_episcout_universo_identificadores_multitabla.md` to a dedicated GitHub issue, resolve its contract questions, and implement spec `021-postgresql-identity-universe` on `feature/postgresql-identity-universe` after release `0.3.0`:
    - Problem: The current longitudinal linkage contract requires one enrolment source and cannot audit or materialise a reviewed identity universe from several equivalent PostgreSQL relations.
    - Goal: Add an audit-first, database-resident, value-free-by-default universe workflow that can materialise a restricted canonical identifier table only after explicit validation.
    - Out of scope: Probabilistic linkage, automatic correction, pseudonym generation, identifier export or claims of anonymity/disclosure control.
    - Required design gates: Contract ownership, initial normalisation policy, membership materialisation, enrolment integration, invalid-value policy, replacement semantics, portable fingerprints and resource limits.
    - Suggested spec ID: `021-postgresql-identity-universe`.

#### Item 7 — narrow PostgreSQL EDA row-count reuse

- [ ] Promote `future/scratch/episcout_postgres_eda_performance_issue.md` to a focused GitHub issue and spec `022-postgresql-eda-row-count-reuse` on `bugfix/postgresql-eda-row-count-reuse` after item 6:
    - Scope: Reuse the existing transaction-local relation row count in categorical summaries and remove exactly one redundant query per categorical/binary variable.
    - Out of scope: Wider aggregate consolidation or changed snapshot, bundle, privacy or reconciliation contracts.
    - Suggested spec ID: `022-postgresql-eda-row-count-reuse`.


## Later

- [ ] Deferred item 0b — historical Codecov credential containment and history decision. The owner deferred this work under issue #204. Do not expose credential material, change token policy, rewrite history or activate conditional spec `011` without a separate owner instruction. Deferral does not itself satisfy the release plan's no-unresolved-disclosure gate; item 3 must classify the remaining release impact for an explicit owner decision before item 4 can pass go/no-go.

- [ ] Deferred item 5 — issue #196 and spec `018-database-eda-report-rendering`:
    - Problem: `epi_eda_render_report()` cannot yet consume a completed PostgreSQL EDA run or its verified aggregate bundle.
    - Intended direction: Explicit dispatch into a separate self-contained report folder without modifying the manifest-owned source bundle or reading row-level data.
    - Status: Design-ready but deliberately outside the approved execution order until the owner revises issue #204.

- [ ] Resolve the scope questions in issues #61, #62 and #65 before promoting any of them to numbered specifications; no dependency reduction, broad cleanup or SIAP plot implementation is authorised by their current descriptions.

- [ ] Sanitise dictionaries so that R, QGIS, SQL/MariaDB/PostgreSQL can use them as input; obtain an explicit interoperability contract before creating an issue or spec.
- [ ] Add functions to load and connect data into databases; define supported backends, credentials boundary and user workflow before creating an issue or spec.
- [ ] Check the historical Codecov percentage decrease separately from credential remediation when the roadmap is revised.
- [ ] Consider visual-regression strategy for EDA plots only after plot contracts are stable.
- [ ] Add biomedical EDA extensions only as separately prioritised numbered specs.

- [ ] Deferred detailed record for the historical Codecov upload-token disclosure; retain these instructions without acting until the owner reactivates item 0b:
    - Problem: A redacted all-history secret scan found a token-shaped Codecov credential in the deleted `codecov.yml`. Commit `b22f919904317f2d3f27584412ccec02464c7d1c` is an affected historical landmark; commit `13815543bc81f5a16ad40f7c3426cfe40f36738e` removed the plaintext configuration and `78dcd5d53a8b2fa9916b93df4bef5258732b4236` later deleted the file. Deleting the current file did not remove the value from Git history. Never copy the credential into this task, an issue, a PR, chat, terminal output or a remediation report.
    - Goal: Make the historical credential unusable, verify that coverage upload still works securely, assess its exposure, and make an explicit owner-approved decision about whether destructive history rewriting is warranted.
    - User need: A safe walkthrough that separates urgent credential containment from optional history cleanup and leaves evidence that does not disclose the credential.
    - Proposed scope:
        - Phase 1 — contain first:
            - [ ] Sign in to Codecov with admin access to `antoniojbt/episcout`; confirm whether the displayed upload credential is repository-scoped or inherited from an account/organisation global token. Do not reveal or record its value.
            - [ ] Treat the historical value as compromised even if it appears old or inactive. Use Codecov's current rotate/regenerate/invalidate control for the applicable token; if no self-service invalidation is available, ask Codecov Support to invalidate it before doing anything to Git history.
            - [ ] In GitHub, open repository or organisation `Settings` -> `Secrets and variables` -> `Actions`. Replace `CODECOV_TOKEN` with the newly issued value, or deliberately remove it only if the owner chooses Codecov's supported tokenless mode for this public repository. Never include the `CODECOV_TOKEN=` prefix in the stored value.
            - [ ] Prefer retaining token authentication unless the owner explicitly accepts the weaker protection against false uploads to protected branches. The current workflow uses `codecov/codecov-action@v7`, which supports Codecov's public-repository tokenless setting, but tokenless operation is a policy choice rather than the default remediation.
            - [ ] After the secret is updated, merge or run a controlled change that causes `.github/workflows/test-coverage.yaml` to execute on a protected `master` push. Confirm the upload step succeeds and the matching commit appears in Codecov; a fork pull-request run alone does not prove that the protected repository secret works.
            - [ ] Record only the revocation/rotation date, token scope, responsible owner and successful workflow-run URL. Do not record either old or new token values.
        - Phase 2 — assess exposure with redacted tools:
            - [ ] Install or update Gitleaks, then run `gitleaks git --redact --log-opts="--all"` and save only a redacted report outside the repository. Also inspect GitHub secret-scanning alerts if that feature is available.
            - [ ] Confirm the first and last affected commits, branches, tags and file paths instead of assuming the landmark commits above describe every affected ref.
            - [ ] Inventory forks, open pull requests, release/tag references, Actions artifacts and known clones that may retain the historical objects. Assume public clones cannot be recalled.
            - [ ] Check Codecov and GitHub audit information available to the owner for unexpected configuration changes or uploads. Record the limits of available audit history; absence of logs is not proof the token was never used.
        - Phase 3 — make the history decision before rewriting anything:
            - [ ] Read Codecov's current token guidance at <https://docs.codecov.com/docs/codecov-tokens> and GitHub's sensitive-data-removal guidance at <https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/removing-sensitive-data-from-a-repository>.
            - [ ] Decide with the repository owner whether revocation is sufficient. GitHub recommends revoke/rotate first and notes that this may fully mitigate a revocable secret; history rewriting has significant coordination and recontamination risks.
            - [ ] If revocation is accepted as sufficient, document that decision and skip the remaining rewrite steps. Continue to the verification and prevention phase.
            - [ ] If history removal is still required, create and approve spec `011-codecov-credential-history-remediation` before running any rewrite. Schedule a maintenance window, pause pushes, merge or close open PRs, notify collaborators and fork owners, and make a recoverable mirror backup with restricted access.
        - Phase 4 — only if the separately approved spec authorises a rewrite:
            - [ ] Use a fresh mirror clone and `git-filter-repo` version 2.47 or later with `--sensitive-data-removal`. Prefer removing `codecov.yml` from all affected refs with `--invert-paths --path codecov.yml`; use `--replace-text` only if preserving its non-secret history is important and the replacement file can be handled outside the repository without displaying the credential.
            - [ ] Inspect `.git/filter-repo/changed-refs`, especially affected pull-request refs and the reported first changed commits. Stop if the affected scope is larger than the approved plan.
            - [ ] Run the full redacted secret scan and repository tests against the rewritten mirror before changing GitHub. Verify branches, tags, release ancestry and the current workflow configuration.
            - [ ] Temporarily adjust branch protection only as authorised, force-push the approved rewritten mirror, then immediately restore protection. Do not treat a successful force-push as complete removal.
            - [ ] Contact GitHub Support with the repository name, affected pull-request count, first changed commits and any reported orphaned LFS objects so cached views and server-side PR references can be purged where eligible.
            - [ ] Require collaborators to re-clone or follow `git-filter-repo` cleanup instructions; they must rebase, not merge, old branches. Coordinate separately with fork owners because GitHub cannot remove objects from their forks.
        - Phase 5 — verify and prevent recurrence:
            - [ ] Confirm `.github/workflows/test-coverage.yaml` obtains any token only from `${{ secrets.CODECOV_TOKEN }}` and that no plaintext credential exists in the current tree, workflow logs, artifacts or rebuilt history.
            - [ ] Re-run `gitleaks git --redact --log-opts="--all"` from a fresh clone and archive only the redacted result and scanner version outside the repository.
            - [ ] Verify a protected-branch Codecov upload and the expected required/optional status behaviour without printing secrets.
            - [ ] Enable or review GitHub secret scanning and push protection where available. Consider a separate, narrowly scoped task to add a pinned secret-scanning check to local/CI workflows.
            - [ ] Close any GitHub secret-scanning alert only after recording the revocation evidence and the owner-approved history decision. Update this TODO and `future/changelog.md` without including credential material.
    - Out of scope: Changing coverage thresholds or investigating a coverage-percentage decrease; those remain separate from credential remediation. No history rewrite, force-push, tokenless-policy change or release operation is authorised by this TODO alone.
    - Candidate files: `future/TODOs.md`, a future `future/specs/011-codecov-credential-history-remediation/`, `.github/workflows/test-coverage.yaml`, GitHub Actions secrets/settings and Codecov repository/account settings. Historical `codecov.yml` is evidence, not a file to restore.
    - Risks: Exposing the value while investigating it; rotating the wrong global token and breaking other repositories; accepting tokenless uploads without understanding protected-branch integrity; invalidating coverage unexpectedly; rewriting signed commits/tags and PR diffs; losing collaborator work; leaving cached/forked copies; or recontaminating cleaned history from an old clone.
    - Suggested spec ID: `011-codecov-credential-history-remediation` if history rewriting or workflow/security-policy changes are chosen. Rotation and verification should happen before that spec because containment must not wait for planning.


## Done

### 2026-08-07

- [x] Complete roadmap item 3's release-readiness audit at `806b3e2`: pass the local `0/0/0` baseline, classify the one CRAN incoming NOTE and 14 source-test skips, inspect/install the 373-member source archive, verify current/archive secret coverage, reconcile spec 010 and the release delta, prove both pinned fixtures match their declared upstream objects, and record blockers as issues #208 and #209 plus the owner-gated historical Codecov disposition. No package behaviour, credential policy, history, tag, release or submission changed.
- [x] Accept and merge issue #197/spec `019-postgresql-catalogue-missingness` through PR #207 with PostgreSQL 17, macOS, Ubuntu, coverage, Codecov and CodeFactor checks green; move the completed specification to `future/specs/done/`.
- [x] Accept and merge issue #198/spec `020-data-frame-writer-delimiter-contract` through PR #206 with macOS, Ubuntu, PostgreSQL, coverage, Codecov and CodeFactor checks green; move the completed specification to `future/specs/done/` and activate only issue #197/spec 019 next.
- [x] Reconcile the roadmap under issue #204: defer item 0b and issue #196/spec 018, order issues #198 and #197 before the release-readiness audit and release 0.3.0, then schedule the PostgreSQL identifier-universe and narrow row-count work; align the scratch index, release plan and spec template without starting package implementation.

### 2026-08-05

- [x] Reconcile all seven open GitHub issues into explicit TODO priorities, remove the completed spec-003 backend scratch plan and prune merged local branches without deleting active scratch inputs.
- [x] Complete spec `003-large-data-backend-strategy` and issue #194 through PR #201: replace the production-performance claim with a fixed synthetic PostgreSQL 17 gate, resolve three independent reviews, pass all local and GitHub checks, record owner acceptance and move the spec to `future/specs/done/` without releasing or tagging.
- [x] Fix issue #195 so PostgreSQL pseudonymisation apply releases each acquired session advisory lock exactly once during transaction-lock promotion, retains failed releases for exit cleanup, and verifies warning-free success, partial timeout cleanup and forced rollback without changing the public contract.

### 2026-08-04

- [x] Extract the useful repository-compatible parts of the hidden environment-level `ds-pipeline-designer` skill into a visible, explicitly non-active scratch draft for later owner review, while excluding its Python defaults, foreign artifacts and scaffolding workflow.
- [x] Revise and activate spec `003-large-data-backend-strategy` as the PostgreSQL-first large-data EDA handoff, with explicit source/type/statistical/snapshot/privacy/bundle contracts, neutral parity evidence and a reproducible synthetic scale protocol.
- [x] Complete spec `017-deterministic-local-time-ambiguity` for issues #190 and #81: replace host timezone sampling with `clock`'s bundled IANA engine, preserve offset-bearing behaviour and value-free blocking, and record GitHub macOS/Ubuntu confirmation as a pull-request check because publication was not authorised.
- [x] Accept and merge spec `016-longitudinal-pseudonymisation` through PR #189, reconcile its final status records and move the specification to `future/specs/done/`.
- [x] Separate active and completed specifications and reviews into `done/` directories, repair their references, and make TODO reconciliation part of every future-work change.

### 2026-08-03

- [x] Implement spec `015-data-intake-to-report-workflow` for issue #184: compose review-gated intake, conservative audit/apply preparation, canonical and optional stratified summaries, a privacy-conscious artifact manifest and a report view without writing row-level data.
- [x] Implement spec `014-stratified-descriptive-summaries` for issue #183: add canonical grouped summaries and a traceable, non-inferential Table 1 presentation.
- [x] Implement spec `013-specification-guided-data-preparation` for issue #182: audit and apply a reviewed EDA specification through a deterministic, value-free and all-or-nothing preparation boundary.
- [x] Implement spec `012-data-frame-eda-spec-scaffold` for issue #181: create a conservative, review-required EDA specification scaffold from an existing data frame without exposing observed values.
- [x] Implement spec `010-canonical-eda-summary-contract`: replace the unreleased EDA v1/v2 interface with one authoritative typed summary contract, with no legacy adapter and no release or tag operation in scope.

### 2026-07-31

- [x] Update spec 010 based on the revised agent guidance and checklists.
- [x] Review the saved spec 010 plan from the prior Codex thread.

### 2026-07-25

- [x] Implement spec `009-repository-lint-style-cleanup`: remove the 163 genuine loaded-package lint findings and enforce the corrected lint policy locally and in CI.
- [x] Implement spec `008-univariate-stats-eda-alignment`: shared univariate statistics cores, compatible public adapters and opt-in complete EDA v2 summaries.
- [x] Review and accept the target contracts and ordered implementation recommendations from completed spec `007-eda-stats-alignment-review` before creating spec 008.
