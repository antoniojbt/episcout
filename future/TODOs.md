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
- Move completed tasks to the 'Done' section, in reverse chronological order.

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
- Move completed items to `changelog.md`.
- Check this `TODOs.md` file and `future/specs/` specs are aligned.

## Task list

### Priority 1

- [ ] full git scrub
- [ ] Remediate the historical Codecov upload-token disclosure:
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
- [ ] update 'future/': specs are not marked clearly if done or not, when done move to a 'done' dir; update this TODOs file. Same for 'reviews/'.
- [ ] See two md files with plans/instructions from prior codex threads, saved in `future/scratch`:
  - [ ] 1-Review applicable agent checklists
  - [ ] 3-episcout release readiness
  - [x] 2- done already (spec 010 plan)
- [x] Update spec 10 based on new agents*md and checklists files.
- [x] Implement spec `010-canonical-eda-summary-contract`: replace the unreleased EDA v1/v2 interface with one authoritative typed summary contract, with no legacy adapter and no release or tag operation in scope.
- [x] Review and accept the target contracts and ordered implementation recommendations from completed spec `007-eda-stats-alignment-review`; create spec 008 only after that human approval.
- [x] Implement spec `008-univariate-stats-eda-alignment`: shared univariate statistics cores, compatible public adapters and opt-in complete EDA v2 summaries.
- [x] Implement spec `009-repository-lint-style-cleanup`: remove the 163 genuine loaded-package lint findings and enforce the corrected lint policy locally and in CI.
- [ ] agent truth review with specific instructions pack
    - [ ] why are penguins and blood data not downloaded directly each time from the package itself. My concern is the agent may re-write them to fit tests given it recreated these fixtures.
- [ ] Human live walkthrough, no agent needed here (clone, install, follow vignettes).
- [ ] Carry out changes needed from human review

### Priority 2

- [x] Implement spec `012-data-frame-eda-spec-scaffold` for issue #181: create a conservative, review-required EDA specification scaffold from an existing data frame without exposing observed values.
- [ ] Sanitise dictionaries so that R, QGIS, SQL/MariaDB/postgreSQL can easily use them as input
- [ ] Add a follow-up pseudonymisation spec for PII identification, dataset
      rewriting, identifier removal, output validation and read-only raw-data
      files. Secure bridge-table v1 was completed in spec 005.
- [ ] add functions to load, connect, etc data into db. 

### Priority 3

- [ ] check codecov percentage decrease
- [ ] Consider visual-regression strategy for EDA plots only after plot
      contracts are stable.
- [ ] Add biomedical EDA extensions as separate numbered specs.
- [ ] Revisit spec `003-large-data-backend-strategy` only after a concrete
      workload and performance target are defined.
