# episcout 0.3.0 Release Plan

Status: Ready for execution  
Created: 2026-07-31  
Owner: Antonio Berlanga-Taylor  
Baseline release: `0.2.0`  
Target release: `0.3.0`

## Outcome

Release `episcout 0.3.0` from a reviewed and reproducible commit after the repository has been scrubbed, spec `010-canonical-eda-summary-contract` has been accepted and implemented, a human has completed the release-facing workflows from a clean clone, all resulting findings have been resolved, and the exact release artifact has passed the required checks and inspection.

## Scope

In scope:

- Audit the current tree, Git history, ignored material, package source contents and public-disclosure risks.
- Resolve repository-hygiene findings that affect confidentiality, reproducibility, package contents or release quality.
- Update, approve and implement spec `010-canonical-eda-summary-contract` before testing the release candidate manually.
- Run a human walkthrough from a clean clone and record observed results and defects.
- Resolve walkthrough findings, finalise release metadata and user-facing documentation, and validate the exact source package.
- Create an annotated `0.3.0` tag and a GitHub release after explicit human approval.

Out of scope:

- CRAN submission. Prepare a CRAN-quality source package, but treat submission under issue `#81` as a separate authorised operation.
- Unrelated Priority 2 or Priority 3 features.
- New dependencies, architecture changes or compatibility layers not required by spec 010 or a confirmed release blocker.
- Git history rewriting, destructive cleanup, branch deletion or removal of published material without a separate finding-specific plan and explicit approval.

## Release decisions

- Use `0.3.0`, not `0.2.1`, because the release adds public database and dictionary APIs, introduces the canonical typed EDA summary contract, and includes material behaviour corrections.
- Use the tag name `0.3.0`, consistent with the immediately preceding release.
- Treat spec 010 as a pre-release gate. The current v1/v2 EDA distinction is unreleased and should be replaced before the human walkthrough so that the walkthrough exercises the intended public contract.
- Treat the human walkthrough as independent acceptance evidence, not as a substitute for automated checks.
- Do not create or push a tag, publish a GitHub release or submit to CRAN until the final go/no-go checkpoint is approved by the owner.

## Execution rules

- Start every phase from a clean working tree and record the starting commit.
- Keep the scrub, spec 010 implementation, walkthrough fixes and release preparation in reviewable commits or pull requests; do not hide behaviour changes inside cleanup commits.
- Classify each finding as `release blocker`, `follow-up` or `accepted limitation`, with a short rationale and evidence.
- Stop on suspected credentials, confidential data, unexplained fixture provenance, scientific ambiguity, an unauthorised compatibility break or an unexpected package artifact.
- Use a numbered specification before making any new multi-step or scientifically consequential code change not already authorised by spec 010.
- Preserve command output or concise results in the relevant spec review, walkthrough record or pull-request description. Do not paste secrets or confidential values into logs.

## Phase 0: Establish the baseline

1. Refresh remote state and confirm the release baseline:

   ```bash
   git fetch --prune --tags origin
   git status --short --branch
   git rev-list --left-right --count master...origin/master
   git log -1 --format='%H %ad %s' --date=iso-strict
   git describe --tags --always
   git tag --sort=-version:refname --format='%(refname:short) %(creatordate:iso8601) %(subject)'
   ```

2. Record the R environment through the repository wrapper:

   ```bash
   scripts/rscript_env_caller.R -e "R.version.string; R.home(); .libPaths(); sessionInfo()"
   ```

3. Record baseline results before package-code changes:

   ```bash
   scripts/check-local.sh
   scripts/check-cran.sh
   ```

4. Record all errors, warnings and notes exactly. A green GitHub Actions conclusion is not sufficient because prior checks completed successfully while reporting two package NOTEs.

Exit criteria:

- `master` and `origin/master` are reconciled, the working tree is clean, the baseline commit and environment are recorded, and every baseline failure is classified.

## Phase 1: Full repository scrub

Perform the scrub as a read-only audit first. Propose and review remediation before deleting files, changing history or altering public interfaces.

### 1.1 Git integrity and state

- Inspect branches, tags, worktrees, submodules and object health:

  ```bash
  git status --short --branch --ignored
  git branch --all --verbose --verbose
  git worktree list
  git submodule status
  git fsck --full
  git count-objects -vH
  git clean -nd
  git clean -ndX
  ```

- Treat both `git clean` commands as previews only. Do not run a destructive clean as part of this plan.
- Confirm that annotated release tags point to the intended release commits and note the historical `v0.1.3` naming inconsistency without rewriting existing tags.

### 1.2 Public disclosure, credentials and confidential material

- Reconfirm that the GitHub repository is public before assessing disclosure risk.
- Inspect tracked and historical content for credentials, private keys, tokens, connection strings, personal or restricted data, local paths, logs, database dumps and machine-specific agent state. Prefer a secret scanner with redaction, such as `gitleaks git --redact --log-opts="--all"`, if installed; record explicitly when no scanner is available.
- Inspect fixtures, generated outputs and review artifacts for provenance, licensing, neutral identifiers and accidental real-world data. Resolve the existing concern about the `penguins_raw` and `blood_storage` fixtures by verifying their sources and independently justified expected outputs rather than regenerating expectations through production code.
- If a credential or confidential value is found, stop normal release work, revoke or contain it first, assess exposure, and prepare a separately approved history-remediation plan. Deleting the current file alone is not sufficient.

### 1.3 Tracked, ignored, generated and oversized material

- Inspect tracked files that also match ignore rules:

  ```bash
  git ls-files -ci --exclude-standard
  git ls-files --others --exclude-standard
  find . -path ./.git -prune -o -type f -size +5M -print
  ```

- Review `.gitignore` and `.Rbuildignore` against actual repository and package behaviour. Keep development-only directories such as `future/`, `archive/`, `outputs/`, `checklists/`, build artifacts and agent state out of the source package unless their inclusion is explicitly required.
- Review binary and generated files, especially archives, rendered outputs, fixture products and review bundles. Retain them only when their provenance and continuing value are clear.
- Reconcile `future/TODOs.md`, spec statuses and implemented public APIs. Move or relabel completed planning work according to the repository's chosen organisation without altering package behaviour.

### 1.4 Package source contents and current check NOTEs

- Build through the canonical release-oriented entry point and inspect the complete archive rather than relying only on `R CMD check` status:

  ```bash
  scripts/check-cran.sh
  tar -tzf build/cran-check/episcout_*.tar.gz
  ```

- Resolve the known hidden-file NOTE from `inst/project-template/data/.gitkeep` and `inst/project-template/outputs/.gitkeep`. If the installed template must preserve those directories, use a meaningful non-hidden placeholder or create the directories at runtime; otherwise exclude or remove them.
- Resolve the known non-standard top-level `outputs/` NOTE and ensure internal truth-review outputs are not included in the source package.
- Verify that the tarball excludes development plans, checklists, review outputs, archives, local configuration, histories, credentials and unrelated generated artifacts while retaining every installed template, vignette, fixture and licence file required by users.

### 1.5 Review the release delta

- Inspect the complete change from `0.2.0` to the candidate, with special attention to exported interfaces, defaults, dependencies, joins, missingness, denominators, generated documentation and fixture provenance:

  ```bash
  git diff --stat 0.2.0..HEAD
  git diff 0.2.0..HEAD -- DESCRIPTION NAMESPACE NEWS.md R/ tests/testthat/ vignettes/ inst/
  git diff --check 0.2.0..HEAD
  ```

- Confirm that the full-outer-join default in `epi_clean_merge_nested_dfs()` is authorised, tested and prominently documented as a behaviour change, with explicit legacy left-join instructions if that path remains supported.
- Confirm that every new export has roxygen documentation, user-facing release notes and meaningful tests.

### Phase 1 exit criteria

- No suspected secret, confidential datum or unreviewed disclosure remains.
- No destructive or history-rewriting operation is pending.
- The package source archive contains only intended release material.
- The two known `R CMD check` NOTEs are resolved and any new diagnostic is classified.
- Fixture provenance and anti-circularity concerns are resolved or explicitly block the release.
- Planning and repository state accurately distinguish completed, active and deferred work.
- Scrub remediations are reviewed and committed separately from behavioural changes.

## Phase 2: Finalise and implement spec 010

1. Update `future/specs/010-canonical-eda-summary-contract/` to apply the current `AGENTS.md` and the routed checklists, especially software verification, truth and semantics, analysis and statistics, copy-edit, and render and release.
2. Reconfirm the observable contract, independently justified analytical expectations, compatibility decision, affected callers, report behaviour and acceptance checks.
3. Obtain human approval of the revised brief, SDD and TDD before implementation.
4. Implement on `refactor/canonical-eda-summary-contract` without version bumps, tags or release operations.
5. Run focused tests first, then `scripts/check-local.sh`, `scripts/check-cran.sh` and `git diff --check` as required by the spec.
6. Inspect the returned tables, written CSV files, rendered HTML and source package contents directly. Record evidence and unresolved limitations in the spec review.
7. Complete the spec acceptance record, obtain review, merge it to `master`, and wait for successful macOS, Linux, lint and coverage workflows.

Exit criteria:

- Spec 010 is accepted and merged; package code and user documentation expose one canonical six-component EDA summary contract; no v1/v2 adapter or release operation remains; checks pass with `0 errors, 0 warnings, 0 notes`.

## Phase 3: Human walkthrough of the release candidate

The owner performs this phase manually from a fresh clone. Agents may supply commands or diagnose reported failures but must not mark the walkthrough complete on the owner's behalf.

### 3.1 Prepare an isolated candidate

- Clone the public repository into a new temporary directory, check out the exact candidate commit on `master`, and record the commit, operating system and R session.
- Build with `scripts/check-cran.sh` and install the resulting source tarball into an isolated R library.
- Do not use a developer checkout loaded by `devtools::load_all()` as the walkthrough target; exercise the installed package.
- Use only public or synthetic data and temporary output directories.

### 3.2 Walk the user journeys

- Follow the README installation and introductory usage exactly as written.
- Follow the specification-first EDA vignette from input specification through schema checking, missingness, canonical summaries, plots, synthetic data where documented, complete workflow output and report rendering.
- Confirm that the canonical summary contains `variables`, `numeric`, `categorical`, `text`, `temporal` and `skipped`, and that every specified variable is accounted for with explicit missingness, exclusions, denominators and failure reasons.
- Create a project with `epi_eda_create_project()` and verify that the installed scaffold contains the documented directories and usable templates.
- Exercise the new database inventory and dictionary APIs with a disposable SQLite database or another documented local example; verify read-only behaviour where promised, schema output, validation failures and cleanup.
- Exercise at least one corrected boundary for synthetic integer generation, repeated-measure spreading, transpose labels, nested-data-frame joining, event proportions and all-missing/non-finite summaries.
- Open and inspect every generated HTML report and relevant plot at normal viewing size for missing content, broken navigation, clipping, blank sections, unreadable labels and disagreement with returned or written values.
- Confirm that commands, argument names, defaults, output filenames and error messages agree across help pages, README, NEWS and vignettes.

### 3.3 Record and disposition findings

- Create a dated walkthrough record under `future/reviews/` containing the candidate commit, environment, exact workflows exercised, observed outputs, screenshots or paths where useful, defects, accepted limitations and the owner's decision.
- Classify discrepancies affecting correctness, missingness, denominators, public interfaces, installation, report values, privacy or reproducibility as release blockers.
- Route substantive fixes through a numbered specification; use a focused bug-fix branch for small, clearly intended corrections. Rerun affected walkthrough steps and the full relevant automated checks after every fix.

Exit criteria:

- The owner records a human `GO` with no unresolved release blockers, and the accepted candidate commit is identifiable and reproducible.

## Phase 4: Prepare release 0.3.0

1. Create `feature/release-0.3.0` from the accepted and fully synchronised `master`.
2. Change `DESCRIPTION` from `0.2.0` to `0.3.0`.
3. Replace the `NEWS.md` development heading with `0.3.0` and finalise concise user-facing notes that cover:

   - The canonical six-component EDA summary contract from spec 010, with obsolete v1/v2 wording removed.
   - The six new database inventory and dictionary exports.
   - Statistical, missingness, schema, plotting, synthetic-data, transpose and repeated-measure corrections.
   - The changed full-outer-join default and any migration instruction.
   - Material limitations that remain after the walkthrough.

4. Align README, vignettes, report templates, examples and roxygen documentation with observed installed-package behaviour. Edit roxygen sources and regenerate `man/` and `NAMESPACE`; do not edit generated files directly.
5. Update planning status only where the release or completed work makes existing entries demonstrably stale. Do not bundle unrelated backlog reorganisation into the release commit.
6. Inspect the complete release diff and create a focused commit with an imperative subject, for example `[Release] Prepare 0.3.0`.

Exit criteria:

- Version, NEWS, documentation, generated files and package behaviour agree; the release diff contains no unrelated changes; compatibility and limitations are explicit.

## Phase 5: Validate the exact release artifact

1. Run the canonical checks from the release branch:

   ```bash
   scripts/check-local.sh
   scripts/check-cran.sh
   scripts/rscript_env_caller.R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); covr::report()"
   git diff --check
   git status --short
   ```

2. Require `0 errors, 0 warnings, 0 notes` from the source-package check. Investigate unexplained coverage regressions; do not accept coverage percentage alone as evidence of correctness.
3. Inspect `build/cran-check/episcout_0.3.0.tar.gz` directly:

   - Review the complete member list and file metadata.
   - Confirm the absence of `future/`, `archive/`, `outputs/`, `checklists/`, agent state, secrets, local histories, caches and unrelated artifacts.
   - Extract it to a temporary directory, inspect the packaged DESCRIPTION, NEWS, licence, vignettes and installed templates, and install from that exact tarball into an isolated library.
   - Render and inspect the vignettes and representative HTML report from the installed package, comparing important values with returned summaries and written CSVs.
   - Record a checksum for any tarball that will be attached to the GitHub release.

4. Open a pull request titled `[Release] Prepare 0.3.0`. Its description must state what changed, checks run, compatibility impact, measured coverage information and every unresolved limitation, explicitly stating when none remain.
5. Require successful macOS and Linux package checks, lint and coverage workflows on the release commit. If CRAN submission is later authorised, separately add or run appropriate Windows, R-devel and oldrel checks.
6. Obtain review, merge without bypassing required checks, and confirm that the exact release-preparation changes are present on `origin/master`.

Exit criteria:

- The exact `0.3.0` artifact is inspected and reproducible; local and GitHub checks pass; the release PR is reviewed and merged; no unresolved blocker remains.

## Phase 6: Go/no-go and publication

### Final go/no-go

The owner explicitly confirms all of the following before publication:

- [ ] The repository scrub is complete and no disclosure or history-remediation issue remains.
- [ ] Spec 010 is accepted and merged.
- [ ] The human walkthrough records a `GO` against the final behaviour.
- [ ] Walkthrough findings are resolved or documented as accepted non-blocking limitations.
- [ ] `DESCRIPTION` and `NEWS.md` identify `0.3.0` and agree with the installed package.
- [ ] The exact source tarball built from the intended tag target passed with `0 errors, 0 warnings, 0 notes` and was inspected.
- [ ] GitHub Actions passed on the commit to be tagged.
- [ ] `master` is clean, equals `origin/master`, and the intended tag target commit is recorded.
- [ ] Release notes and any uploaded artifact contain no confidential or unintended material.
- [ ] The owner authorises public tag and GitHub release creation.

### Publish

1. Synchronise without creating a new merge commit and verify the target:

   ```bash
   git switch master
   git pull --ff-only origin master
   git status --short --branch
   git rev-list --left-right --count master...origin/master
   git log -1 --format='%H %ad %s' --date=iso-strict
   ```

2. Rebuild from the synchronised `master` with `scripts/check-cran.sh`, require `0 errors, 0 warnings, 0 notes`, and confirm that the archive contents match the reviewed release candidate. Reinspect any content that differs before continuing.
3. Create and inspect an annotated tag on that exact commit:

   ```bash
   git tag -a 0.3.0 -m "Release 0.3.0"
   git show --no-patch --format=fuller 0.3.0
   ```

4. Push only the verified tag, then create a GitHub release titled `0.3.0` using the reviewed `NEWS.md` section as the basis for its notes. Attach only the exact inspected tarball if a source artifact is intentionally provided.
5. Recheck repository visibility, tag target, release notes, assets and checksums on GitHub.
6. Perform a minimal post-publication installation from the published source and verify package version, loading and one canonical EDA summary invocation.
7. Record the release URL, tag commit, artifact checksum, publication time and post-publication smoke-test result in `future/changelog.md`; update the relevant TODO items and issue status separately.

Exit criteria:

- Tag `0.3.0` and the GitHub release are public, point to the approved commit, contain only reviewed material, and pass the post-publication smoke test.

## Recovery

- Before a tag is pushed: correct the release branch, rerun all affected checks and repeat review. A local unpushed tag may be deleted and recreated after verifying the target.
- After a tag is pushed or a release is public: do not silently move or reuse the tag. Withdraw a defective release if necessary and publish a new patch version after diagnosis and validation.
- After an accidental disclosure: revoke credentials or contain the data first, preserve evidence needed for assessment, notify the owner, and use a separately approved remediation plan for Git history or published assets.
- After a failed check or walkthrough: retain the failure evidence, return to the earliest affected phase, implement the smallest justified correction, and rerun both the focused verification and every downstream gate.

## Required handoff evidence

At completion, report:

- Baseline and tagged commit hashes.
- Scrub findings and dispositions, including secret-scanner coverage and fixture provenance review.
- Spec 010 approval, acceptance and verification record.
- Human walkthrough record and owner decision.
- Commands and outcomes for local, CRAN-oriented, coverage and GitHub checks.
- Exact inspected artifact path, inspection method and checksum.
- Compatibility changes, accepted limitations and checks not run.
- Tag and release URLs and post-publication smoke-test result.
- Whether each review was a self-check, automated check or independent human check.
