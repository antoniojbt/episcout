# Release-readiness audit

Status: Completed self-review; release remains blocked
Audit date: 2026-08-07
Baseline commit: `6a117a06678f1ec020d5b59dbab236339bd1b1e7` (rewritten equivalent of the original audited commit)
Branch: `refactor/release-readiness-audit`
Roadmap: issue [#204](https://github.com/antoniojbt/episcout/issues/204), item 3

## Outcome

The repository, package source, external fixtures, completed canonical EDA contract and current local/CRAN-oriented baseline were audited without changing package behaviour. Local package validation is clean, the mandatory GitHub PostgreSQL and coverage evidence is current, and the installed source archive exposes the intended package and four vignettes. Release `0.3.0` must not begin until the release blockers below are resolved or, where explicitly required, the repository owner records a decision.

This was an implementation self-review supported by automated checks and official source reconciliation. It is not an independent human review, does not satisfy the release plan's human walkthrough and does not authorise a tag, GitHub release or CRAN submission.

## Finding dispositions

| ID | Classification | Finding | Disposition |
| --- | --- | --- | --- |
| AUD-001 | Release blocker; owner gate | A redacted Gitleaks 8.30.1 scan of all 424 commits found exactly the already-recorded historical Codecov credential location and no additional secret. The current tree and source archive contain no detected secret. | The owner subsequently authorised removal on 2026-08-07. Rewritten upstream `master` and fork heads are published; five upstream tags, credential revocation/rotation, protected-branch verification and eligible GitHub cache cleanup remain. |
| AUD-002 | Release blocker | The prescribed `scripts/check-local.sh` then `scripts/check-cran.sh` sequence leaves an ignored `tests/testthat/Rplots.pdf` that enters the source tarball. Four packaged test files also retain obsolete developer-specific absolute paths, and unused `vignettes/R_datasets.xlsx` enters the archive. | Resolve issue [#208](https://github.com/antoniojbt/episcout/issues/208) through spec `023-package-source-hygiene` on its own branch. Do not remove intended visual fixtures or change package behaviour. |
| AUD-003 | Release blocker | Both external fixture CSVs match their declared upstream package objects exactly, but the repository does not yet record immutable source archive checksums, fixture checksums and a complete licence/redistribution/attribution record. | Resolve issue [#209](https://github.com/antoniojbt/episcout/issues/209) through spec `024-external-fixture-provenance` on its own branch. Keep routine tests offline and expectations independent. |
| AUD-004 | Item 4 release work | `scripts/check-cran.sh` reports one incoming-feasibility NOTE: new-submission status, a VignetteBuilder package with no prebuilt CRAN index, and two Stack Overflow URLs returning 403. The built package itself installs correctly and lists all four intended vignettes. | Retain in release issue [#81](https://github.com/antoniojbt/episcout/issues/81): replace/remove unstable links at their roxygen sources, record the first-submission explanation and reconcile the vignette diagnostic against the installed candidate. |
| AUD-005 | Expected environment difference | The local source checkout skips service-gated PostgreSQL tests, installed-package parallel plotting and disabled visual snapshots. The mandatory PostgreSQL 17 integration and coverage jobs passed on PR #207. | Keep PostgreSQL opt-in locally and mandatory in CI. The roadmap already leaves visual-regression redesign outside the ordered programme; do not regenerate snapshots and call them independent truth. |
| AUD-006 | Follow-up, not a demonstrated release defect | The 2026-07-31 guidance still identifies unpromoted truth work for contingency methods, correlation/shape/normality references, temporal boundaries, survival/legacy plots and selected helper semantics. | Keep these as separately triaged candidate work. They affect released legacy helpers but the audit found no new regression or contradictory result in the `0.3.0` development delta. Do not fold them into release cleanup. |
| AUD-007 | Accepted repository-history limitation | `git diff --check 0.2.0..HEAD` reports trailing spaces in historical planning records, while `git diff --check` for the current tree is clean. The repository also retains tracked archive files that match `.gitignore`, the historical `v0.1.3` tag naming inconsistency and five harmless dangling blobs. | Do not rewrite history or broaden the release diff. Planning and archive directories remain excluded from package source. |

## Baseline and repository integrity

- Local `master`, fork `origin/master` and authoritative `upstream/master` all resolved to the merged PR #207 commit before the branch was created; both comparisons were `0 0`.
- The repository is public. The worktree was clean except for ignored `build/` and a generated `tests/testthat/Rplots.pdf`; no untracked non-ignored file and no file larger than 5 MB was found.
- `git fsck --full` reported five dangling blobs and no corrupt or missing object. The repository contained 507 loose objects (2.39 MiB) and 4,696 packed objects (2.06 MiB).
- There is one worktree and no submodule. Branches and historical tags were inspected; no branch, tag, worktree or object was deleted.
- The release baseline is tag `0.2.0`; after the authorised rewrite the equivalent audit commit describes as `0.2.0-79-g6a117a0`.
- The environment was R 4.5.3 on Ubuntu 24.04.4 with the repository mamba library, OpenBLAS 0.3.33 and UTC runtime timezone.

## Package checks and coverage

- `scripts/check-local.sh` passed documentation, package-loaded lint, the full source test suite and package check with `0 errors, 0 warnings, 0 notes`. Fourteen source-test skips were reported: ten PostgreSQL EDA cases, two PostgreSQL pseudonymisation cases, installed-package parallel plotting and disabled vdiffr snapshots. The check's generated Rd and snapshot-pruning side effects were restored to the exact baseline before the CRAN-oriented run.
- `scripts/check-cran.sh` built and checked `episcout_0.2.0.tar.gz` with `0 errors`, `0 warnings` and `1 NOTE`. The NOTE combined expected new-submission status, the prebuilt-vignette-index diagnostic and two Stack Overflow 403 responses. PDF and HTML manuals passed; the former old-HTML-Tidy NOTE did not recur.
- A local `covr::package_coverage()` run without the PostgreSQL service measured 65.2787% before an audit-only table-formatting expression failed after the percentage was printed. This is not comparable to the mandatory CI run because the large PostgreSQL integration surfaces were skipped locally.
- Codecov reports 91.55% at `806b3e2` (8,114 of 8,862 lines), up 0.05 percentage points from 91.50% at the preceding merged commit. PR #207's coverage, Codecov project/patch, PostgreSQL 17, Ubuntu, macOS and CodeFactor checks all passed.

## Source archive and installed-package inspection

- The exact audit archive was 784 KiB with 373 members. It correctly excluded `future/`, `archive/`, `outputs/`, `checklists/`, `.github/`, scripts, build material, prior `.gitkeep` placeholders, histories and agent state.
- The archive incorrectly included `tests/testthat/Rplots.pdf`, four commented developer paths and the unused 2019 `vignettes/R_datasets.xlsx` workbook. The workbook contains public R-dataset inventory sheets and no repository caller; it was not treated as confidential, but its package purpose is no longer demonstrated.
- Gitleaks directory scanning reported zero finding in the extracted archive. A separate local-path/credential-boundary scan found only the four obsolete commented paths, intentional `PGPASSWORD` environment-variable handling and tests that assert credential names do not leak; no value was recorded.
- Installing the exact tarball into an isolated library succeeded. `vignette(package = "episcout")` listed `epi_stats_dates_example`, `introduction_episcout`, `longitudinal-pseudonymisation` and `specification-first-eda`. A smoke invocation returned canonical components `variables`, `numeric`, `categorical`, `text`, `temporal` and `skipped`.
- The archive contains no binary executable. Tracked non-text material is limited to SVG visual references and the workbook identified above. No tracked database, dump, private-key, credential, environment or log file was found.

## Fixture provenance and truth review

- `blood_storage.csv` is byte-for-byte the documented `write.csv(..., row.names = FALSE, na = "")` serialization of `medicaldata::blood_storage` from version 0.2.0. The source package is MIT-licensed teaching data, its official documentation describes the dataset as cleaned and complete, and the package website describes its datasets as de-identified and obtained through donations, other packages, reconstruction or the Teaching Statistics in the Health Sciences project.
- `penguins_raw.csv` is byte-for-byte the documented `write.csv(..., row.names = FALSE, na = "NA")` serialization of `palmerpenguins::penguins_raw` from version 0.1.1. The official package documentation states that the 344-row, 17-variable raw dataset is available under CC0 and traces the three species sources to Environmental Data Initiative packages.
- Official source archives downloaded on 2026-08-07 had SHA-256 `56dab0c6078e6f9a9f183427a4481c5497e5d107b795bf965cc7ce4ac4c39236` for `medicaldata_0.2.0.tar.gz` and `2a40d48ba6c7978fdf2a6daf647ccb39cd17590680138931d11194d3dd1a30b4` for `palmerpenguins_0.1.1.tar.gz`.
- Pinned fixture SHA-256 values were `e3a1c6b83de9ddae8380ef2a92ce995fe927c5a176c589039d8b6089dae812b9` for `blood_storage.csv` and `a634e85f0676c74c4cd73f94ff8cbf9ec12540d01797434cf1fd0ba8d9af663f` for `penguins_raw.csv`.
- Ordinary tests are deterministic and offline. The manual generator uses simple base R calculations, calls no `episcout` function and reads the serialized fixture before deriving expected output. Executable tests consume every expected CSV.
- Missingness and penguin numeric/categorical expectations are independently calculated from pinned observations. Schema projection files are explicitly labelled as regression projections because their classifier mirrors historical package logic. Small hand-authored tests independently establish type compatibility, missing/sentinel handling, numeric edge values, categorical denominators, literal `"NA"`, all-missing states and canonical reconciliation.
- The blood-storage fixture mainly establishes realistic workflow coverage and independently calculated missingness; it is not evidence that every clinical semantic label or analytical function is externally validated. This limitation remains explicit.

## Release-delta reconciliation

- Completed specs 010, 019 and 020 are merged. The canonical EDA API has no active summary-version selector or legacy two-table adapter; the only remaining `v1` wording names the current semicolon-delimited specification encoding.
- All 17 exports added since `0.2.0` have generated Rd aliases, tests and user-facing documentation. Package checks found no missing documentation or undeclared runtime dependency.
- The changed `epi_clean_merge_nested_dfs()` full-outer default is explicit in roxygen/Rd and NEWS, with `all.x = TRUE, all.y = FALSE` documented for legacy left joins.
- Current README, NEWS, vignettes and installed help describe the PostgreSQL, preparation, stratified, intake and pseudonymisation workflows without claiming anonymity or automatic disclosure control.
- Deferred issue #196 and post-release specs 021/022 have not entered package implementation. No tag, release, CRAN upload, credential operation or destructive history action occurred.

## Guidance review

- The still-relevant 2026-07-31 findings were classified above rather than replayed from stale truth-review statuses. Completed repairs and spec 010 are not reopened.
- `future/scratch/repo-specific-spec-design-guidance-draft.md` is substantially consistent with `AGENTS.MD`, current templates and checklists, but repeats their intake, evidence, privacy and activation rules. It should remain a non-authoritative scratch record; promoting it wholesale would add duplication without changing current practice. Its useful requirement to disclose material runtime-skill effects is already enforced by the active tool instructions and can be reconsidered only if the owner later revises repository authority.

## Official sources consulted

- CRAN Repository Policy, <https://cran.r-project.org/web/packages/policies.html>, accessed 2026-08-07.
- Writing R Extensions, <https://cran.r-project.org/doc/manuals/r-release/R-exts.html>, accessed 2026-08-07.
- `medicaldata` package record and documentation, <https://CRAN.R-project.org/package=medicaldata> and <https://higgi13425.github.io/medicaldata/>, accessed 2026-08-07.
- `palmerpenguins` package and raw-data documentation, <https://allisonhorst.github.io/palmerpenguins/> and <https://allisonhorst.github.io/palmerpenguins/reference/penguins_raw.html>, accessed 2026-08-07.

## Gate to the next work

Issues #208/#209 are implemented together through specs 023/024. After their PR merges, complete the remaining Codecov tag/credential/cache containment before release item 4. Item 4 then remains subject to its isolated human walkthrough, exact-artifact verification and explicit tag/GitHub-release approval; CRAN polish and submission are deferred.
