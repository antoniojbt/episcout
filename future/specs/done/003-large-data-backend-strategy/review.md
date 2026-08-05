# Review Notes

Spec ID: `003-large-data-backend-strategy`
Status: Active

## Planning Findings

- The prior draft named PostgreSQL and the intended outputs but did not define a source object, exact API dispatch, supported relation/type matrix, transaction boundary, per-statistic collection rule, fixed result/bundle schema, failure semantics or redacted benchmark evidence. Those gaps would have allowed materially different implementations to claim compliance.
- Preserving the canonical six-component contract rules out adding identifier QA as a seventh summary component. The direct summary records the existing identifier-policy skip, while the database run exposes a separate aggregate-only QA artifact.
- Preserving the direct plot-list shape rules out replacing one variable element with a nested multi-plot structure. The primary plot remains one named element; the owned bundle may add a quantile-box SVG through the same compact-data/rendering internals.
- Current text plots enumerate raw strings and scale poorly. A length-distribution plot is the narrow privacy/scalability correction required by this backend and is applied consistently to data frames and PostgreSQL; raw text remains absent from plot data.
- PostgreSQL `timestamp without time zone` cannot establish the instant or DST policy needed by the existing UTC datetime summaries. V1 requires native `timestamptz` or a caller-reviewed view cast rather than using the session timezone.
- Multiple sequential queries at read-committed isolation could mix source states. Each public call therefore owns a repeatable-read, read-only transaction and rejects a caller-owned transaction rather than nesting or committing caller work.
- Exact parity does not mean byte-identical floating-point reductions. Discrete results remain exact; continuous fields have a fixed tolerance and type-7/shape formulas remain authoritative. Approximate PostgreSQL functions/extensions are not permitted.
- Shapiro testing is the only canonical statistic that intrinsically requires a small vector. Its existing eligibility cap gives a hard collection bound of 4,999 finite values; all other non-categorical results are scalar or fixed-bin aggregates.
- Complete categorical frequencies are part of the released canonical contract and may be unbounded by cardinality. This exception must be visible in query evidence and disclosure classification; display-only collapse cannot alter the canonical table.
- A performance claim without a fixed snapshot, warm-up, repetition count and included stages would not be reviewable. The owner-approved acceptance amendment defines a deterministic one-million-row synthetic regression gate: one warm-up, three complete measured runs and a median below 120 seconds in dedicated PostgreSQL CI.
- The synthetic gate can detect a severe regression but cannot establish production runtime or comparative data-frame performance. Data locality is instead established by the enforced query/fetch boundary and the fact that the ordinary data-frame workflow necessarily materialises selected source rows in R.

## Skill Influence

The environment-level `ds-pipeline-designer` workflow kept this revision design-only and prompted explicit stage input/output contracts, validations, failure modes, provenance, success-metric mapping and non-goals. It is outside the repository at `$CODEX_HOME/skills/ds-pipeline-designer/` and is not project authority. The repository's established SDD/TDD format remained authoritative, so the skill's Python defaults, scaffolder, generic artifact names and library guidance were not adopted. After owner concern about hidden behaviour, the potentially useful practices were extracted into `future/scratch/repo-specific-spec-design-guidance-draft.md` as a visible, explicitly non-active proposal for later review; future project authority depends on owner-approved repository text, not this skill.

## Checklist Routing

- `checklists/software-verification.md` applies to the public source/run interfaces, backend dispatch, connection/result lifecycle, stable schemas, bounded fetches, staging/overwrite behaviour, failures, compatibility and all verification commands.
- `checklists/truth-and-semantics.md` applies to reviewed specification authority, technical-versus-semantic types, missing sentinels, identifiers, timezone meaning, provenance, privacy and performance claims.
- `checklists/analysis-and-statistics.md` applies to every canonical statistic, finite/missing partitions, denominators, type-7 quantiles, shape formulae, Shapiro eligibility, outliers, temporal units, reconciliation and independent fixture expectations.
- `checklists/figures.md` applies to compact plot data, exclusions, bins, level collapse, axes/labels, SVG rendering and exact delivered-file inspection.
- `checklists/copy-edit.md` applies to README, NEWS, roxygen, vignette and all user-facing condition/privacy/performance language.
- `checklists/render-and-release.md` applies to rendered help/vignettes, SVGs, source-package installation/discovery and package/check artifacts. It does not authorise a release or tag.

## Design Review Against Quality Gates

This activation review is the primary designer's self-review, not an independent implementation review. The independent reviews listed below remain mandatory after executable behaviour and evidence exist.

| Gate | Evidence | Status |
| --- | --- | --- |
| Every stage has I/O contracts | SDD stages S-01 through S-06 define inputs, outputs, validations and cleanup. | Ready |
| Success metrics map to evidence | Brief M-001 through M-006 map to TDD parity, fetch instrumentation, canaries, snapshot tests, fixed synthetic timing and regressions. | Ready |
| Reproducibility is concrete | SDD fixes source/spec fingerprints, runtime versions, snapshot/isolation, sequential order, benchmark repetitions and nondeterministic metadata. | Ready |
| Governance/privacy limits are explicit | Specification authority, database immutability, caller infrastructure responsibility, artifact sensitivity and disclosure-control exclusions are fixed. | Ready |
| Open questions are non-blocking | The owner selected the fixed synthetic threshold; implementation stop conditions cover scope-changing discoveries. | Ready |

## Baseline Evidence

The pre-code baseline was recorded on 2026-08-04 at detached commit `87d40dce5a793355d509d58f2a0a595b7e691966`, before any package-file change. The planned implementation branch is `feature/postgresql-eda-backend`; implementation must switch to and verify that branch rather than changing package code on detached HEAD. The wrapper used R 4.5.3 with DBI 1.3.0 and RPostgres 1.4.10.

- Package-loaded `lintr::lint_package()` completed with no findings.
- The direct full suite reproduced one inherited macOS historical daylight-saving ambiguity failure at `tests/testthat/test-eda-prepare.R:223` and four expected skips. The runner returned success despite reporting the failed expectation, so the textual test result, not only its process status, is the baseline authority.
- `scripts/check-local.sh` completed documentation, lint, tests, build and check, then failed with 1 check error because the same inherited DST expectation and an environment-specific PSOCK `serverSocket()` restriction at `tests/testthat/test-utility.R:73` produced two test failures. It recorded 1,626 passes, six skips and the known one-core warning. Package check also recorded two environment/worktree notes: `.git` was included from the linked worktree context and current time could not be verified.
- `scripts/check-cran.sh` likewise built the source package/manual and recorded 1,626 passes, six skips, the same two failures and one warning, ending with 1 error and four notes. The additional notes were unavailable CRAN/URL incoming checks plus the missing prebuilt vignette index, and unavailable recent HTML Tidy validation. Network resolution was unavailable in the sandbox.
- Documentation generation reported three inherited missing external `@example` files for `epi_stats_format.R`, `epi_stats_summary.R` and `epi_stats_tidy.R`; package documentation/check continued. Check-generated Rd drift and the known disabled-vdiffr snapshot cleanup were restored, leaving only planning files changed.

These results are baseline evidence, not permission to weaken the DST, multicore, documentation, snapshot or package-check contracts. Final verification must distinguish inherited defects, sandbox limitations, external/network notes and spec-003-created failures. Live PostgreSQL tests were skipped in the ordinary baseline and become mandatory for implementation acceptance.

## Implementation Checkpoints

1. Record baseline and verify the intended feature branch before package-code changes.
2. Add neutral independently justified fixture expectations and source/privacy/transaction tests before backend implementation.
3. Implement and verify the source/type/snapshot boundary before statistical queries.
4. Implement missingness and canonical components one type family at a time, reconciling every count and collection bound.
5. Separate compact plot-data preparation from rendering, preserve direct plot-list compatibility and inspect exact SVGs.
6. Extract/reuse owned-bundle helpers only behind intake regression tests, then implement database run publication and fault injection.
7. Complete documentation, live PostgreSQL verification, the fixed synthetic benchmark, structural data-locality evidence and independent bounded reviews.
8. Reconcile TODO/changelog/spec records before commit, PR and owner acceptance.

## Implementation Evidence — 2026-08-05

Implementation was completed on `feature/postgresql-eda-backend` from the synchronised `master` tip. The active specification remains in place while the fixed synthetic benchmark, three independent reviews and owner acceptance are completed on `feature/postgresql-eda-acceptance`.

- The exported source and run interfaces, explicit profiler dispatch, PostgreSQL catalogue/type boundary, checked aggregate query boundary, repeatable-read/read-only transactions, identifier QA, compact plot data, shared renderers and owned staged bundle are implemented in `R/eda_postgres_source.R`, `R/eda_postgres_queries.R`, `R/eda_db_run.R` and the existing EDA modules.
- Neutral tests cover the complete reviewed storage matrix, enums, zero-row schemas, standard and sentinel missingness, NaN/infinities, type-7 edges for sample sizes 1 through 12, e1071 type-3 shape statistics, Unicode text, dates/timestamps, identifier policy, unusual quoted relation names, all five supported relation kinds, catalogue drift, active-transaction rejection, stable concurrent snapshots, query cleanup, fetch limits, overwrite ownership and checksum refusal.
- The disposable live run used PostgreSQL 17.10, R 4.5.3, DBI 1.3.0 and the locally provisioned RPostgres 1.4.8 package. The pull-request PostgreSQL job remains the mandatory clean-environment run and installs its R dependencies independently.
- `devtools::test(filter = "eda-postgres|sec-pseudonymise-postgres", reporter = "summary")` passed the new EDA integration expectations and all 119 existing PostgreSQL security expectations.
- Package-loaded `lintr::lint_package()` completed with no findings. The complete `devtools::test(reporter = "summary")` suite passed with only the expected environment-gated PostgreSQL/visual skips.
- `scripts/check-local.sh` completed with 0 errors, 0 warnings and 0 notes. `scripts/check-cran.sh` completed with 0 errors, 0 warnings and one inherited/external NOTE covering new-submission metadata, the absent prebuilt vignette index and two Stack Overflow URL 403 responses.
- `vignettes/specification-first-eda.Rmd` rendered successfully after loading the working package. Selected exact delivered SVGs for numeric histograms, aggregate quantile boxes, categorical collapse, text lengths and temporal bins were converted only for inspection and visually checked for labels, counts and privacy.
- The repository/client-artifact canary audit found only deliberately authored test canaries and environment-variable reads. Bundle and SVG assertions exclude raw text observations, identifiers, SQL, connection attributes and credentials; the reviewed specification exception remains documented.

## Checklist Evidence

| Checklist | Implementation evidence | Status |
| --- | --- | --- |
| Software verification | Fixed formals/classes/order, explicit dispatch, lifecycle/fetch spies, live transaction cleanup, manifest ownership and regression suites. | Self-review complete |
| Truth and semantics | Specification authority, technical/type incompatibilities, typed sentinels, identifier policy, UTC boundary and privacy limitations are explicit in code, tests and documentation. | Self-review complete |
| Analysis and statistics | Independent type-7/e1071 expectations, exact count conversion, finite partitions, fences/outliers, Shapiro bounds and zero-row reconciliation pass locally and live. | Self-review complete |
| Figures | Compact data reconcile before rendering; text uses lengths; identifiers are `NULL`; selected delivered SVGs were visually inspected. | Self-review complete |
| Copy edit | README, NEWS, roxygen help, vignette, project map and future records agree on supported behaviour and limitations. | Self-review complete |
| Render and release | Help/vignette generation, source build, local/CRAN checks and delivered SVG inspection completed; no release or tag was created. | Self-review complete |

## Required Independent Reviews

- Statistical parity and anti-circularity: canonical definitions, type-7 evidence, shape formulae, tolerances, denominators and fixture provenance.
- PostgreSQL/read-only/privacy: type/collation/time semantics, SQL quoting/binding, snapshot/lifecycle, no writes, bounded collection, conditions and artifact canaries.
- Bundle/plot/documentation: overwrite/recovery authority, manifest truth, compact plot reconciliation, rendered SVG/help/vignette usability and disclosure limitations.

Independent reviewers do not replace primary integration ownership. Any conflict with the reviewed SDD is resolved before implementation proceeds.

## Independent Acceptance Reviews — 2026-08-05

Three read-only reviewers independently examined the amended implementation and regression evidence. All initial findings were resolved and all final reviews reported no acceptance blocker.

- Statistical parity and anti-circularity: the reviewer required complete independently stated expectations for numeric, integer, missingness, categorical/binary, text, temporal, variables and skipped frames. The final fixture now fixes every denominator, exercises asymmetric type-7 and discriminating e1071 type-3 values, and uses the documented combined floating tolerance. No statistical, provenance or synthetic-claim blocker remains.
- PostgreSQL lifecycle, read-only behaviour, bounded collection and privacy: the reviewer required fixture cleanup to drop only a successfully created owned schema and found that native database errors, notices and warnings could otherwise expose server text. Cleanup ownership is now explicit; query, statement, result-cleanup and transaction lifecycle actions muffle native text and re-signal fixed value-free conditions while preserving message/warning semantics; injected canaries and a live PostgreSQL `RAISE NOTICE` regression pass. No lifecycle, SQL, read-only, collection or privacy blocker remains.
- Bundle ownership, plots, documentation and disclosure labelling: the reviewer required hard dependency failures under the benchmark gates, explicit canonical categorical results and exact SVG/manifest reconciliation. The final benchmark checks dependencies before connection acquisition, requires nine non-empty SVGs, independently verifies all published checksums, and fixes the 20-level categorical and two-level binary results. No bundle, plot, documentation or benchmark blocker remains.

The PostgreSQL/privacy review retains only the documented limitations: complete categorical frequencies may be large or sensitive, server-side logging remains infrastructure-controlled, and abrupt process termination may leave a disposable test fixture until its container is removed.

## Acceptance Amendment — 2026-08-05

The owner directed the specification to use a fixed synthetic threshold with an explicit boundary against production-performance claims. The amendment does not weaken the canonical, snapshot, privacy, bounded-fetch or bundle contracts.

The committed fixture contains 1,000,000 deterministic PostgreSQL rows and eight reviewed variables spanning identifier, numeric, integer, categorical, binary, text, date and timestamp semantics. Table construction and `ANALYZE` occur before timing. One warm-up creates the owned bundle; three complete measured calls validate and overwrite that bundle with plots enabled. The median must remain below 120 seconds in the dedicated PostgreSQL integration job. Coverage does not set the benchmark gate because instrumentation would invalidate the timing comparison.

The planning probe used PostgreSQL 17 with the same fixed fixture shape and complete rendering/publication path. It recorded 7.252 seconds for warm-up and 6.877 seconds for one measured overwrite. These values selected a high-headroom 120-second CI ceiling; they are planning evidence, not the required three-run acceptance result or a production-runtime promise.

## Synthetic Performance Evidence

| Field | Evidence |
| --- | --- |
| Fixture | 1,000,000 rows; eight mixed-type reviewed variables; deterministic modular missingness; 20 categorical levels |
| PostgreSQL/R/episcout/DBI/RPostgres versions | PostgreSQL 17.10; R 4.5.3; episcout 0.2.0; DBI 1.3.0; RPostgres 1.4.10 |
| Warm-up status | Complete before measurement |
| Run seconds 1/2/3 | 7.463 / 7.216 / 7.319 locally |
| Median seconds | 7.319 locally; required `< 120` in dedicated PostgreSQL CI |
| Maximum client rows | Categorical `treatment = 20`, binary `flag = 2`, every non-categorical query kind at most `30` |
| Reconciliation and manifest checksums | All three measured runs completed; the focused test independently recomputed every created non-manifest checksum after each publication |
| Claim boundary | Synthetic regression/scalability gate only; no production-runtime or comparative data-frame claim |
| Data-frame limitation basis | Full materialisation scales client memory and disclosure exposure with source rows; the PostgreSQL client boundary collects aggregates and bounded vectors subject to complete categorical frequencies |

## Acceptance Verification — 2026-08-05

- The focused PostgreSQL 17 benchmark, parity and source suite passed after the final review changes. The complete mandatory PostgreSQL filter, including the existing pseudonymisation integration tests, also passed; the disposable PostgreSQL 17 container and its fixture were then removed.
- Package-loaded `lintr::lint_package()` reported no findings, the GitHub Actions workflow parsed as valid YAML, and `git diff --check` passed.
- `scripts/check-local.sh` completed with 0 errors, 0 warnings and one environment NOTE because current time could not be verified. Check-generated Rd and disabled-vdiffr snapshot drift were restored.
- `scripts/check-cran.sh` completed with 0 errors, 0 warnings and three inherited/external environment NOTEs: incoming new-submission/vignette-index and Stack Overflow 403 findings, unavailable current-time verification, and an outdated local HTML Tidy executable. Tests, examples, vignettes, PDF manual and package loading all passed.
- PR #201 passed its dedicated PostgreSQL integration job, including the fixed synthetic gate, plus macOS/Ubuntu package checks, coverage, Codecov patch/project, CodeQL and CodeFactor. The threshold was not weakened.

## Open Questions

None blocking the amended acceptance work. Stop for owner review under any SDD stop condition or if the fixed synthetic gate cannot pass without changing its threshold or the reviewed implementation contract.

## Closeout Notes

The executable implementation, fixed synthetic gate, independent statistical/PostgreSQL/privacy/bundle reviews and all PR #201 checks completed without unresolved blocker. The repository owner merged PR #201 on 2026-08-05, closing issue #194 and recording acceptance. The spec is completed and moved to `future/specs/done/`; no release or tag was created.
