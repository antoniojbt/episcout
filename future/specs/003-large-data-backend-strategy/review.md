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
- A five-minute claim without a fixed snapshot, warm-up, repetition count, included stages, host/runtime metadata and redacted record would not be reviewable. The SDD now fixes three measured end-to-end runs after one warm-up and uses the median threshold of 300 seconds.
- The representative workload establishes operational acceptance but cannot be copied into this public repository. Neutral fixture correctness and the external performance/data-locality record are separate evidence classes.

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
| Success metrics map to evidence | Brief M-001 through M-006 map to TDD parity, fetch instrumentation, canaries, snapshot tests, external timing and regressions. | Ready |
| Reproducibility is concrete | SDD fixes source/spec fingerprints, runtime versions, snapshot/isolation, sequential order, benchmark repetitions and nondeterministic metadata. | Ready |
| Governance/privacy limits are explicit | Specification authority, database immutability, caller infrastructure responsibility, artifact sensitivity and disclosure-control exclusions are fixed. | Ready |
| Open questions are non-blocking | External workload availability is recorded as an acceptance dependency; implementation stop conditions cover scope-changing discoveries. | Ready |

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
7. Complete documentation, live PostgreSQL verification, external benchmark/data-frame limitation evidence and independent bounded reviews.
8. Reconcile TODO/changelog/spec records before commit, PR and owner acceptance.

## Implementation Evidence — 2026-08-05

Implementation was completed on `feature/postgresql-eda-backend` from the synchronised `master` tip. The active specification remains in place because the external workload benchmark, the three independent reviews and owner acceptance are not available in this environment.

- The exported source and run interfaces, explicit profiler dispatch, PostgreSQL catalogue/type boundary, checked aggregate query boundary, repeatable-read/read-only transactions, identifier QA, compact plot data, shared renderers and owned staged bundle are implemented in `R/eda_postgres_source.R`, `R/eda_postgres_queries.R`, `R/eda_db_run.R` and the existing EDA modules.
- Neutral tests cover the complete reviewed storage matrix, enums, zero-row schemas, standard and sentinel missingness, NaN/infinities, type-7 edges for sample sizes 1 through 12, e1071 type-3 shape statistics, Unicode text, dates/timestamps, identifier policy, unusual quoted relation names, all five supported relation kinds, catalogue drift, active-transaction rejection, stable concurrent snapshots, query cleanup, fetch limits, overwrite ownership and checksum refusal.
- The disposable live run used PostgreSQL 17.10, R 4.5.3, DBI 1.3.0 and the locally provisioned RPostgres 1.4.8 package. The pull-request PostgreSQL job remains the mandatory clean-environment run and installs its R dependencies independently.
- `devtools::test(filter = "eda-postgres|sec-pseudonymise-postgres", reporter = "summary")` passed the new EDA integration expectations and all 119 existing PostgreSQL security expectations.
- Package-loaded `lintr::lint_package()` completed with no findings. The complete `devtools::test(reporter = "summary")` suite passed with only the expected environment-gated PostgreSQL/visual skips.
- `scripts/check-local.sh` completed with 0 errors, 0 warnings and 0 notes. `scripts/check-cran.sh` completed with 0 errors, 0 warnings and one inherited/external NOTE covering new-submission metadata, the absent prebuilt vignette index and two Stack Overflow URL 403 responses.
- `vignettes/specification-first-eda.Rmd` rendered successfully after loading the working package. Representative exact delivered SVGs for numeric histograms, aggregate quantile boxes, categorical collapse, text lengths and temporal bins were converted only for inspection and visually checked for labels, counts and privacy.
- The repository/client-artifact canary audit found only deliberately authored test canaries and environment-variable reads. Bundle and SVG assertions exclude raw text observations, identifiers, SQL, connection attributes and credentials; the reviewed specification exception remains documented.

## Checklist Evidence

| Checklist | Implementation evidence | Status |
| --- | --- | --- |
| Software verification | Fixed formals/classes/order, explicit dispatch, lifecycle/fetch spies, live transaction cleanup, manifest ownership and regression suites. | Self-review complete |
| Truth and semantics | Specification authority, technical/type incompatibilities, typed sentinels, identifier policy, UTC boundary and privacy limitations are explicit in code, tests and documentation. | Self-review complete |
| Analysis and statistics | Independent type-7/e1071 expectations, exact count conversion, finite partitions, fences/outliers, Shapiro bounds and zero-row reconciliation pass locally and live. | Self-review complete |
| Figures | Compact data reconcile before rendering; text uses lengths; identifiers are `NULL`; representative delivered SVGs were visually inspected. | Self-review complete |
| Copy edit | README, NEWS, roxygen help, vignette, project map and future records agree on supported behaviour and limitations. | Self-review complete |
| Render and release | Help/vignette generation, source build, local/CRAN checks and delivered SVG inspection completed; no release or tag was created. | Self-review complete |

## Required Independent Reviews

- Statistical parity and anti-circularity: canonical definitions, type-7 evidence, shape formulae, tolerances, denominators and fixture provenance.
- PostgreSQL/read-only/privacy: type/collation/time semantics, SQL quoting/binding, snapshot/lifecycle, no writes, bounded collection, conditions and artifact canaries.
- Bundle/plot/documentation: overwrite/recovery authority, manifest truth, compact plot reconciliation, rendered SVG/help/vignette usability and disclosure limitations.

Independent reviewers do not replace primary integration ownership. Any conflict with the reviewed SDD is resolved before implementation proceeds.

## External Benchmark Evidence Template

Complete this table without restricted or project-specific names. Retain the detailed private evidence with the named custodian outside the repository.

| Field | Evidence |
| --- | --- |
| Opaque workload ID | Pending |
| Custodian and date | Pending |
| Relation kind | Pending |
| Order-of-magnitude rows/columns | Pending |
| Specification type mix | Pending |
| PostgreSQL/R/episcout/DBI/RPostgres versions | Pending |
| Coarse CPU/RAM class | Pending |
| Warm-up status | Pending |
| Run seconds 1/2/3 | Pending |
| Median seconds | Pending |
| Maximum client rows by query kind | Pending |
| Peak R memory method/value | Pending |
| Reconciliation/privacy status | Pending |
| Data-frame limitation basis | Pending |

## Open Questions

None blocking activation. Stop for owner review under any SDD stop condition, or if the external workload/spec will not be available for final performance acceptance.

## Closeout Notes

The executable implementation and local verification are complete, including live PostgreSQL 17 statistical, snapshot, privacy and bundle evidence. Pull-request CI is an additional publication gate, not a substitute for the unresolved acceptance dependencies. This is not completed-spec status: the external representative benchmark, three required independent reviews and owner acceptance remain pending. The spec stays active and no release or tag is authorised.
