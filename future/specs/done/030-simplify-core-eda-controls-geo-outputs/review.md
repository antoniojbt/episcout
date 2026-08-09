# Review

Spec ID: `030-simplify-core-eda-controls-geo-outputs`
Status: Completed

## Planning Review

Draft planning/prerequisite PR #244 contains only spec 030, synchronised planning records and the quoted R-wrapper repair. At head `8c4935f`, Bash syntax, online/offline checks in the canonical checkout, online/offline checks in a fresh real checkout path containing spaces and `git diff --check` pass. macOS, Ubuntu, PostgreSQL integration, coverage, both Codecov gates and CodeFactor are green; the PR is mergeable/CLEAN and has no actionable review feedback.

Confirm before activation:

1. Core EDA produces requested outputs but makes no sharing/approval decision.
2. Geo mapping remains explicit point geometry from complete declared pairs only.
3. `max_map_points` is inclusive, no failed pair is partially mapped and PostgreSQL never truncates.
4. PostgreSQL observation collection is minimal and contained within existing read-only snapshot ownership.
5. High-cardinality explicit text themes are not silently collapsed or capped.
6. Core five-column manifests are separate from unchanged specialised security manifests.
7. `linkage$columns`, rather than the semantic dictionary, owns privacy/action/validation decisions.
8. Immediate breaks have targeted migration errors and documentation, not shims.
9. PostgreSQL HTML bundle rendering remains out of scope under issue #196.

## Implementation Review

Implementation evidence at the pre-PR branch head:

- Full offline suite: 2,134 passed, 24 expected environment-gated skips, no failures or warnings.
- Final focused intake suite: 129 passed with no skips, failures or warnings; local `covr` reports 91.95% package coverage and 94.96% coverage for `R/eda_intake.R`.
- Live PostgreSQL 18 suites: 388 passed across coordinate QA, map collection, catalogue profiling, snapshot/parity, identity-universe and pseudonymisation cases, with no skips.
- Changed R files parse and lint clean; `git diff --check` passes.
- `scripts/check-local.sh`: 0 errors, 0 warnings and one reconciled note caused by its test phase creating the ignored top-level `Rplots.pdf` before its build phase. The generated file was removed after the run.
- `scripts/check-cran.sh`: 0 errors, 0 warnings and the existing new-submission/remote-URL note (two Stack Overflow links returned HTTP 403); source, tests, vignettes, PDF manual and HTML manual pass.
- Online and offline workflow-state checks pass against `antoniojbt/episcout@master`.
- Representative geometry, numeric, categorical and missing-theme maps were visually inspected. A report containing a failed declared pair showed stable `incomplete_pairs` inventory rows with no files, and an empty report showed `no_rows` plus “No maps were created.” Repeated SVG renders were byte-identical, manifest MD5 values matched the files and HTML embedded only created map paths.
- Generated roxygen, all affected vignettes, both report paths and the replacement database walkthrough build successfully.

Draft implementation PR #246 is stacked on the contributor branch that contains planning PR #244; because GitHub cannot target a fork-only base branch in an upstream cross-fork PR, #246 targets `master`, declares the dependency and will shed the duplicate planning commits when #244 merges. At package head `4af3ffc`, macOS, Ubuntu, PostgreSQL integration, coverage, both Codecov gates and CodeFactor are green. The pull request is mergeable/CLEAN and has no reviews or review threads. The first hosted coverage run exposed a narrow fail-closed intake-test gap; commit `4af3ffc` added the missing reconciliation, publication, validation and stage-failure cases and made both coverage gates green.

## Closeout Review

Planning PR #244 merged to canonical `master` as `2d2237b2ee9e8c8181f9ea97fac5a0f34da5fb11`; implementation PR #246 then merged as `825215ea0eb2c7aab79768d6174315f1708bec09`. All seven required checks passed at the final implementation head `f4d4972`, and #246 had no reviews or review threads. Issue #243 closed automatically. Before this closeout, the online workflow audit correctly identified the closed issue attached to an unfinished active spec; this closeout moves the record under `future/specs/done/`, completes the manifest and records the terminal reason. Local and fork `master` synchronization follows the closeout merge. No concrete successor is authorised because the remaining roadmap work is separately tracked or deferred.
