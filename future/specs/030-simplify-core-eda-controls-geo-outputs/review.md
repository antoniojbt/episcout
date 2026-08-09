# Review

Spec ID: `030-simplify-core-eda-controls-geo-outputs`
Status: Active

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

Pending. Record focused, live-database, render, lint, local, CRAN, CI and review-thread evidence here before handoff.

## Closeout Review

Pending canonical merge verification and repository reconciliation.
