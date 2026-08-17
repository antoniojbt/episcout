# Review Notes

Spec ID: 050
Status: Active

## Recovery Findings

The clarified dispatcher attempt passed behaviour audit and wrote a recoverable implementation, then stopped at blocked verification without a commit or PR. Recovery review found three blocking gaps: this specification was absent; unavailable pair summaries used NA_integer_ instead of required 0L; and adjacent domain union was assembled in R without the required bounded PostgreSQL preflight. Tests also lacked literal 51-level evidence and a deliberately complete four-cell truth fixture.

The issue contract was further bounded to at most 50 states in v1, limiting each complete matrix to 2,500 rows. This resolves the prior theoretical 46,340-state input that could fit an integer square but could not be safely materialised.

## Repair And Evidence

The unavailable-pair schema now uses 0L, adjacent domains receive a PostgreSQL UNION preflight limited to max_levels + 1, and v1 rejects max_levels above 50. Neutral tests now contain the four A/B cells, missing-from and missing-to exclusions, conflict precedence, entrants/exits, literal 51-state failures, an empty period, binary canonical states, concurrent snapshot behaviour and a real sanitised PostgreSQL failure followed by connection reuse.

Focused unit and disposable PostgreSQL 17 tests passed. Workflow-state and git diff checks passed. scripts/check-local.sh completed with zero errors, zero warnings and two known notes for clock verification and the existing docs directory. scripts/check-cran.sh completed with zero errors, zero warnings and two repository/publication notes. An independent read-only review found no remaining analytical, denominator, SQL/snapshot, privacy, API/schema or documentation blocker. Its two optional evidence suggestions do not change the accepted behaviour.

## Remaining Lifecycle

Draft PR-354 is the implementation review surface. Hosted checks, PR merge, canonical issue closure and closeout remain pending. Do not mark the specification completed, record a merge commit or move it under done until those gates pass.
