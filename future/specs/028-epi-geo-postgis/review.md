# Review Notes

Spec ID: `028-epi-geo-postgis`
Status: Active

## Findings

No unresolved implementation blocker after focused self-review.

- Grouped observed type, SRID and dimension queries are capped at two rows because the contract only needs to distinguish zero, one and mixed values; this prevents high-cardinality aggregate materialisation.
- The source records both a catalogue fingerprint and a non-connection source signature, retains the original connection reference and revalidates before and inside each transaction. Modified sources, relation replacement and catalogue drift fail before aggregate or feature queries.
- Description query structure selects only grouped/scalar aggregates. Feature WKB appears only in the explicit collection query after a same-snapshot count passes the caller's bound.
- Null selected geometry fails after bounded fetch rather than being silently converted to empty geometry; this preserves the Phase-A `sf` contract and source missingness semantics.
- CI uses the upstream PostGIS project's recommended PostgreSQL 17/PostGIS 3.5 image and retains the repository's PostgreSQL 17 support floor.

## Required Review Focus

- Prove aggregate description cannot materialise feature geometry or ordinary attributes.
- Inspect every SQL statement for exact identifier quoting, bound values and bounded fetches.
- Exercise transaction cleanup and connection reuse after each handled failure class.
- Reconcile returned `sf` objects with the Phase-A contract and exact selection count.
- Inspect conditions, source methods, snapshots and documentation for connection or location leakage.

## Closeout Notes

- Pull request and checks: pending.
- Stacked dependency: closeout PR #236 must merge before this implementation can merge cleanly.
- Successor: Phase-C issue #237 exists and remains unstarted until Phase-B merge and closeout.
