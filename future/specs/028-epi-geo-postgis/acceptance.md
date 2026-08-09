# Acceptance

Spec ID: `028-epi-geo-postgis`
Status: Review

- [x] Issue #233, completed spec 026 and completed spec 027 define the required outcome.
- [x] SDD and TDD were completed before package-code changes.
- [x] Owner authorised stacked activation after green closeout PR #236.
- [x] Executable unit, structural and disposable PostGIS tests pass.
- [x] Generated documentation and the observed primer workflow agree with the implementation.
- [x] Package lint, local check and CRAN-oriented check pass with the recorded external NOTE.
- [ ] Draft PR #238 links and closes issue #233, records its dependency on #236 and reaches green CI.
- [x] Phase-C successor issue #237 is recorded and remains gated on Phase-B merge and closeout.
- [ ] Post-merge closeout records the canonical merge and moves this spec to `done/`.

## Activation Evidence

- `scripts/check-workflow-state.sh` matched canonical GitHub before activation.
- The stacked base is green closeout PR #236 at `664a0a3`; canonical `upstream/master` contains Phase-A implementation PR #234 at `b37b391`.
- R 4.5.3, sf 1.1.2, RPostgres 1.4.10, DBI 1.3.0 and PostgreSQL 18.4 binaries are available locally. A PostGIS extension was not initially installed; mandatory live evidence remains outstanding.

## Implementation Evidence

- Installed PostGIS 3.6.1 into the disposable repository environment, initialised a user-owned PostgreSQL 18.4 cluster on port 55432 and enabled PostGIS only in its disposable `synthetic_records` database.
- Focused offline Phase-A/Phase-B tests and the complete live Phase-B unit/integration suite pass. Live expectations reconcile hand-authored points, edge-inclusive bbox selection, empty/null/invalid geometry, geography, approved relation kinds, ambiguous columns, mixed runtime metadata, catalogue drift, modified source objects, transaction ownership, disconnects and connection reuse.
- Structural tests inspect the query inventory: description returns at most two grouped aggregate rows per metadata dimension, one validity/count row and one extent row, and contains no feature geometry, WKT/WKB or ordinary attribute selection. Collection binds bbox and limit values, quotes exact identifiers and fetches at most `max_features + 1` rows.
- Package-loaded lint reports no findings. `scripts/check-local.sh` completed at 0 errors, 0 warnings and 0 notes, including vignette rebuild. `scripts/check-cran.sh` completed with one existing incoming-feasibility NOTE for new-submission/vignette-index state and two external Stack Overflow 403 responses; package checks, manual and vignette rebuild passed.
- The full live database filter passes all geospatial, EDA and identity-universe cases. Two pre-existing longitudinal registry cases fail only on the local PostgreSQL 18.4 metadata shape; the supported PostgreSQL 17 CI service remains authoritative and is unchanged in version.
