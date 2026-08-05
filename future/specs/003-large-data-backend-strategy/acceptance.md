# Acceptance

Spec ID: `003-large-data-backend-strategy`
Status: Draft; revision required before activation

- [ ] The revised brief, SDD, TDD, acceptance contract and manifest receive review before package-code changes.
- [ ] Existing data-frame/tibble interfaces and canonical results remain the baseline.
- [ ] PostgreSQL is the only first backend and dependency placement is explicit.
- [ ] Every specification type, missingness rule, statistical definition and identifier-QA result has a precise PostgreSQL parity contract.
- [ ] Plot-data preparation, shared rendering, bounded collection and high-cardinality display policy are explicit.
- [ ] The owned output bundle reuses staged writes and manifest-validated replacement and contains no source rows or sensitive connection material.
- [ ] Independent fixtures, privacy checks, live PostgreSQL checks and external performance evidence are defined before implementation.
- [ ] Arrow, DuckDB, data.table, generic DBI dispatch and all other stated exclusions remain outside the first implementation.
