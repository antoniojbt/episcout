# Review Notes

Spec ID: `022-postgresql-eda-row-count-reuse`
Status: Implemented; acceptance pending

## Findings

Pre-code review confirms one narrow redundant query. The existing `n_total` is checked and belongs to the same transaction snapshot as every categorical frequency query. No correctness or privacy failure was observed in the baseline.

## Open Questions

None. Wider aggregate consolidation remains a separate, unapproved performance feature.

## Closeout Notes

The implementation changes one internal signature, removes one helper-level call and passes the existing checked denominator at the only categorical/binary call site. The public formals and all output schemas remain unchanged. The new mocked test fails if the helper attempts any row-count query; the live exported test proves a two-variable categorical/binary fixture records one truthful `row_count` timing entry instead of the baseline three. Existing exact frequency, proportion, zero-row, reconciliation, plot, checksum and overwrite assertions remain green.

Focused unit and PostgreSQL 18.4 parity tests passed. The one-million-row gated benchmark, catalogue and identity-universe suites also passed. The combined PostgreSQL 18.4 run reproducibly retains two unrelated `sec-pseudonymise-postgres` registry-metadata errors on authoritative `master`; this branch does not touch registry code, and the required GitHub integration job currently uses PostgreSQL 17. Package lint is clean, the complete local package check passed with `0/0/0`, and `git diff --check` passed. Pull-request CI and owner acceptance remain open.
