# Test Design

Spec ID: `040-approved-cleaning-rules-and-processed-outputs`

Status: Active

## Independent Expectations

Use hand-authored neutral fixtures whose expected transition masks and counts are written literally in tests. Do not generate expected results with production rule helpers. For example, the numeric vector `-1, 0, 5, 11, 999, NA` under bounds 0 through 10 and missing code 999 has three observed-to-missing transitions, one source missing value and four destination missing values. A categorical vector `A, B, X, M, NA` with allowed values A/B and missing code M has two transitions, one source missing and three destination missing values.

## Rule Boundary And Validation

- Assert exact public formals, classes, approved-rule columns and normalisation.
- Reject the issue-271 proposal object and its proposal table directly, pending state, missing/extra/reordered fields, zero rules, duplicate/malformed keys, malformed approvals and unsupported types.
- Reject contradictory bounds, type-inapplicable fields, overlapping allowed/missing values, malformed delimiters, non-finite or non-integral numeric fields and rule rows with no operation.
- Use canary names, values, keys, paths and relation identities and confirm validation errors and print/structure output contain none of them.

## In-Memory Behaviour

- Check numeric/integer bounds, allowed categorical/binary values and approved missing codes against literal processed vectors and typed missing values.
- Check factors retain class and levels; unchanged columns, row names, row count, row order and column order remain exact.
- Save and compare the source, approved rules and key map before and after success and failure, including a data.table input when available.
- Verify zero-row and all-missing inputs, existing `NA`/`NaN`, absent variables, unsupported storage and complete prevalidation with no partial transformation.
- Assert literal per-variable and summary missing counts, transition counts, dimensions and reconciliation flags; repeat with reordered semicolon sets and rules to check canonical hashing.

## File Behaviour

- Publish explicit CSV and RDS files, read them back and compare dimensions; compare RDS exactly with returned data.
- Reject implicit format, RData/Parquet, existing files, non-scalar CSV columns and invalid parent/output conditions.
- Inject staged-write and post-publication reconciliation failures and assert neither destination nor staging artefact remains.
- Exercise zero-row publication.

## PostgreSQL Behaviour

- Inspect constructed SQL under mocked scalar database helpers to confirm quoted column projection, bound parameters, server-side `CASE` transformation and absence of embedded rule values.
- In a disposable PostgreSQL 18 database, apply equivalent supported data-frame and PostgreSQL rules, order results by a neutral retained sequence only for comparison, and assert identical processed values and transition counts.
- Confirm source rows remain exact, the new destination exists only after success, destination collisions are refused and the connection is idle.
- Exercise zero rows and all-missing values.
- Inject failure after destination creation but before reconciliation/commit and confirm the complete transaction rolls back with no destination table.
- Search captured errors, result objects and display output for source/destination/value canaries.

## Broader Checks

Run the focused test file without and with the disposable PostgreSQL gate, style the new R source, regenerate roxygen output, lint the package, run workflow-state validation and then run `scripts/check-local.sh`. Apply software-verification, truth-and-semantics, analysis-and-statistics and copy-edit checklists before handoff.
