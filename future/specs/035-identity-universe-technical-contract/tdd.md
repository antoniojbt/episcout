# Test Design

Spec ID: `035-identity-universe-technical-contract`
Status: Active

## Independent Basis

`Issue-278` and completed `spec-034` define the required interface and technical authority boundary. The two-source synthetic fixture is independently hand-derived: `{A, B, B, C}` has 4 inputs, 3 distinct identifiers and duplicate excess 1; `{B, C, D}` has 3 inputs, 3 distinct identifiers and duplicate excess 0; their union is 4, single-source membership is 2, multi-source membership is 2, intersection is 2 and each directional coverage is `2 / 3`.

## Constructor Tests

- Build version-2 objects from the exact five-column metadata in both source orders and verify the exact stored schema, class, contract version, deterministic order and one SHA-256 fingerprint.
- Pass the exact legacy six-column metadata with confirmed, pending, blank and missing status values; verify one warning per call, identical version-2 output and no authority effect.
- Reject missing and arbitrary extra/value-bearing columns, fewer than two or duplicate relations, mixed namespaces, invalid identifiers, empty provenance, unsupported normalisation and empty regex.
- Reject saved version-1 and modified version-2 objects at the database validation boundary with regeneration guidance and without changing their fingerprints.
- Confirm print output is relation/value-free and contains no confirmation or mandatory-audit wording.

## Aggregate And Status Tests

- Verify every existing issue code/count remains while error cases use severity/status `error` and warnings remain `warning`.
- Verify audit status is `audit_complete` and writes are false both with no errors and with error-severity issues.
- Verify materialisation status is `not_written` for error-severity findings, an existing destination and lock timeout, and `complete` only after commit.
- Preserve the independently derived fixture results, empty-source denominator behaviour and aggregate/value-free result assertions.

## PostgreSQL Transaction And Privilege Tests

- Preserve ordinary source relation, source column, compatible family, deterministic collation, regex, schema existence, source/destination distinction and caller-transaction errors.
- Preserve read-only audit, statement timeout, advisory lock transfer/cleanup, uniqueness/check constraints, source non-mutation and destination refusal coverage.
- Grant explicit `PUBLIC` schema privileges in a disposable output schema, materialise successfully and compare the schema grant state before/after.
- Force a non-permission failure immediately after the destination insert executes and verify the complete destination rolls back behind a fixed value-free package error.
- Keep the opt-in PostgreSQL test limited to generated synthetic values and disposable schemas.

## Documentation And Verification

- Regenerate both Rd files from roxygen and compare formals, source columns, status/severity schemas and transaction/privilege text with the implementation.
- Render and inspect the longitudinal vignette after changing only its bounded identity-universe section.
- Run focused offline tests, the approved disposable PostgreSQL test, package lint, `scripts/check-local.sh`, `scripts/check-workflow-state.sh` and `git diff --check`.
