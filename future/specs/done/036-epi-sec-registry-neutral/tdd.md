# Test Design

Spec ID: `036-epi-sec-registry-neutral`
Status: Review

## Independent Basis

`issue-284` and completed `spec-034` define the required technical authority boundary. Registry compatibility is independently fixed by the existing version-1 six-table schema, exact column/type/nullability signatures, deterministic `C` collations, primary/foreign/check constraints, defaults, one metadata row and immutable prefix/byte settings. PostgreSQL catalogue and grant state observed directly before and after calls provides independent evidence that package SQL does not inspect or mutate privileges.

## Offline Registry Tests

- Verify registry results expose exactly the seven neutral components, use `incompatible` for structural audit failure and print no `PUBLIC`, restriction, approval or authorisation language.
- Mock registry catalogue inspection and prove the query requests only relation names/kinds, compatible foreign ownership is not classified, and object states are only structural.
- Mock registry creation and prove the exact six tables and metadata insert remain while captured SQL contains no privilege query, `GRANT` or `REVOKE`.
- Preserve immutable token-setting failures and structural object/version classification.

## PostgreSQL Integration Tests

- Grant explicit `PUBLIC` schema/table privileges before registry initialisation, capture every query/statement issued by registry initialisation and pseudonymisation, and prove no privilege predicate, `GRANT` or `REVOKE` appears.
- Compare schema/table grant state before and after registry creation, crosswalk inspection and output creation; default output grants remain configured server outcomes.
- Change registry table ownership to a disposable foreign role, grant sufficient PostgreSQL access and verify the compatible registry remains `ready` and the technical pseudonymisation audit can proceed.
- Revoke one required permission from a disposable connected role and verify a fixed value-free package failure without native permission text, identifiers or credentials.
- Preserve registry version/structure/settings, source non-mutation, stable mappings, replacement ownership/dependency checks, advisory-lock cleanup, rollback and post-write reconciliation coverage.
- Force a non-permission failure after output creation and verify registry/output changes roll back behind the fixed error.

## Documentation And Verification

- Regenerate the affected Rd files from roxygen and compare formals, result fields/statuses, physical registry version, transaction behaviour and privilege-neutral text with implementation.
- Render and inspect only the directly affected registry portion of the longitudinal vignette.
- Run focused offline/live PostgreSQL tests, package lint, `scripts/check-local.sh`, `scripts/check-workflow-state.sh` and `git diff --check`.
