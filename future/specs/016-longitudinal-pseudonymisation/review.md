# Review Notes

Spec ID: `016-longitudinal-pseudonymisation`
Status: Active

## Planning Findings

- The released single-vector helper is not a persistent identity registry and must remain behaviourally compatible. Multi-table database pseudonymisation is therefore an additive interface rather than an overload of `epi_sec_pseudonym()`.
- A portable crosswalk containing identifier values would defeat the principal privacy boundary. The linkage specification stores only confirmed metadata for a caller-controlled restricted PostgreSQL table.
- The enrolment relation may contain repeated longitudinal rows. Exactly one table is allowed to enrol entities, but it must not be assumed to contain one row per entity.
- Exact identity resolution and duplicate records are different semantic questions. Repeated identifiers are valid; only reviewed record keys or exact final projections may establish row conflicts/duplicates.
- Text identifiers require exact preservation of case, whitespace and leading zeros, while UUID equality belongs to PostgreSQL. Moving identifiers into R for generic normalization would change semantics and expand disclosure risk.
- Audit and apply cannot safely share a caller-managed transaction or rely on a pre-transaction audit snapshot. Apply must own one repeatable-read transaction and redo all checks after it begins.
- Safe replacement requires exact destination, ownership, relation-kind and dependency validation and a non-cascading operation. General schema cleanup or broad drop authority is not permitted.
- Native PostgreSQL details can include row values. Public issues and errors must use known value-free codes, aggregate counts and recommended actions rather than forwarding database messages.
- Pseudonymisation is not anonymisation or disclosure control. The result and guide must retain restricted-data language even when no direct identifier appears in outputs.

## Checklist Routing

- `checklists/software-verification.md` applies to every public contract, input validation, database invariant, transaction boundary, lock, replacement path, stable schema, non-mutation check, regression test and acceptance command.
- `checklists/truth-and-semantics.md` applies to identity equivalence, namespace families, reviewed authority, missing/blank identity handling, record keys, duplicates, row reconciliation, provenance and privacy/completion claims.
- `checklists/analysis-and-statistics.md` applies narrowly to independently verified counts, denominators/reconciliation and output-dictionary handoff. This specification adds no statistical estimator, model or epidemiological interpretation.
- `checklists/copy-edit.md` applies to README, NEWS, roxygen help, status/issue wording, the canonical guide and all privacy/recovery messages.
- `checklists/render-and-release.md` applies to vignette rendering, visual inspection, installed-vignette discovery and package/check artifacts. It does not authorise a release or tag.
- `checklists/figures.md` does not apply unless implementation introduces a figure; no figure is planned.

## Baseline Evidence

The baseline was recorded on 2026-08-04 before package-code changes on `feature/longitudinal-pseudonymisation`:

- Package-loaded lint completed with no findings.
- The full suite recorded 1,573 passing expectations and one existing failure at `tests/testthat/test-eda-prepare.R:223`, where macOS resolves a daylight-saving-time-ambiguous local datetime differently.
- `scripts/check-local.sh` failed from that same existing test.
- `scripts/check-cran.sh` recorded the same existing failure plus inherited notes for new-submission status, the vignette index, documented URLs returning HTTP 403 and HTML tidy output.

These findings are inherited baseline evidence, not permission to weaken a regression test or accept new failures. Final review must distinguish inherited, externally caused, resolved and spec-016-created results.

## Implementation Checkpoints

1. Inspect current security, dictionary, catalogue, database inventory, transaction helper, documentation and CI contracts before choosing exact package files.
2. Add failing unit tests for linkage metadata, validation, redaction and output dictionary behaviour before implementation.
3. Add registry schema/privilege/constraint tests and implement audit/apply initialization.
4. Add exact identity and longitudinal-duplicate integration tests before implementing the main workflow.
5. Add transaction, concurrency, replacement, rollback and source-invariant tests.
6. Add friendly documentation and execute the full synthetic walkthrough.
7. Run independent PostgreSQL, leakage and documentation reviews; reconcile findings with the primary implementation.
8. Complete genericity, rendered-documentation and package acceptance checks before commit/push/draft PR.

Reconcile `future/TODOs.md`, `future/changelog.md`, this review and the manifest at status changes and material checkpoints.

## Independent Review Plan

Use sub-agents only for bounded independent work explicitly authorised by the approved plan:

- PostgreSQL integrity/concurrency review: constraints, namespace identity storage, privileges, repeatable-read snapshot, advisory locks, destination replacement and rollback.
- Identifier-leakage review: default/sensitive R objects, conditions, printed output, SQL text, manifests, persisted registry audit, output relations and documentation.
- Documentation usability review: one-step discoverability, DBA prerequisites, initial audit, blocker recovery, duplicate decision table, output handoff and privacy language.

The primary implementer retains integration ownership, resolves conflicting findings and performs final acceptance review.

## Open Questions

None currently. Stop for owner review if repository inspection shows that fulfilling the contract requires a new persistent backend, schema creation, role grants, identifier values in portable files, fuzzy matching, composite identity columns, destructive replacement beyond exact declared tables, a new statistical/derivation rule, or a privacy claim broader than this specification.

## Verification Evidence

Verification was completed on 2026-08-04 with the repository R wrapper and a disposable PostgreSQL 17 instance:

- Focused linkage, pseudonymisation and dictionary tests passed. The live PostgreSQL suite completed 119 expectations without failures or warnings and exercised registry structure and privileges, stable cross-table tokens, exact text/integral/UUID identity, reviewed crosswalks, longitudinal duplicates, lock timeout, rollback, destination safety, source invariants, redaction and EDA dictionary/catalogue handoff.
- Package-loaded `lintr::lint_package()` completed with no findings. `git diff --check` completed cleanly.
- `scripts/check-local.sh` passed code, documentation, examples and vignette checks with 0 warnings and 0 notes from `R CMD check`; 1,628 expectations passed. It stopped only at the inherited macOS daylight-saving ambiguity expectation at `tests/testthat/test-eda-prepare.R:223`; the existing multicore test also emitted its baseline one-core warning.
- `scripts/check-cran.sh` built the source package and manuals and likewise recorded 1,628 passing expectations before the same inherited daylight-saving failure. Its two notes are inherited: CRAN incoming/new-submission, missing prebuilt vignette index and two documented Stack Overflow URLs returning HTTP 403 are grouped in the incoming note; the second note reports that the installed HTML Tidy is too old for validation.
- The new vignette rendered successfully with all 15 numbered sections and a visible table of contents. A source package was built and installed into a disposable library, where `vignette(package = "episcout")` listed `longitudinal-pseudonymisation`. The in-app browser was unavailable, so the primary review inspected rendered structure rather than claiming interactive browser inspection; the independent documentation reviewer inspected the rendered guide and found no remaining usability or truth finding.
- Runtime-generated schemas and opaque identifiers were used throughout live tests, and cleanup targets were validated. Source relation content and metadata invariants, output reconciliation, default result/condition redaction and output dictionary/catalogue validation are asserted by the live suite.
- A targeted content audit over the new implementation, tests, vignette, specification and CI additions found no credential, connection string, developer path, observed identifier or real project-specific entity. Existing repository names and historical README/NEWS material were not attributed to this change. The unrelated untracked `future/scratch/episcout_postgresql_backend_plan_2.md` was explicitly excluded.
- The PostgreSQL integrity/concurrency reviewer found no unresolved release blocker after registry, lock, dependency and governance corrections. The identifier-leakage reviewer found no unresolved privacy blocker after default/sensitive object, condition, ACL and runtime-ID checks. The documentation reviewer found no remaining documentation finding. All three reviews were independent of final integration ownership.

Coverage was not measured. The live suite establishes the required safety boundaries but does not exhaustively fault-inject every PostgreSQL write statement or prove behaviour under every server, driver, backup or administrator logging configuration; those infrastructure controls remain explicitly outside episcout's claims.

## Closeout Notes

Implementation and primary verification are complete on `feature/longitudinal-pseudonymisation` and published for owner review in draft PR #189, `[Feature] Add longitudinal pseudonymisation workflow`. The specification remains active until that review is accepted. No release or tag was created, and episcout did not create schemas, grant roles, manage backups or configure server logging.
