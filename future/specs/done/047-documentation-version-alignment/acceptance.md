# Acceptance

Spec ID: `047-documentation-version-alignment`
Status: Completed

- [x] SDD and TDD define the documentation-only contract before implementation.
- [x] `DESCRIPTION` identifies development version `0.4.1.9000` without changing tag `0.4.1`.
- [x] NEWS and GitHub release-0.4.1 notes accurately distinguish released and post-release work and explain the tagged version mismatch.
- [x] README, roxygen source, generated help, all vignettes, templates and installed walkthroughs agree with the current interface.
- [x] The starter dictionary has the exact 16-field schema and loads through `epi_eda_spec()` in an executable test.
- [x] Longitudinal stability language requires reuse of the same persisted registry and does not claim stability across separate registries.
- [x] Focused tests, all vignette renders, the installed PostgreSQL walkthrough, local checks, CRAN checks and workflow-state checks pass or have an explicit evidence-based exception.
- [x] Hosted CI and review feedback are resolved.
- [x] The implementation PR is merged to canonical `master` and its exact merge commit is recorded.
- [x] Issue-327, lifecycle records and existing release notes are reconciled through post-merge closeout.
- [x] No public interface, data-processing behaviour, dependency or governance gate changes.
- [x] The terminal reason is recorded, the manifest is `completed` and this specification is moved under `future/specs/done/`.
