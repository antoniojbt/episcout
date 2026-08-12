# Software Design

Spec ID: `042`
Status: Active

## Scope

Change the package version from `0.3.0` to `0.4.0`, promote the verified development notes to the `0.4.0` NEWS section, and record candidate and release evidence. Build the source package with the repository wrapper, inspect its contents, install it into a temporary library, and publish only the verified canonical commit as GitHub tag and release.

## Public API

No new API is introduced by this release slice. The release packages additive interfaces already merged to canonical `master`; semantic version `0.4.0` signals the additive, potentially compatibility-relevant change set since `0.3.0`.

## Inputs And Outputs

Inputs are canonical source at the release-preparation merge commit and the repository verification commands. Outputs are the checked source tarball, temporary installed-package smoke evidence, tag `0.4.0`, and GitHub release notes derived from `NEWS.md`.

## Data Flow

1. Update version and release notes on the scoped branch.
2. Regenerate package metadata and run local and CRAN-oriented checks.
3. Inspect the exact tarball and a clean temporary installation.
4. Merge the verified preparation PR and re-check canonical `master`.
5. Tag the verified canonical commit and publish the GitHub release.

## Edge Cases

- A check may fail because of a transient external dependency; confirm the failed step before retrying.
- Any package, artifact, privacy or documentation defect blocks tagging until corrected and reverified.
- CRAN-only findings do not authorize a CRAN upload and must be reported rather than concealed.

## Errors And Warnings

No `R CMD check` ERROR or WARNING is acceptable for this GitHub release candidate. Environment-only or CRAN incoming NOTES are recorded precisely and do not imply CRAN readiness.

## Dependencies

No dependency changes are planned.
