# Software Design

Spec ID: `023-package-source-hygiene`
Status: Implemented

## Scope

Add one exact build-ignore rule for the generated plotting PDF, remove four
obsolete comments and the unreferenced workbook, and inspect the built archive
before `R CMD check --as-cran` begins.

## Public API

None. Package code and installed interfaces are unchanged.

## Data Flow

1. Existing package tests may generate `tests/testthat/Rplots.pdf`.
2. `R CMD build` excludes that exact path through `.Rbuildignore`.
3. `scripts/check-cran.sh` identifies the archive root and rejects the PDF,
   workbook or legacy absolute path before checking the archive.

## Errors And Warnings

The archive check exits non-zero with the exact forbidden artifact class. It
does not inspect or remove intended visual references.

## Dependencies

No package dependency changes. The guard uses Bash and existing `tar`/`grep`
commands already required by the release workflow.
