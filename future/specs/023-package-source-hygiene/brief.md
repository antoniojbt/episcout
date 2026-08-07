# Brief

Spec ID: `023-package-source-hygiene`
Status: Implemented
Owner: repository-owner

## Problem

The documented local-check then source-build sequence can place generated
`tests/testthat/Rplots.pdf`, obsolete developer paths and an unused workbook in
the package archive.

## Goal

Keep generated and legacy development artifacts out of the exact source archive
without weakening tests or changing package behaviour.

## Non-goals

- Change any public R interface or analytical result.
- Remove visual-regression references or plotting tests.
- Perform CRAN URL or vignette-NOTE cleanup.

## Candidate Files

- `.Rbuildignore`
- `scripts/check-cran.sh`
- four legacy test files
- `vignettes/R_datasets.xlsx`

## Risks

- An overly broad ignore rule could hide intended fixtures.
- A non-portable archive guard could make release checks unreliable.
