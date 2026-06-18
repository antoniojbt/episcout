# Brief

Spec ID: `004-senior-review-followups`  
Status: Implemented  
Owner: TBD  

## Problem

The senior package review in
`future/reviews/2026-06-15-senior-r-package-review.md` identified focused
maintenance risks before the development branch can be treated as stable:

- EDA missingness ignores data-dictionary `missing_codes`.
- EDA summary tests duplicate implementation logic.
- Numeric EDA summaries are unstable for all-missing variables.
- Categorical summary percentage denominators are not explicit.
- Some optional dependency failures are indirect.
- Tidy-eval re-exports broaden the public API, but changing them may be
  compatibility-breaking.

## Goal

Lock and implement small, reviewable follow-ups that improve EDA correctness,
test quality and user-facing error clarity without broad package redesign.

## Non-goals

- Removing or deprecating tidy-eval re-exports.
- Implementing large-data backends.
- Redesigning the full EDA schema API.
- Adding new package dependencies.
- Rewriting unrelated helper functions.

## Candidate Files

- `R/eda_missing.R`
- `R/eda_summaries.R`
- `R/eda_schema.R`
- `R/epi_plot_parallel.R`
- `R/eda_report.R`
- `tests/testthat/test-eda_missing-fixtures.R`
- `tests/testthat/test-eda_summaries-fixtures.R`
- `tests/testthat/test-eda_schema-fixtures.R`
- `tests/testthat/test-plot-parallel.R`
- `README.md`
- `vignettes/specification-first-eda.Rmd`

## Risks

- Applying `missing_codes` changes summary and missingness output for data sets
  that already use sentinel values.
- Adding `p_observed` changes categorical summary output shape.
- Tightening dependency guards can change exact error messages.
- Public API cleanup is intentionally deferred because removing accidental
  exports may break existing users.
