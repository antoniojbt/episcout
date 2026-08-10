# Brief

Spec ID: `033-categorical-denominator-presentation`
Status: Completed
Owner: repository-owner
Tracking issue: #253

## Problem

Canonical and stratified categorical summaries already contain the required counts and population totals, but their presentation consumers choose or omit denominators independently. Table 1 applies a compatibility rule internally, compact frequency data carry counts without a numeric percentage contract, and the data-frame, intake and PostgreSQL delivery reports do not present one reconciled aggregate view.

## Goal

Add one public aggregate-only categorical display calculation with explicit numerator, denominator, proportion, basis, population/group size and missing-level treatment. Make Table 1, compact categorical plot companions and all existing EDA report families consume those fields while preserving current default analytical and plotting behaviour.

## Non-goals

- Recalculating canonical or stratified counts from raw observations.
- Changing the six canonical or eight stratified calculation-component schemas.
- Inferring study populations, missing/not-applicable meaning, category semantics or a percentage basis.
- Adding p-values, effect estimates, survey weights, multi-way strata, automatic annotations, disclosure policy or row-level artifacts.
- Adding PostgreSQL queries, coordinate/theme collection, database report connections or a second statistics engine.
- Changing the default flat PostgreSQL bundle or the five-column manifest schema.

## Candidate Files

- New `R/eda_categorical_display.R` and generated help/export records.
- Table 1, plot preparation, runner, intake and both report implementations/templates.
- Focused calculation, compatibility, report, bundle and live PostgreSQL tests.
- README, NEWS, the EDA vignette and installed database walkthrough.

## Risks

- Ambiguous row/column terminology could produce technically valid but misleading percentages.
- Counting Overall in row denominators would double-count the analysis population.
- Adding a missing category to plots would change current visual behaviour.
- Recomputing PostgreSQL companions from observations would violate the snapshot and aggregate-only boundaries.
- Strictly requiring the new companion schema could make valid delivery bundles from the immediately preceding contract unrenderable.

## Successor Or Terminal Outcome

Issue #253 completes the bounded denominator-presentation slice. No successor is needed because remaining roadmap work is already separately tracked or deferred; canonical merge verification records this as the terminal reason.
