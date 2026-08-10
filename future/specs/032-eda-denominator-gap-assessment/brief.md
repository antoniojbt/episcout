# Brief

Spec ID: `032-eda-denominator-gap-assessment`
Status: Review

## Problem

Issue #248 asks which generic EDA count, denominator and percentage capabilities remain after the completed stratified-summary work in #183 and canonical delivery work in #245. The repository already has strong calculation components, so treating the tracker as a request for a second summary engine would duplicate behaviour and increase reconciliation risk.

## Objective

Audit the current public calculation, Table 1, compact-plot and report contracts; distinguish completed foundations from real presentation gaps; and create only the smallest actionable implementation successor. This contribution changes planning records only.

## Outcome

The assessment retains existing canonical and stratified calculation schemas. One successor, issue #253, will add a shared aggregate-only categorical display contract with explicit numerator, denominator, proportion and basis fields, then make current Table 1, frequency-plot companion data and report paths consume that contract.

## Non-goals

- No package source, public API, test, generated documentation, dependency or output changes.
- No inferred population, filter, clinical meaning, not-applicable category or percentage basis.
- No p-values, effect estimates, survey weighting, multi-way stratification, row-level output or disclosure/suppression policy.
- No second successor for chart styling alone; readable aggregate companion data satisfy the figure requirement when direct annotations would add clutter.
