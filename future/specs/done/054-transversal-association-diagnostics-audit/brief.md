# Brief

Spec ID: `054-transversal-association-diagnostics-audit`
Status: Completed

## Problem

Oferta currently calculates PostgreSQL-native pairwise Spearman coefficients, categorical cross-tabs against a selected literal grouping field, Cramér's V from those aggregate counts and three overview plots. Some mechanics are plausibly reusable, but Episcout already owns canonical missingness, categorical denominators, PostgreSQL stratification and correlation/heatmap helpers. Moving the complete Oferta helper upstream would duplicate existing behaviour and incorrectly transfer SIAP selection and presentation policy into the package.

## Objective

Define the smallest reusable Episcout boundary for ordinary descriptive association diagnostics. Reuse existing contracts where they are sufficient, identify exact statistical or backend gaps where they are not, and leave project selection, labels, interpretation, styling and report composition downstream.

## Outcome

The audit finds one bounded implementation successor is justified:

1. add an explicit-pair descriptive Spearman profiler with data-frame and aggregate-only PostgreSQL backends;
2. add one canonical Cramér's V calculation from validated aggregate contingency counts;
3. add a finite, fail-not-truncate categorical-domain option to the existing stratified profiler so association work can reuse its cross-tabs safely.

No new cross-tab engine or plotting API is justified. Oferta's `PLZOCU=0` location query and every scientific or presentation decision remain downstream.

## Non-goals

- No package source, public API, test, generated documentation, dependency or Oferta changes in issue 358.
- No p-values, confidence intervals, multiplicity workflow, automatic interpretation, thresholds, anomaly labels or feature selection.
- No arbitrary SQL framework, multidimensional cube, dashboard or report framework.
- No SIAP catalogue meaning, Spanish labels, AEIS styling or project-specific grouping.
- No behavioural rewrite of released `epi_stats_corr()` or historical contingency helpers.
