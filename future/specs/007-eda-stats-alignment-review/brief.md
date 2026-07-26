# Brief

Spec ID: `007-eda-stats-alignment-review`
Status: Completed
Owner: Antonio Berlanga-Taylor

## Problem

`epi_stats_*` is the active main statistics layer, but `epi_eda_profile_summaries()` independently implements a smaller numeric and categorical contract. This duplicates behaviour, omits text, date and datetime summaries required by the archived EDA MVP, and leaves no reviewed boundary between the main statistics API and the specification-first workflow.

An earlier triage incorrectly described `epi_stats_*` as legacy and closed the alignment idea as speculative. Repository architecture, README guidance and current exports instead establish these functions as actively supported lower-level helpers that the EDA layer should reuse where suitable.

## Goal

Review the complete exported `epi_stats_*` namespace and the EDA summary flow, document current contracts and confirmed gaps, and recommend one target architecture plus an ordered, separately approved implementation programme.

## Success Criteria

- Every exported `epi_stats_*` function and related compatibility helper is classified with its contract, evidence, risks and recommended disposition.
- Every EDA specification type has a proposed summary contract or an explicit reason for deliberate exclusion.
- Missingness, sentinel codes, edge cases, dependencies and downstream consumers are mapped.
- Breaking recommendations include migration and deprecation expectations.
- Baseline tests and package check are recorded.
- No package code, generated documentation, fixtures or executable tests are changed.

## Non-goals

- Refactoring R package code.
- Changing public APIs, output files or report templates.
- Adding executable tests or fixtures.
- Creating spec 008 before human acceptance of this review.
- Folding correlation, contingency or outcome analysis directly into univariate EDA summaries.

## Primary Evidence

- `R/epi_stats_*.R`, `R/eda_summaries.R`, `R/eda_missing.R` and `R/run_eda.R`.
- `NAMESPACE`, `DESCRIPTION`, README and generated function documentation.
- Existing statistics and EDA tests, including blood-storage and penguins fixtures.
- Archived EDA architecture and MVP requirements.

## Risks

- Treating current output shapes as immutable could preserve accidental inconsistencies.
- Redesigning all statistics functions in one implementation change would be difficult to review and unsafe for users.
- Reusing broad public outputs directly inside EDA could couple the workflow to presentation-oriented or inferential fields.
- Renaming or removing compatibility aliases without staged deprecation could break existing analysis scripts.
